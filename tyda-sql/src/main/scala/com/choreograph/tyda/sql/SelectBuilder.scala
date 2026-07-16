package com.choreograph.tyda.sql

import scala.NamedTuple.AnyNamedTuple
import scala.collection.mutable
import scala.deriving.Mirror
import scala.reflect.ClassTag

import shapeless3.deriving.Complete
import shapeless3.deriving.K0
import shapeless3.deriving.Labelling
import shapeless3.deriving.internals.ErasedProductInstances1
import shapeless3.deriving.internals.ErasedProductInstancesN

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Dataset.codec
import com.choreograph.tyda.ExplodeExpr
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Field
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.rewrite.IsNone
import com.choreograph.tyda.rewrite.Nullable
import com.choreograph.tyda.rewrite.StructFields
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.sql.Result.sequence
import com.choreograph.tyda.sql.ast.From
import com.choreograph.tyda.sql.ast.Query
import com.choreograph.tyda.sql.ast.SqlExpr
import com.choreograph.tyda.unreachable

/** Helper class for building a Query from a Dataset.
  *
  * This builder aims to combine as many operations as possible in seach SELECT.
  * In order to do this safely the internal state uses the type Expr api a far
  * as possible and only lowering to the untype SQL ast when no more operations
  * can be combined into that select.
  */
private final case class SelectBuilder[T, R](
    args: UnparserArgs,
    from: TypedFrom[T],
    select: CompiledExpr[T, R] | CompiledAggregateExpr[T, R],
    where: Option[CompiledExpr[T, Boolean]] = None,
    groupBy: Option[CompiledExpr[T, ?]] = None,
    having: Option[CompiledExpr[T, Boolean]] = None,
    distinct: Boolean = false,
    orderBy: Option[CompiledExpr[T, ?]] = None,
    limit: Option[Int] = None
) {
  import SelectBuilder.*

  def filter(predicate: CompiledExpr[R, Boolean]): SelectBuilder[T, R] =
    select match {
      case CompiledAggregateExpr(arg, expr) =>
        copy(having = Some(combinePredicates(having, predicate.compose(CompiledExpr(arg, expr)))))
      case compiled: CompiledExpr[T, R] =>
        copy(where = Some(combinePredicates(where, predicate.compose(compiled))))
    }

  def limit(n: Int): Result[SelectBuilder[R, R]] =
    /* We wrap the limit in a subquery to make sure the order of operations is preserved when it's combined
     * with other operations that can change the number of rows (e.g. filters, joins, aggregates). */
    copy(limit = Some(n)).toSubquery

  def orderBy[K](key: CompiledExpr[R, K]): Result[SelectBuilder[?, R]] = {
    val ordered = select match {
      // TODO: orderBy should be fine to combine with distinct. But it currently it leads to spark
      // not resolving fully qualified column names in the order by clause.
      case _ if distinct => toSubquery.flatMap(_.orderBy(key))
      case CompiledAggregateExpr(arg, expr) =>
        Right(copy(orderBy = Some(key.compose(CompiledExpr(arg, expr)))))
      case compiled: CompiledExpr[T, R] => Right(copy(orderBy = Some(key.compose(compiled))))
    }

    // TODO: We always force a subquery after order by for simplicity. But in the future we might
    // want to allow it to be combined with more operations.
    ordered.flatMap(_.toSubquery)
  }

  def select[R2](expr: CompiledExprOrExplode[R, R2]): Result[SelectBuilder[?, R2]] =
    if distinct || requiresSubqueryForSeqOps(expr) then toSubquery.flatMap(_.select(expr))
    else
      expr match {
        case compiled @ CompiledExpr(_, _) => Right(copy(select = andThen(select, compiled)))
        case explode @ CompiledExplodeExpr(_, _) => selectExplode(explode)
      }

  def selectN[R2 <: Tuple](
      exprs: Tuple.Map[R2, [X] =>> CompiledExprOrExplode[R, X]]
  ): Result[SelectBuilder[?, R2]] = {
    val hasExplode = tupleInstances(exprs).foldLeft0(false)([t] =>
      (acc, compiled) =>
        compiled match {
          case _: CompiledExplodeExpr[?, ?] => Complete(true)
          case compiled: CompiledExpr[?, ?] => acc
        }
    )
    if (distinct || requiresSubqueryForAnySeqOps(exprs)) toSubquery.flatMap(_.selectN(exprs))
    else if (hasExplode) selectNExplode(exprs)
    else {
      val compiledExprs = tupleInstances(exprs).mapK([t] =>
        (compiled: CompiledExprOrExplode[R, t]) =>
          compiled match {
            case _: CompiledExplodeExpr[?, ?] => unreachable("unreachable due to hasExplode check")
            case compiled: CompiledExpr[R, ?] => andThen(select, compiled)
          }
      )
      val hasAggregate = compiledExprs.foldLeft0(false)([t] =>
        (acc, compiled) =>
          compiled match {
            case _: CompiledAggregateExpr[?, ?] => Complete(true)
            case _: CompiledExpr[?, ?] => acc
          }
      )
      val newArg = ExprNode.Reference[T]()(using select.arg.codec)
      val newExpr =
        ExprNode.makeTuple[R2](compiledExprs.mapK([t] => c => c.expr.replace(c.arg, newArg)).toTuple)
      val newSelect =
        if hasAggregate then { CompiledAggregateExpr(newArg, newExpr) } else { CompiledExpr(newArg, newExpr) }
      Right(copy(select = newSelect))
    }
  }

  def aggregate[A](agg: CompiledAggregateExpr[R, A]): Result[SelectBuilder[?, Option[A]]] = {
    given Codec[A] = agg.expr.codec
    select match {
      case compiled: CompiledExpr[T, R] if !distinct =>
        val combinedAgg = agg.compose(compiled).andThen(CompiledExpr(Expr.some(_)))
        buildWithSelect(FinalSelect.Aggregate(combinedAgg)).map(makeSubquery(_, summon))
      case _ => toSubquery.flatMap(_.aggregate(agg))
    }
  }

  def aggregate[K, A](
      key: CompiledExpr[R, K],
      aggregate: CompiledAggregateExpr[R, A]
  ): Result[SelectBuilder[?, (K, A)]] =
    select match {
      case compiled @ CompiledExpr(_, _) if !distinct =>
        val newGroupBy = key.compose(compiled)
        val newSelect = aggregate.compose(compiled).combine(key.compose(compiled))
        if args.dialect.useSubqueryToAvoidStructInGroupBy && hasMakeProductAfterFlattening(newGroupBy) then
          given Codec[R] = key.arg.codec
          given Codec[K] = key.expr.codec
          selectN[(R, K)]((CompiledExpr[R, R](identity), key))
            .flatMap(_.toSubquery)
            .flatMap(_.aggregate(CompiledExpr(_._2), aggregate.compose(CompiledExpr(_._1))))
        else Right(copy(select = newSelect, groupBy = Some(newGroupBy)))
      case _ => toSubquery.flatMap(_.aggregate(key, aggregate))
    }

  def join[U, R2](
      rhs: SelectBuilder[U, R2],
      on: CompiledExpr2[R, R2, Boolean]
  ): Result[SelectBuilder[?, (R, R2)]] = {
    val selectLhs = select match {
      case compiled: CompiledExpr[T, R] if !distinct => compiled
      case _ => return toSubquery.flatMap(_.join(rhs, on))
    }
    val selectRhs = rhs.select match {
      // If the right hand side is already a join we force a subquery to maintain join order.
      // It might be possible to relax this condition in the future.
      case compiled: CompiledExpr[U, R2] if !rhs.distinct && !isJoin(rhs.from) => compiled
      case _ => return rhs.toSubquery.flatMap(join(_, on))
    }
    given Codec[T] = selectLhs.arg.codec
    given Codec[U] = selectRhs.arg.codec
    val extractLhs = CompiledExpr[(T, U), T](_._1)
    val extractRhs = CompiledExpr[(T, U), U](_._2)
    val newSelect = selectLhs.compose(extractLhs).combine(selectRhs.compose(extractRhs))
    val newWhere = combinePredicates(where.map(_.compose(extractLhs)), rhs.where.map(_.compose(extractRhs)))
    TypedFrom
      .join(from, rhs.from, on.compose(selectLhs, selectRhs), args)
      .map(newFrom => SelectBuilder(args, from = newFrom, select = newSelect, where = newWhere))
  }

  def leftJoin[U, R2](
      rhs: SelectBuilder[U, R2],
      on: CompiledExpr2[R, R2, Boolean]
  ): Result[SelectBuilder[?, (R, Option[R2])]] = {
    given Codec[R] = select.expr.codec
    given Codec[R2] = rhs.select.expr.codec
    val wrap = CompiledExpr[R2, Option[R2]](e => Expr.some(e))
    val unwrap = CompiledExpr[Option[R2], R2](e => Expr.knownNotNull(e))
    rhs
      .select(wrap)
      .flatMap(_.toSubquery)
      .flatMap(leftJoinNullable(_, on.compose(CompiledExpr[R, R](identity), unwrap)))
  }

  private def leftJoinNullable[R2 <: Option[?]](
      rhs: SelectBuilder[R2, R2],
      on: CompiledExpr2[R, R2, Boolean]
  ): Result[SelectBuilder[?, (R, R2)]] = {
    val selectLhs = select match {
      case compiled: CompiledExpr[T, R] if !distinct => compiled
      case _ => return toSubquery.flatMap(_.leftJoinNullable(rhs, on))
    }
    val selectRhs = rhs.select match {
      case compiled @ CompiledExprIdentity() if !rhs.distinct && rhs.where.isEmpty => compiled
      case _ => return rhs.toSubquery.flatMap(leftJoinNullable(_, on))
    }
    given Codec[T] = selectLhs.arg.codec
    given Codec[R2] = rhs.select.arg.codec
    val extractLhs = CompiledExpr[(T, R2), T](_._1)
    val extractRhs = CompiledExpr[(T, R2), R2](_._2)
    val newSelect = selectLhs.compose(extractLhs).combine(selectRhs.compose(extractRhs))
    val newWhere = where.map(_.compose(extractLhs))
    TypedFrom
      .leftJoin(from, rhs.from, on.compose(selectLhs, selectRhs), args)
      .map(newFrom => SelectBuilder(args, from = newFrom, select = newSelect, where = newWhere))
  }

  def fullJoin[U, R2](
      rhs: SelectBuilder[U, R2],
      on: CompiledExpr2[R, R2, Boolean]
  ): Result[SelectBuilder[?, (Option[R], Option[R2])]] = {
    given Codec[R] = select.expr.codec
    given Codec[R2] = rhs.select.expr.codec
    val wrapLhs = CompiledExpr[R, Option[R]](e => Expr.some(e))
    val unwrapLhs = CompiledExpr[Option[R], R](e => Expr.knownNotNull(e))
    val wrapRhs = CompiledExpr[R2, Option[R2]](e => Expr.some(e))
    val unwrapRhs = CompiledExpr[Option[R2], R2](e => Expr.knownNotNull(e))
    for {
      lhsNullable <- select(wrapLhs).flatMap(_.toSubquery)
      rhsNullable <- rhs.select(wrapRhs).flatMap(_.toSubquery)
      joined <- lhsNullable.fullJoinNullable(rhsNullable, on.compose(unwrapLhs, unwrapRhs))
    } yield joined
  }

  def antiJoin[R2](rhs: Dataset[R2], on: CompiledExpr2[R, R2, Boolean]): Result[SelectBuilder[?, R]] = {
    val lhsSelect = select match {
      case compiled @ CompiledExpr(_, _) if !distinct => compiled
      case _ => return toSubquery.flatMap(_.antiJoin(rhs, on))
    }
    given Codec[R2] = rhs.codec
    val onComposed = on.compose(lhsSelect, CompiledExpr[R2, R2](identity))
    val correlatedSubqery = Dataset.Filter(rhs, CompiledExpr(onComposed.arg2, onComposed.expr))
    val notExistsExpr =
      CompiledExpr[T, Boolean](onComposed.arg1, ExprNode.Not(ExprNode.ExistsSubquery(correlatedSubqery)))
    Right(copy(where =
      combinePredicates(where, Some(CompiledExpr[T, Boolean](onComposed.arg1, notExistsExpr.expr)))
    ))
  }

  def union(rhs: SelectBuilder[?, R]): Result[SelectBuilder[?, R]] =
    for {
      left <- build()
      right <- rhs.build()
    } yield {
      val newFrom = TypedFrom(Query.Union(left, right, true), args.aliasGen)(using select.expr.codec)
      SelectBuilder.from(newFrom, args)
    }

  def except(rhs: SelectBuilder[?, ?]): Result[SelectBuilder[?, R]] =
    for {
      left <- build()
      right <- rhs.build()
    } yield {
      val newFrom = TypedFrom(Query.Except(left, right, false), args.aliasGen)(using select.expr.codec)
      SelectBuilder.from(newFrom, args)
    }

  private def toSubquery: Result[SelectBuilder[R, R]] = build().map(makeSubquery(_, select.expr.codec))

  private def requiresSubqueryForSeqOps[T, R](compiled: CompiledExprOrExplode[T, R]): Boolean =
    compiled match {
      case CompiledExpr(_, expr) => requiresSubqueryForSeqOps(expr)
      case CompiledExplodeExpr(_, expr) => requiresSubqueryForSeqOps(expr)
    }

  private def isAggregateAndUsesSubqueryForArrayOps: Boolean =
    select.isInstanceOf[CompiledAggregateExpr[?, ?]] &&
      args.dialect.arrayHigherOrderFunctions.isInstanceOf[SqlDialect.ArrayHigherOrderFunctions.Subquery]

  private def requiresSubqueryForSeqOps(expr: ExprNode[?]): Boolean =
    isAggregateAndUsesSubqueryForArrayOps && containsSeqHigherOrderOp(expr)

  private def requiresSubqueryForAnySeqOps[R2 <: Tuple](
      exprs: Tuple.Map[R2, [X] =>> CompiledExprOrExplode[R, X]]
  ): Boolean =
    isAggregateAndUsesSubqueryForArrayOps && tupleInstances(exprs).foldLeft0(false)([t] =>
      (acc, compiled) =>
        compiled match {
          case CompiledExpr(_, expr) if containsSeqHigherOrderOp(expr) => Complete(true)
          case _ => acc
        }
    )

  private def containsSeqHigherOrderOp(expr: ExprNode[?]): Boolean =
    expr.exists {
      case ExprNode.MapSeq(_, _) | ExprNode.FlattenSeq(_) | ExprNode.AggregateSeq(_, _, _) => true
      case _ => false
    }

  private def makeSubquery[T](query: Query, codec: Codec[T]): SelectBuilder[T, T] =
    SelectBuilder.from(TypedFrom(query, args.aliasGen)(using codec), args)

  private def selectExplode[R2](explode: CompiledExplodeExpr[R, R2]): Result[SelectBuilder[?, R2]] = {
    given Codec[R2] = explode.codec
    args.dialect.explode match {
      case SqlDialect.ExplodeSupport.Function(_) => selectExplodeFunction(explode)
      case SqlDialect.ExplodeSupport.InnerJoin(_) => selectExplodeJoin(explode)
    }
  }

  private def selectExplodeFunction[R2: Codec](
      explode: CompiledExplodeExpr[R, R2]
  ): Result[SelectBuilder[?, R2]] = {
    val newSelect = andThenRelaxed(select, explode)
    explode.codec match {
      case Codec.Product(_, _, _) | Codec.Sum(_, _) =>
        // When exploding to a structured type we end up with a single column and need to flatten it manually
        buildWithSelect(
          NonEmpty[Seq]((newSelect, "_1"))
        ).flatMap(makeSubquery[Tuple1[R2]](_, summon).select(CompiledExpr(_._1)))
      case _ => buildWithSelect(NonEmpty[Seq]((newSelect, "value"))).map(makeSubquery(_, explode.codec))
    }
  }

  private def selectExplodeJoin[R2: Codec](
      explode: CompiledExplodeExpr[R, R2]
  ): Result[SelectBuilder[?, R2]] =
    select match {
      case compiled: CompiledExpr[T, R] if !distinct =>
        given Codec[T] = from.output.codec
        val newSelect = CompiledExpr[(T, R2), R2](_._2)
        val newWhere = where.map(_.compose(CompiledExpr[(T, R2), T](_._1)))
        TypedFrom
          .join(from, explode.compose(compiled), args)
          .map(newFrom => SelectBuilder(args, from = newFrom, select = newSelect, where = newWhere))
      case _ =>
        given Codec[Iterable[R2]] = explode.expr.codec
        toSubquery
          .flatMap(_.select(explode.asCompiledExpr))
          .flatMap(_.select(CompiledExprOrExplode[Iterable[R2], R2](Expr.explode(identity))))
    }

  private def selectNExplode[R2 <: Tuple](
      exprs: Tuple.Map[R2, [X] =>> CompiledExprOrExplode[R, X]]
  ): Result[SelectBuilder[?, R2]] =
    args.dialect.explode match {
      case SqlDialect.ExplodeSupport.Function(_) => selectNExplodeFunction(exprs)
      case SqlDialect.ExplodeSupport.InnerJoin(_) => selectNExplodeJoin(exprs)
    }

  private def selectNExplodeFunction[R2 <: Tuple](
      exprs: Tuple.Map[R2, [X] =>> CompiledExprOrExplode[R, X]]
  ): Result[SelectBuilder[?, R2]] = {
    val codec = Codec.tuple(tupleInstances(exprs).mapK([t] => _.codec))
    val newSelect = NonEmpty
      .from(
        tupleInstances(exprs)
          .mapConst([t] => compiled => andThenRelaxed(select, compiled))
          .zipWithIndex
          .map { case (e, i) => (e, s"_${i + 1}") }
      )
      .getOrElse(unreachable("Selects contains at least one statement"))

    buildWithSelect(newSelect).map(makeSubquery(_, codec))
  }

  private def selectNExplodeJoin[R2 <: Tuple](
      exprs: Tuple.Map[R2, [X] =>> CompiledExprOrExplode[R, X]]
  ): Result[SelectBuilder[?, R2]] = {
    val instances = tupleInstances(exprs)
    select match {
      case compiled: CompiledExpr[T, R] if !distinct =>
        // The explodes can be added as joins in the existing from without a subquery.
        val explodeCodecs = instances.foldLeft0(Vector.empty[Codec[?]])([t] =>
          (acc, select) =>
            select match {
              case CompiledExpr(_, _) => acc
              case explode: CompiledExplodeExpr[?, ?] => acc :+ explode.expr.codec
            }
        )
        // TYPE SAFETY: Each value in (from.output.codec +: explodeCodecs) is a Codec
        val newFromCodec: Codec.Product[Tuple] = Codec.tuple(tupleInstances(
          Tuple.fromArray((from.output.codec +: explodeCodecs).toArray).asInstanceOf[Tuple.Map[Tuple, Codec]]
        ))
        val newArg = ExprNode.Reference()(using newFromCodec)
        val joinExprs = mutable.ArrayBuffer[CompiledExplodeExpr[T, ?]]()
        val selectExprs = instances.mapK([t] =>
          _ match {
            case noExplode @ CompiledExpr(_, _) =>
              val composed = compiled.andThen(noExplode)
              composed.expr.replace(composed.arg, ExprNode.Select(newArg, "_1"))
            case explode @ CompiledExplodeExpr(_, _) =>
              joinExprs.addOne(explode.compose(compiled))
              ExprNode.Select(newArg, s"_${joinExprs.size + 1}")
          }
        )
        val newSelect = CompiledExpr(newArg, ExprNode.makeTuple[R2](selectExprs.toTuple))
        val newWhere =
          where.map(cond => CompiledExpr(newArg, cond.expr.replace(cond.arg, ExprNode.Select(newArg, "_1"))))
        TypedFrom
          .joinExplode(from, joinExprs.toSeq, newFromCodec, args)
          .map(newFrom => SelectBuilder(args, from = newFrom, select = newSelect, where = newWhere))
      case _ => toSubquery.flatMap(_.selectN(exprs))
    }
  }

  def build(): Result[Query] = {
    /* For builders that are only select directly from a subquery, we return the subquery to avoid an
     * unnecessary select */
    this match {
      case SelectBuilder(
            _,
            TypedFrom(From.Subquery(query, _), _, _),
            CompiledExprIdentity(),
            None,
            None,
            None,
            false,
            None,
            None
          ) => return Right(query)
      case _ => ()
    }
    def finalSelect[A](expr: ExprNode[A]): FinalSelect[T] =
      expr.codec match {
        case Codec.Product(_, fieldInstances, _) =>
          val maybeFields = NonEmpty.from(
            fieldInstances
              .mapConst[Field[?]]([t] => identity(_))
              .map(f => (RelaxedCompiledExpr(select.arg, ExprNode.Select(expr, f.name)), f.name))
          )
          maybeFields.map(FinalSelect.Multiple(_)).getOrElse(FinalSelect.Empty())
        case sum @ Codec.Sum(_, _) => finalSelect(ExprNode.ToRepr(expr, sum))
        case _ => FinalSelect.Multiple(NonEmpty((RelaxedCompiledExpr(select), "value")))
      }

    buildWithSelect(finalSelect(select.expr))
  }

  private def buildWithSelect(
      finalSelect: NonEmpty[Seq[(RelaxedCompiledExpr[T, ?], String)]]
  ): Result[Query] = buildWithSelect(FinalSelect.Multiple(finalSelect))

  private def buildWithSelect(finalSelect: FinalSelect[T]): Result[Query] = {
    val output = from.output
    val ids = from.ids
    def relaxedToSqlExpr(compiled: RelaxedCompiledExpr[T, ?]): Result[SqlExpr] = {
      val node = compiled.expr match {
        case ExplodeExpr(node) => ExplodeExpr(node.replace(compiled.arg, output))
        case node: ExprNode[?] => node.replace(compiled.arg, output)
      }
      simplifyAndToSqlExpr(node, ids, args)
    }
    def groupByToSqlExpr(compiled: CompiledExpr[T, ?]): Result[Seq[SqlExpr]] = {
      val node = compiled.expr.replace(compiled.arg, output)
      flattenMakeStructAndRemoveLiterals(node).map(simplifyAndToSqlExpr(_, ids, args)).sequence
    }

    def orderByToSqlExpr(compiled: CompiledExpr[T, ?]): Result[Seq[SqlExpr]] = {
      val node = compiled.expr.replace(compiled.arg, output)
      val nodes = flattenMakeStructAndRemoveLiterals(node).flatMap(flattenForOrderBy(args.dialect, _))
      nodes.map(simplifyAndToSqlExpr(_, ids, args)).sequence
    }

    def compiledToSqlExpr(compiled: CompiledExpr[T, ?]): Result[SqlExpr] =
      simplifyAndToSqlExpr(compiled.expr.replace(compiled.arg, output), ids, args)
    val maybeSelect = finalSelect match {
      case FinalSelect.Multiple(exprs) =>
        exprs.map((compiled, alias) => relaxedToSqlExpr(compiled).map(SqlExpr.As(_, alias))).sequence
      case FinalSelect.Aggregate(agg) => relaxedToSqlExpr(RelaxedCompiledExpr(agg))
          .map(sqlAgg =>
            val count = SqlExpr.Function("count", Seq(SqlExpr.LiteralNumeric("1")))
            val nonEmpty = SqlExpr.BinaryOp("<>", count, SqlExpr.LiteralNumeric("0"))
            SqlExpr.As(SqlExpr.Case(Seq((condition = nonEmpty, result = sqlAgg))), "value")
          )
          .map(NonEmpty[Seq](_))
      case FinalSelect.Empty() => Right(NonEmpty[Seq](emptyProductFieldNull(args.dialect)))
    }
    for {
      select <- maybeSelect
      where <- where.map(compiledToSqlExpr).sequence
      groupBy <- groupBy.toSeq.map(groupByToSqlExpr).sequence.map(_.flatten)
      having <- having.map(compiledToSqlExpr).sequence
      orderByExprs <- orderBy.toSeq.map(orderByToSqlExpr).sequence.map(_.flatten)
    } yield Query.Select(
      select = select,
      from = Some(from.from),
      where = where,
      groupBy = groupBy,
      having = having,
      distinct = distinct,
      orderBy = orderByExprs,
      limit = limit
    )
  }

  def simplifyExprs(): SelectBuilder[T, R] =
    SelectBuilder(
      args,
      from,
      select match {
        case compiled: CompiledExpr[T, R] => simplifySelects(compiled)
        case aggregate: CompiledAggregateExpr[T, R] => simplifySelects(aggregate)
      },
      where.map(simplifySelects),
      groupBy.map(simplifySelects),
      having.map(simplifySelects),
      distinct,
      orderBy.map(simplifySelects),
      limit
    )
}

private object SelectBuilder {
  def from[T](from: TypedFrom[T], args: UnparserArgs): SelectBuilder[T, T] = {
    val select = CompiledExpr[T, T](identity)(using from.output.codec)
    SelectBuilder(args, from, select)
  }

  extension [R <: Option[?]: Codec](lhs: SelectBuilder[R, R]) {
    private def fullJoinNullable[R2 <: Option[?]: Codec](
        rhs: SelectBuilder[R2, R2],
        on: CompiledExpr2[R, R2, Boolean]
    ): Result[SelectBuilder[?, (R, R2)]] = {
      val selectLhs = lhs.select match {
        case compiled @ CompiledExprIdentity() if !lhs.distinct && lhs.where.isEmpty => compiled
        case _ => return lhs.toSubquery.flatMap(_.fullJoinNullable(rhs, on))
      }
      val selectRhs = rhs.select match {
        case compiled @ CompiledExprIdentity() if !rhs.distinct && rhs.where.isEmpty => compiled
        case _ => return rhs.toSubquery.flatMap(lhs.fullJoinNullable(_, on))
      }
      TypedFrom
        .fullJoin(lhs.from, rhs.from, on.compose(selectLhs, selectRhs), lhs.args)
        .map(newFrom =>
          SelectBuilder(lhs.args, from = newFrom, select = CompiledExpr[(R, R2), (R, R2)](identity))
        )
    }
  }

  enum FinalSelect[T] {
    case Multiple(exprs: NonEmpty[Seq[(RelaxedCompiledExpr[T, ?], String)]])
    // When aggregating a whole dataset we should produce None on empty data, this need to be
    // implemented using the untyped api.
    case Aggregate(agg: CompiledAggregateExpr[T, ?])
    // Empty select used for selecting singleton values. This will mainly show up in tests
    case Empty[T]() extends FinalSelect[T]
  }

  private def getFields(c: Codec[?]): Seq[Field[?]] =
    c match {
      case Codec.Product(_, fields, _) => fields.mapConst[Field[?]]([t] => identity(_))
      case s @ Codec.Sum(_) => s.reprFields
      case other => Seq(Field("value", other))
    }

  // This can be seens as a codec for NamedTuple.Concat[NamedTuple.From[P], NamedTuple.From[T]]
  private def combinedFlatCodec[P: Codec, T: Codec]: Codec[?] = {
    val fields = getFields(Codec[P]) ++ getFields(Codec[T])
    assert(fields.map(_.name).distinct.size == fields.size, "Field names in combined codec must be unique")
    val tag: ClassTag[AnyNamedTuple] = Codec.tupleClassTag(fields.size)
    // TYPE SAFETY: NamedTuples mirror is just a Tuple mirror with different refinements
    val mirror = new scala.runtime.TupleMirror(fields.size).asInstanceOf[Mirror.ProductOf[AnyNamedTuple]]
    val labelling = Labelling[AnyNamedTuple](s"Tuple${fields.size}", fields.map(_.name).toIndexedSeq)
    val instances: K0.ProductInstances[Codec, AnyNamedTuple] = fields.size match {
      case 1 => new ErasedProductInstances1(mirror, () => fields.head.codec)
      case _ => new ErasedProductInstancesN(mirror, () => fields.toArray.map(_.codec: Any))
    }
    Codec.product(using tag, mirror, labelling, instances)
  }

  def empty[T](codec: Codec[T], args: UnparserArgs): Result[SelectBuilder[T, T]] =
    TypedFrom
      .dummy(codec, args)
      .map(SelectBuilder.from(_, args).filter(CompiledExpr[T, Boolean](_ => lit(false))(using codec)))

  def fromSeq[T](values: Seq[T], codec: Codec[T], args: UnparserArgs): Result[SelectBuilder[T, T]] =
    NonEmpty
      .from(values)
      .fold(empty(codec, args))(TypedFrom.values(_, codec, args).map(SelectBuilder.from(_, args)))

  def fromTable[P, T](
      name: String,
      partitionCodec: Codec[P],
      modelCodec: Codec[T],
      args: UnparserArgs
  ): SelectBuilder[?, (P, T)] = {
    val arg = ExprNode.Reference(codec = combinedFlatCodec(using partitionCodec, modelCodec))
    def fields[C](codec: Codec[C]): ExprNode[C] =
      codec match {
        case codec @ (Codec.Product(_, _, _) | Codec.Sum(_, _)) =>
          val fieldsExprs = getFields(codec).map(f => ExprNode.Select(arg, f.name))
          def product[I](prod: Codec.Product[I]) =
            ExprNode.MakeProduct(
              // TYPE SAFETY: Each element in fieldsExprs has type ExprNode[?]
              values = Tuple.fromArray(fieldsExprs.toArray).asInstanceOf[Tuple.Map[Tuple, ExprNode]],
              codec = prod
            )
          codec match {
            case prod @ Codec.Product(_, _, _) => product(prod)
            case sum @ Codec.Sum(_, _) => ExprNode.FromRepr(product(sum.reprCodec), sum)
          }
        case _ => ExprNode.Select(arg, "value")
      }
    val from = TypedFrom.named(name, arg)
    val select = CompiledExpr(arg, ExprNode.makeTuple[(P, T)]((fields(partitionCodec), fields(modelCodec))))
    SelectBuilder(args = args, from = from, select = select)
  }

  private def combinePredicates[T](
      pred1: Option[CompiledExpr[T, Boolean]],
      pred2: Option[CompiledExpr[T, Boolean]]
  ): Option[CompiledExpr[T, Boolean]] = pred1.zip(pred2).map(_ && _).orElse(pred1).orElse(pred2)

  private def combinePredicates[T](
      pred1: Option[CompiledExpr[T, Boolean]],
      pred2: CompiledExpr[T, Boolean]
  ): CompiledExpr[T, Boolean] = pred1.fold(pred2)(_ && pred2)

  type CompiledExprOrAggregate[T, R] = CompiledExpr[T, R] | CompiledAggregateExpr[T, R]

  extension [T, R](select: CompiledExprOrAggregate[T, R]) {
    private def expr: ExprNode[R] =
      select match {
        case agg: CompiledAggregateExpr[T, R] => agg.expr
        case e: CompiledExpr[T, R] => e.expr
      }

    private def arg: ExprNode.Reference[T] =
      select match {
        case agg: CompiledAggregateExpr[T, R] => agg.arg
        case e: CompiledExpr[T, R] => e.arg
      }
  }

  private def isJoin(from: TypedFrom[?]): Boolean =
    from match {
      case TypedFrom(From.Join(_, _, _, _), _, _) => true
      case _ => false
    }

  private def hasMakeProductAfterFlattening(newGroupBy: CompiledExpr[?, ?]): Boolean =
    flattenMakeStructAndRemoveLiterals(newGroupBy.expr).exists(_.exists {
      case ExprNode.MakeProduct(_, _) => true
      case ExprNode.MakeSome(Nullable(_)) => true
      case _ => false
    })

  private def flattenMakeStructAndRemoveLiterals(node: ExprNode[?]): Seq[ExprNode[?]] = {
    val exprs = simplifySelects(node)
      .fold(Seq.empty[ExprNode[?]])((acc, expr) =>
        expr match {
          case ExprNode.MakeProduct(_, _) => Continue(acc)
          case ExprNode.Literal(_, _) | ExprNode.None(_) => Skip(acc)
          // MakeSome are either no-op or make struct so we need to continue into them
          case ExprNode.MakeSome(_) => Continue(acc)
          // The as and from sum repr are just no-ops in the this backend
          case ExprNode.FromRepr(_, _) | ExprNode.ToRepr(_, _) => Continue(acc)
          // References to top level product will result as a make struct so we need to unpack them
          case expr @ ExprNode.Reference(_, _) => expr match {
              case StructFields(fields) => Skip(acc ++ fields)
              case _ => Skip(acc :+ expr)
            }
          case other => Skip(acc :+ other)
        }
      )
      .distinct
    // We need at least one group by expression to preserve correct behavior for empty inputs.
    if exprs.isEmpty then Seq(ExprNode.None(Codec.Int)) else exprs
  }

  private def flattenForOrderBy[T](dialect: SqlDialect, node: ExprNode[T]): Seq[ExprNode[?]] = {
    def inner[A](node: ExprNode[A]): Option[Seq[ExprNode[?]]] =
      node.codec match {
        case codec @ (Codec.Float | Codec.Double)
            if dialect.floatingOrder == SqlDialect.FloatingOrder.NaNFirst =>
          val nanCheck = codec match {
            case Codec.Float => ExprNode.IsNaN[Float](node)
            case Codec.Double => ExprNode.IsNaN[Double](node)
          }
          Some(Seq(nanCheck, node))
        case Codec.Product(_, fields, _) =>
          val fieldsExpanded = fields.mapConst { [t] => f =>
            val fieldNode = ExprNode.Select(node, f.name)
            (expansion = inner(fieldNode), fieldNode = fieldNode)
          }
          val expansionNeeded = dialect.flattenStructInOrderBy || fieldsExpanded.exists(_.expansion.isDefined)
          Option.when(expansionNeeded)(
            fieldsExpanded.map((expansion, fieldNode) => expansion.getOrElse(Seq(fieldNode))).flatten
          )
        case Codec.Option(element: Codec[t]) =>
          val nullCheck = !IsNone(node: ExprNode[Option[t]])
          val innerNode = ExprNode.KnownNotNull(node: ExprNode[Option[t]])
          element match {
            case Codec.Option(_) if dialect.flattenStructInOrderBy =>
              Some(nullCheck +: inner(innerNode).getOrElse(Seq(innerNode)))
            // No need to perform extra null check on floats as nulls will already be first
            case Codec.Float | Codec.Double => inner(innerNode)
            case _ => inner(innerNode).map(nullCheck +: _)
          }
        case fi @ Codec.FromInjection(_, _) => inner(ExprNode.ToRepr(node, fi))
        case _ => None
      }
    inner(node).getOrElse(Seq(node))
  }

  private def andThenRelaxed[T, R, A](
      select: CompiledExprOrAggregate[T, R],
      g: CompiledExprOrExplode[R, A]
  ): RelaxedCompiledExpr[T, A] = RelaxedCompiledExpr(g).compose(select)

  private def andThen[T, R, A](
      select: CompiledExprOrAggregate[T, R],
      g: CompiledExpr[R, A]
  ): CompiledExprOrAggregate[T, A] =
    select match {
      case agg: CompiledAggregateExpr[T, R] => agg.andThen(g)
      case e: CompiledExpr[T, R] => e.andThen(g)
    }
}
