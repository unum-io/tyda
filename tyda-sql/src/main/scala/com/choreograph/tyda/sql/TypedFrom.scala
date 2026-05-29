package com.choreograph.tyda.sql

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.rewrite.reduceBalanced
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.sql.Result.sequence
import com.choreograph.tyda.sql.ast.From
import com.choreograph.tyda.sql.ast.JoinType
import com.choreograph.tyda.sql.ast.Query
import com.choreograph.tyda.sql.ast.SqlExpr

/** Typed version of [[ast.From]].
  *
  * This additionally keeps information about the type in the [[ast.From]] how
  * that related the underlying table ids.
  */
private final case class TypedFrom[T](
    from: From,
    output: ExprNode[T],
    ids: Map[ExprNode.Reference[?], IdentifierOrSqlExpr]
)

private object TypedFrom {
  def join[T, U](
      left: TypedFrom[T],
      right: TypedFrom[U],
      on: CompiledExpr2[T, U, Boolean],
      args: UnparserArgs
  ): Result[TypedFrom[(T, U)]] = {
    val combinedIds = left.ids ++ right.ids
    val exprReplaced = on.expr.replace(on.arg1, left.output).replace(on.arg2, right.output)
    simplifyAndToSqlExpr(exprReplaced, combinedIds, args).map(onSql =>
      TypedFrom(
        From.Join(left.from, right.from, JoinType.Inner, Some(onSql)),
        ExprNode.makeTuple(left.output, right.output),
        combinedIds
      )
    )
  }

  private def joinExplodeReferenceAndOutput[U](
      codec: Codec[Iterable[U]],
      dialect: SqlDialect
  ): (reference: ExprNode.Reference[?], output: ExprNode[U]) = {
    given elementCodec: Codec[U] = codec.element
    if shouldWrapArrayElement(elementCodec, dialect.ddl) then {
      val ref = ExprNode.Reference[(value: U)]()
      (ref, ExprNode.Select(ref, "value"))
    } else {
      val ref = ExprNode.Reference[U]()
      (ref, ref)
    }
  }

  /** Explode right column using INNER JOIN syntax */
  def join[T, U](
      left: TypedFrom[T],
      right: CompiledExpr[T, Iterable[U]],
      args: UnparserArgs
  ): Result[TypedFrom[(T, U)]] = {
    val exprReplaced = right.expr.replace(right.arg, left.output)
    val alias = args.aliasGen.table()
    val (ref, output) = joinExplodeReferenceAndOutput(right.codec, args.dialect)
    simplifyAndToSqlExpr(exprReplaced, left.ids, args).map(rightSql =>
      TypedFrom(
        From.Join(left.from, From.Expr(rightSql, alias), JoinType.Inner, None),
        ExprNode.makeTuple(left.output, output),
        left.ids + (ref -> IdentifierOrSqlExpr.Expr(alias))
      )
    )
  }

  /** Explode multiple right column using INNER JOIN syntax
    *
    * The codec for R should be a flat tuple containing `T` as the first element
    * and then all of the elements of right after that.
    */
  def joinExplode[T, R <: Tuple](
      left: TypedFrom[T],
      rights: Seq[CompiledExplodeExpr[T, ?]],
      codec: Codec.Product[R],
      args: UnparserArgs
  ): Result[TypedFrom[R]] = {
    assert(codec.fields.arity == rights.length + 1, "Codec arity does not match number of right expressions")
    rights
      .map(f =>
        val expr = f.expr.replace(f.arg, left.output)
        for {
          sqlExpr <- simplifyAndToSqlExpr(expr, left.ids, args)
          (ref, output) = joinExplodeReferenceAndOutput(f.expr.codec, args.dialect)
        } yield (ref, output, sqlExpr)
      )
      .sequence
      .map { refsAndSqlExprs =>
        val (refs, outputs, exprs) = refsAndSqlExprs.unzip3
        /* TYPE SAFETY: Each element of refs and output are of type ExprNode and that the types match the
         * codec R is a precondition of this function */
        val output = ExprNode.makeTuple(
          Tuple.fromArray((left.output +: outputs).toArray).asInstanceOf[Tuple.Map[R, ExprNode]]
        )
        val aliases = refs.map(_ => args.aliasGen.table())
        val combinedIds = left.ids ++ refs.zip(aliases).map((r, a) => r -> IdentifierOrSqlExpr.Expr(a))
        val from = exprs
          .zip(aliases)
          .foldLeft(left.from) { case (from, (expr, alias)) =>
            From.Join(from, From.Expr(expr, alias), JoinType.Inner, None)
          }
        TypedFrom(from, output, combinedIds)
      }
  }

  /** The U being an Option is to make sure the right side output is nullable.
    * It up to the caller to ensure that the `Option` value is never `None`
    * before the join. Otherwise it will not be possible to tell `None` apart
    * from existing `None` values.
    */
  def leftJoin[T, U <: Option[?]](
      left: TypedFrom[T],
      right: TypedFrom[U],
      on: CompiledExpr2[T, U, Boolean],
      args: UnparserArgs
  ): Result[TypedFrom[(T, U)]] = {
    given Codec[U] = right.output.codec
    val exprReplaced = on.expr.replace(on.arg1, left.output).replace(on.arg2, right.output)
    val nullableRight = ExprNode.Reference[U]()
    for {
      rightIdentOrSql <-
        simplifyAndToSqlExpr(right.output, right.ids, args).map(sql => IdentifierOrSqlExpr.Expr(sql))
      onSql <- simplifyAndToSqlExpr(exprReplaced, left.ids ++ right.ids, args)
    } yield TypedFrom(
      From.Join(left.from, right.from, JoinType.Left, Some(onSql)),
      ExprNode.makeTuple[(T, U)](left.output, nullableRight),
      left.ids ++ Map(nullableRight -> rightIdentOrSql)
    )
  }

  /** The T and U being Options is to make sure both sides outputs are nullable.
    * It up to the caller to ensure that the `Option` values are never `None`
    * before the join. Otherwise it will not be possible to tell `None` apart
    * from existing `None` values.
    */
  def fullJoin[T <: Option[?], U <: Option[?]](
      left: TypedFrom[T],
      right: TypedFrom[U],
      on: CompiledExpr2[T, U, Boolean],
      args: UnparserArgs
  ): Result[TypedFrom[(T, U)]] = {
    given Codec[T] = left.output.codec
    given Codec[U] = right.output.codec
    val nullableLeft = ExprNode.Reference[T]()
    val nullableRight = ExprNode.Reference[U]()
    val exprReplaced = on.expr.replace(on.arg1, left.output).replace(on.arg2, right.output)
    for {
      leftIdentOrSql <-
        simplifyAndToSqlExpr(left.output, left.ids, args).map(sql => IdentifierOrSqlExpr.Expr(sql))
      rightIdentOrSql <-
        simplifyAndToSqlExpr(right.output, right.ids, args).map(sql => IdentifierOrSqlExpr.Expr(sql))
      onSql <- simplifyAndToSqlExpr(exprReplaced, left.ids ++ right.ids, args)
    } yield TypedFrom(
      From.Join(left.from, right.from, JoinType.Full, Some(onSql)),
      ExprNode.makeTuple[(T, U)](nullableLeft, nullableRight),
      Map(nullableLeft -> leftIdentOrSql, nullableRight -> rightIdentOrSql)
    )
  }

  def named[T](identifier: String, ref: ExprNode.Reference[T]): TypedFrom[T] =
    TypedFrom(From.Table(identifier), ref, Map(ref -> IdentifierOrSqlExpr.Ident(identifier)))

  /** VALUES with a single dummy row with the provided schema */
  def dummy[T](codec: Codec[T], args: UnparserArgs): Result[TypedFrom[T]] =
    dummyValue(codec, args).map((exprs, names) => makeValues(NonEmpty(exprs), names, codec, args))

  private def valuesNative[T](
      exprs: Seq[NonEmpty[Seq[SqlExpr]]],
      names: NonEmpty[Seq[String]],
      codec: Codec[T],
      args: UnparserArgs
  ): TypedFrom[T] = {
    val alias = args.aliasGen.table()
    val ref = ExprNode.Reference[T]()(using codec)
    TypedFrom(From.Values(exprs, names, alias), ref, Map(ref -> IdentifierOrSqlExpr.Ident(alias)))
  }

  private def valuesSelectUnionAll[T](
      exprs: NonEmpty[Seq[NonEmpty[Seq[SqlExpr]]]],
      names: NonEmpty[Seq[String]],
      codec: Codec[T],
      args: UnparserArgs
  ): TypedFrom[T] = {
    val unions = exprs
      .map(_.zip(names).map((expr, name) => SqlExpr.As(expr, name)))
      .map(Query.Select(_, None, None, Seq.empty, None, false))
      .reduceBalanced((left, right) => Query.Union(left, right, all = true))
    val alias = args.aliasGen.table()
    val ref = ExprNode.Reference[T]()(using codec)
    TypedFrom(From.Subquery(unions, alias), ref, Map(ref -> IdentifierOrSqlExpr.Ident(alias)))
  }

  private def makeValues[T](
      exprs: NonEmpty[Seq[NonEmpty[Seq[SqlExpr]]]],
      names: NonEmpty[Seq[String]],
      codec: Codec[T],
      args: UnparserArgs
  ): TypedFrom[T] =
    args.dialect.values match {
      case SqlDialect.Values.Native => valuesNative(exprs, names, codec, args)
      case SqlDialect.Values.SelectUnionAll => valuesSelectUnionAll(exprs, names, codec, args)
    }

  private def seqAsValues[T](
      values: NonEmpty[Seq[T]],
      codec: Codec[T],
      args: UnparserArgs
  ): Result[(NonEmpty[Seq[NonEmpty[Seq[SqlExpr]]]], NonEmpty[Seq[String]])] =
    codec match {
      case prod @ Codec.Product(_, _, _) => seqAsValuesForProduct(values, prod, args)
      case Codec.FromInjection(inj, to) => seqAsValues(values.map(inj.apply(_)), to, args)
      case _ =>
        given Codec[T] = codec
        seqAsValuesForProduct(values.map((value = _)), Codec.product[(value: T)], args)
    }

  private def seqAsValuesForProduct[T](
      values: NonEmpty[Seq[T]],
      codec: Codec.Product[T],
      args: UnparserArgs
  ): Result[(NonEmpty[Seq[NonEmpty[Seq[SqlExpr]]]], NonEmpty[Seq[String]])] =
    val exprs = values.map(literalProductToFields(_, codec, args)).sequence
    val names = NonEmpty.from(codec.fields.mapConst[String]([t] => _.name))
    exprs.map((_, names.getOrElse(NonEmpty(Forbidden.column))))

  def values[T](data: NonEmpty[Seq[T]], codec: Codec[T], args: UnparserArgs): Result[TypedFrom[T]] =
    seqAsValues(data, codec, args).map(makeValues(_, _, codec, args))

  def apply[T: Codec](query: Query, aliasGen: AliasGenerator): TypedFrom[T] = {
    val alias = aliasGen.table()
    val ref = ExprNode.Reference[T]()
    TypedFrom(From.Subquery(query, alias), ref, Map(ref -> IdentifierOrSqlExpr.Ident(alias)))
  }
}
