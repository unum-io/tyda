package com.choreograph.tyda.sql

import scala.util.chaining.given

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Codec.seq
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Decimal.MaxPrecision
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Errors
import com.choreograph.tyda.ExplodeExpr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ExprNode.KnownNotNull
import com.choreograph.tyda.ExprNode.Or
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.PrimitiveAggregate
import com.choreograph.tyda.SumMagnet
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.rewrite.ArrayCodec
import com.choreograph.tyda.rewrite.CollectionOrNullableCollectionCodec
import com.choreograph.tyda.rewrite.IsNone
import com.choreograph.tyda.rewrite.MapOption
import com.choreograph.tyda.rewrite.NotNullNonEmptyDummyLiteral
import com.choreograph.tyda.rewrite.Nullable
import com.choreograph.tyda.rewrite.PrimitiveAggregateAsFold
import com.choreograph.tyda.rewrite.SimplifySelects
import com.choreograph.tyda.rewrite.ToFromRepr
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.sql.DdlDialect.DecimalSupport
import com.choreograph.tyda.sql.Result.sequence
import com.choreograph.tyda.sql.SqlDialect.IntegerSupport
import com.choreograph.tyda.sql.SqlDialect.SplitFunction
import com.choreograph.tyda.sql.SqlDialect.TrimFunction
import com.choreograph.tyda.sql.ast.DdlType
import com.choreograph.tyda.sql.ast.DdlWriter
import com.choreograph.tyda.sql.ast.From
import com.choreograph.tyda.sql.ast.Query
import com.choreograph.tyda.sql.ast.SqlExpr
import com.choreograph.tyda.unreachable

private def simplifyAndToSqlExpr(
    expr: ExprNode[?] | ExplodeExpr[?],
    extraReferences: Map[ExprNode.Reference[?], IdentifierOrSqlExpr],
    args: UnparserArgs
): Result[SqlExpr] = simplifyAndToSqlExpr(expr, args.withReferences(extraReferences.toSeq*))

private def simplifyAndToSqlExpr(expr: ExprNode[?] | ExplodeExpr[?], args: UnparserArgs): Result[SqlExpr] = {
  val simplified = expr match {
    case e: ExplodeExpr[?] => ExplodeExpr(simplifySelects(e.expr))
    case e: ExprNode[?] => simplifySelects(e)
  }
  exprToSqlExpr(simplified, args)
}

/* These rules are to generate simpler SQL for outer joins where we do extra handling to make sure we get
 * correct nullability. Hopefully this should be replaced by some more general handling around MapOption and
 * null intolerant expressions. */
private object OuterJoinRules {
  def unapply[U](expr: ExprNode[U]): Option[ExprNode[U]] =
    expr match {

      // This is a _.map(identity), so we can just drop it entirely
      // TYPE SAFETY: arg has the same type as body
      case Nullable(MapOption(arg, bodyArg)) if bodyArg == arg => Some(arg.expr.asInstanceOf[ExprNode[U]])

      case _ => None
    }
}

private def simplifySelects[From, To](
    compiled: CompiledAggregateExpr[From, To]
): CompiledAggregateExpr[From, To] = compiled.copy(expr = simplifySelects(compiled.expr))

private def simplifySelects[From, To](compiled: CompiledExpr[From, To]): CompiledExpr[From, To] =
  compiled.copy(expr = simplifySelects(compiled.expr))

private def simplifySelects[T](expr: ExprNode[T]): ExprNode[T] =
  expr.transformUp([t] =>
    (n: ExprNode[t]) =>
      n match {
        case OuterJoinRules(opt) => Continue(opt)
        case SimplifySelects(simplified) => Continue(simplified)
        case ToFromRepr(value) => Continue(value)
        case other => Continue(other)
      }
  )

private def lambda[T, R](compiled: CompiledExpr[T, R], args: UnparserArgs): Result[SqlExpr] = {
  val argName = args.aliasGen.column()
  exprToSqlExpr(compiled.expr, args.withReferences(compiled.arg -> IdentifierOrSqlExpr.Expr(argName))).map(
    body => SqlExpr.LambdaFunction(SqlExpr.Ident(argName), body)
  )
}

private def lambda2[T1, T2, R](compiled: CompiledExpr2[T1, T2, R], args: UnparserArgs): Result[SqlExpr] = {
  val argName1 = args.aliasGen.column()
  val argName2 = args.aliasGen.column()
  exprToSqlExpr(
    compiled.expr,
    args.withReferences(
      compiled.arg1 -> IdentifierOrSqlExpr.Expr(argName1),
      compiled.arg2 -> IdentifierOrSqlExpr.Expr(argName2)
    )
  ).map(body => SqlExpr.LambdaFunction(Seq(SqlExpr.Ident(argName1), SqlExpr.Ident(argName2)), body))
}

private def exprToSqlExpr(expr: ExplodeExpr[?] | ExprNode[?], args: UnparserArgs): Result[SqlExpr] =
  expr match {
    case e: ExplodeExpr[?] => exprToSqlExpr(e.expr, args).map(arg =>
        args.dialect.explode match {
          case SqlDialect.ExplodeSupport.Function(name, _) => SqlExpr.Function(name, Seq(arg))
          case SqlDialect.ExplodeSupport.InnerJoin =>
            unreachable("Explodes using inner join should be handled in the SelectBuilder")
        }
      )
    case e: ExprNode[?] => exprToSqlExpr(e, args)
  }

private object TopLevelSelect {
  def unapply[T](expr: ExprNode.Select[?, T]): Option[(ExprNode.Reference[?], String)] =
    expr match {
      case ExprNode.Select(ref @ ExprNode.Reference(_, _), field) => Some((ref, field))
      case ExprNode.Select(ExprNode.ToRepr(ref @ ExprNode.Reference(_, _), _), field) => Some((ref, field))
      case _ => None
    }
}

private def shouldWrapArrayElement[T](elementCodec: Codec[T], dialect: DdlDialect): Boolean =
  elementCodec match {
    case CollectionOrNullableCollectionCodec(_) if !dialect.supportsArrayAsArrayElement => true
    case _ => false
  }

private def unwrapArrayElement[T](expr: SqlExpr, elementCodec: Codec[T], dialect: SqlDialect): SqlExpr =
  if shouldWrapArrayElement(elementCodec, dialect.ddl) then SqlExpr.FieldAccess(expr, "value") else expr

private def wrapArrayElement[T](expr: ExprNode[T], dialect: SqlDialect): ExprNode[?] = {
  given elementCodec: Codec[T] = expr.codec
  if shouldWrapArrayElement(elementCodec, dialect.ddl) then ExprNode.namedTuple((value = expr)) else expr
}

private def wrapArrayElement[T: Codec](expr: SqlExpr, dialect: SqlDialect): SqlExpr =
  if shouldWrapArrayElement(Codec[T], dialect.ddl) then makeStruct(Seq("value"), Seq(expr), dialect) else expr

private def exprToSqlExpr[T](fullExpr: ExprNode[T], args: UnparserArgs): Result[SqlExpr] = {
  val dialect = args.dialect

  def inner(expr: ExprNode[?]): Result[SqlExpr] = {
    def binaryOp(op: String, lhs: ExprNode[?], rhs: ExprNode[?]): Result[SqlExpr] =
      inner(lhs).flatMap(l => inner(rhs).map(r => SqlExpr.BinaryOp(op, l, r)))

    def getTableExpr(ref: ExprNode.Reference[?]): IdentifierOrSqlExpr =
      args.references.get(ref).getOrElse(Errors.failUnexpectedReference(ref, args.references.keys))

    expr match {
      case TopLevelSelect(ref, field) =>
        val parent = getTableExpr(ref) match {
          case IdentifierOrSqlExpr.Ident(id) => SqlExpr.Ident(id)
          case IdentifierOrSqlExpr.Expr(expr) => expr
        }
        Right(SqlExpr.FieldAccess(parent, field))
      case ref @ ExprNode.Reference(id, codec) =>
        val fieldNames = codec match {
          case Codec.Product(_, fields, _) => Some(fields.mapConst[String]([t] => _.name))
          case sum @ Codec.Sum(_, _) => Some(sum.reprFields.map(_.name).underlying)
          case _ => None
        }
        getTableExpr(ref) match {
          case IdentifierOrSqlExpr.Expr(expr) => Right(expr)
          case IdentifierOrSqlExpr.Ident(id) =>
            val ident = SqlExpr.Ident(id)
            Right(fieldNames.fold(SqlExpr.FieldAccess(ident, "value"))(names =>
              makeStruct(names, names.map(SqlExpr.FieldAccess(ident, _)), dialect)
            ))
        }
      case ExprNode.Select(p, field) => inner(p).map(e => SqlExpr.FieldAccess(e, field))
      case ExprNode.Literal(value, codec) => Right(literalToSqlExpr(value, codec, dialect))
      case ExprNode.Rand() => Right(SqlExpr.Function(dialect.rand, Seq.empty))
      case ExprNode.IsNaN(operand) => inner(operand).map(e => SqlExpr.Function(dialect.isNanFunction, Seq(e)))
      case ExprNode.Not(IsNone(operand)) => inner(operand).map(SqlExpr.isNotNull)
      case IsNone(operand) => inner(operand).map(SqlExpr.isNull)
      case ExprNode.Equals(Nullable(lhs), Nullable(rhs)) => binaryOp("IS NOT DISTINCT FROM", lhs, rhs)
      case ExprNode.Equals(lhs, rhs) => binaryOp("=", lhs, rhs)
      case ExprNode.LessThan(_, lhs, rhs) if isFloatingPoint(lhs.codec) =>
        dialect.floatingCompare match {
          case SqlDialect.FloatingCompare.NaNIsLargest => binaryOp("<", lhs, rhs)
          case SqlDialect.FloatingCompare.Ieee => for {
              lhsExpr <- inner(lhs)
              rhsExpr <- inner(rhs)
            } yield {
              val lhsIsNan = SqlExpr.Function(dialect.isNanFunction, Seq(lhsExpr))
              val rhsIsNan = SqlExpr.Function(dialect.isNanFunction, Seq(rhsExpr))
              SqlExpr.BinaryOp(
                "AND",
                SqlExpr.not(lhsIsNan),
                SqlExpr.BinaryOp("OR", rhsIsNan, SqlExpr.BinaryOp("<", lhsExpr, rhsExpr))
              )
            }
        }
      case ExprNode.LessThan(_, lhs, rhs) => binaryOp("<", lhs, rhs)
      case ExprNode.LessThanOrEqual(_, lhs, rhs) if isFloatingPoint(lhs.codec) =>
        dialect.floatingCompare match {
          case SqlDialect.FloatingCompare.NaNIsLargest => binaryOp("<=", lhs, rhs)
          case SqlDialect.FloatingCompare.Ieee => for {
              lhsExpr <- inner(lhs)
              rhsExpr <- inner(rhs)
            } yield {
              val lhsIsNan = SqlExpr.Function(dialect.isNanFunction, Seq(lhsExpr))
              val rhsIsNan = SqlExpr.Function(dialect.isNanFunction, Seq(rhsExpr))
              SqlExpr.BinaryOp(
                "OR",
                rhsIsNan,
                SqlExpr.BinaryOp("AND", SqlExpr.not(lhsIsNan), SqlExpr.BinaryOp("<=", lhsExpr, rhsExpr))
              )
            }
        }
      case ExprNode.LessThanOrEqual(_, lhs, rhs) => binaryOp("<=", lhs, rhs)
      case ExprNode.And(lhs, rhs) => binaryOp("AND", lhs, rhs)
      case ExprNode.Or(lhs, rhs) => binaryOp("OR", lhs, rhs)
      case ExprNode.Not(operand) => inner(operand).map(SqlExpr.not)
      case ExprNode.MakeProduct(values, codec) =>
        val names = codec.fields.mapConst[String]([t] => _.name)
        val exprs = tupleInstances(values).mapConst[Result[SqlExpr]]([t] => inner(_))
        exprs.sequence.map(makeStruct(names, _, dialect))
      case ExprNode.UpcastToIterable(operand) => operand.codec match {
          case _: Codec.Map[?, ?] => inner(operand)
              .map(sqlExpr =>
                dialect.mapSupport match {
                  case SqlDialect.MapSupport.Native(mapEntries = name) => SqlExpr.Function(name, Seq(sqlExpr))
                  case SqlDialect.MapSupport.Array => sqlExpr
                }
              )
              .map(SqlExpr.Cast(_, ToDdl.toDdlType(expr.codec, dialect.ddl, false, true).tpe))
          case ArrayCodec(_) => inner(operand)
          case codec => unreachable(s"UpcastToIterable only get codecs of Map and Iterable not $codec")
        }
      case ExprNode.MakeSome(Nullable(operand)) =>
        inner(operand).map(sqlExpr => makeStruct(Seq("value"), Seq(sqlExpr), dialect))
      case ExprNode.MakeSome(operand) => inner(operand)
      case Nullable(ExprNode.KnownNotNull(operand)) =>
        inner(operand).map(sqlExpr => SqlExpr.FieldAccess(sqlExpr, "value"))
      case ExprNode.KnownNotNull(operand) => inner(operand)
      case ExprNode.OptionToIterable(opt) => inner(opt).map(optSql =>
          val elementSql = opt.codec match {
            case Codec.Option(Codec.Option(_)) => SqlExpr.FieldAccess(optSql, "value")
            case _ => optSql
          }
          SqlExpr.Case(
            Seq((
              condition = SqlExpr.isNotNull(optSql),
              result = makeArray(Seq(elementSql), opt.codec.element, dialect)
            )),
            elseExpr = Some(makeArray(Seq(), opt.codec.element, dialect))
          )
        )
      case ExprNode.Coalesce(exprs) => exprs.map(inner).sequence.map(SqlExpr.Function("coalesce", _))
      case ExprNode.Aggregate(arg, func) =>
        inner(arg).flatMap(sqlArg => primitiveAggregate(sqlArg, func, dialect)(using arg.codec))
      case ExprNode.ScalarSubquery(subquery) =>
        unparseDs(subquery, args).flatMap(_.build()).map(SqlExpr.Subquery(_))
      case ExprNode.ExistsSubquery(ds) => unparseDs(ds, args).flatMap(_.build()).map(SqlExpr.Exists(_))
      case ExprNode.Range(start, end) => dialect.range match {
          case SqlDialect.Range.Inclusive(name, errorOnEmpty) => for {
              startExpr <- inner(start)
              endExpr <- inner(end)
            } yield {
              val endInclusive = SqlExpr.BinaryOp("-", endExpr, literalToSqlExpr(1, Codec.Int, dialect))
              val range = SqlExpr.Function(name, Seq(startExpr, endInclusive))
              if errorOnEmpty then
                SqlExpr.Case(
                  Seq((condition = SqlExpr.BinaryOp(">", endExpr, startExpr), result = range)),
                  elseExpr = Some(makeArray(Seq(), Codec.int, dialect))
                )
              else range
            }
        }
      case ExprNode.MakeSeq(values, given Codec[e]) if shouldWrapArrayElement(Codec[e], dialect.ddl) =>
        val wrappedValues = values.map(v => ExprNode.namedTuple((value = v)))
        inner(ExprNode.MakeSeq(wrappedValues, Codec[(value: e)]))
      case ExprNode.MakeSeq(values, codec) => values.map(inner).sequence.map(makeArray(_, codec, dialect))
      case ExprNode.ConcatSeq(lhs, rhs) => for {
          l <- inner(lhs)
          r <- inner(rhs)
        } yield SqlExpr.Function(dialect.arrayConcat, Seq(l, r))
      case ExprNode.MapSeq(operand, f) =>

        for {
          arr <- inner(operand)
          argName = args.aliasGen.column()
          arg = unwrapArrayElement(SqlExpr.Ident(argName), operand.codec.element, dialect)
          fExpr <- exprToSqlExpr(
            wrapArrayElement(f.expr, dialect),
            args.withReferences(f.arg -> IdentifierOrSqlExpr.Expr(arg))
          )
        } yield dialect.arrayHigherOrderFunctions match {
          case SqlDialect.ArrayHigherOrderFunctions.Subquery(makeArray, unnest) =>
            val from = From.Expr(SqlExpr.Function(unnest, Seq(arr)), argName)
            val query = Query.Select(NonEmpty(fExpr), from)
            SqlExpr.Function(makeArray, Seq(SqlExpr.Subquery(query)))
          case SqlDialect.ArrayHigherOrderFunctions.Lambda(map = mapFunction) =>
            SqlExpr.Function(mapFunction, Seq(arr, SqlExpr.LambdaFunction(arg, fExpr)))
        }
      case ExprNode.FilterSeq(operand, predicate) => for {
          arr <- inner(operand)
          argName = args.aliasGen.column()
          arg = unwrapArrayElement(SqlExpr.Ident(argName), operand.codec.element, dialect)
          predExpr <-
            exprToSqlExpr(predicate.expr, args.withReferences(predicate.arg -> IdentifierOrSqlExpr.Expr(arg)))
        } yield dialect.arrayHigherOrderFunctions match {
          case SqlDialect.ArrayHigherOrderFunctions.Subquery(makeArray, unnest) =>
            val from = From.Expr(SqlExpr.Function(unnest, Seq(arr)), argName)
            val query = Query.Select(
              select = NonEmpty(SqlExpr.Ident(argName)),
              from = Some(from),
              where = Some(predExpr),
              groupBy = Seq.empty,
              having = None,
              distinct = false
            )
            SqlExpr.Function(makeArray, Seq(SqlExpr.Subquery(query)))
          case SqlDialect.ArrayHigherOrderFunctions.Lambda(filter = filterFunction) =>
            SqlExpr.Function(filterFunction, Seq(arr, SqlExpr.LambdaFunction(arg, predExpr)))
        }
      case ExprNode.AggregateSeq(operand, onEmpty, primitive) => dialect.arrayHigherOrderFunctions match {
          case SqlDialect.ArrayHigherOrderFunctions.Subquery(makeArray, unnest) => for {
              arr <- inner(operand)
              onEmptyExpr <- inner(onEmpty)
              argName = args.aliasGen.column()
              arg = unwrapArrayElement(SqlExpr.Ident(argName), operand.codec.element, dialect)
              aggExpr <- primitiveAggregate(arg, primitive, dialect)(using operand.codec.element)
              from = From.Expr(SqlExpr.Function(unnest, Seq(arr)), argName)
              query = Query.Select(NonEmpty(coalesce(aggExpr, onEmptyExpr)), from)
            } yield SqlExpr.Subquery(query)
          case SqlDialect.ArrayHigherOrderFunctions.Lambda(aggregate = aggregate) =>
            val asFold = PrimitiveAggregateAsFold(onEmpty, primitive)(using operand.codec.element)
            for {
              arr <- inner(operand)
              initial <- inner(asFold.initial)
              merge <- lambda2(asFold.merge, args)
              finish <- lambda(asFold.finish, args)
            } yield SqlExpr.Function(aggregate, Seq(arr, initial, merge, finish))
        }
      case ExprNode.RaiseError(msg, _) => inner(msg).map(m => SqlExpr.Function(dialect.errorFunction, Seq(m)))
      case ExprNode.Cases(whenThenExpr, whenThenExprs, elseExpr) => for {
          whensSql <- (whenThenExpr +: whenThenExprs)
            .map { branch =>
              for {
                condition <- inner(branch.whenExpr)
                result <- inner(branch.thenExpr)
              } yield (condition, result)
            }
            .sequence
          elseSql <- elseExpr match {
            case ExprNode.None(_) => Right(None)
            case _ => inner(elseExpr).map(Some(_))
          }
        } yield SqlExpr.Case(whensSql, elseSql)
      case ExprNode.StartsWith(string, prefix) => for {
          str <- inner(string)
          pre <- inner(prefix)
        } yield SqlExpr.Function(dialect.startsWithFunction, Seq(str, pre))
      case ExprNode.Trim(string) => inner(string).map(str =>
          dialect.trimFunction match {
            case TrimFunction.Default(name) => SqlExpr.Function(name, Seq(str))
            case TrimFunction.Characters(name, chars) =>
              SqlExpr.Function(name, Seq(str, SqlExpr.LiteralString(chars)))
          }
        )
      case ExprNode.EndsWith(string, suffix) => for {
          str <- inner(string)
          suf <- inner(suffix)
        } yield SqlExpr.Function(dialect.endsWithFunction, Seq(str, suf))
      case ExprNode.ConcatString(strings) =>
        sequence(strings.map(inner)).map(parts => SqlExpr.Function("concat", parts))
      case ExprNode.Split(string, delimiter) => for {
          str <- inner(string)
          del <- inner(delimiter)
        } yield dialect.splitFunction match {
          case SplitFunction.Java(name) =>
            val quoted = SqlExpr.Function(
              "concat",
              Seq(
                SqlExpr.LiteralString("\\Q"),
                SqlExpr.Function(
                  "replace",
                  Seq(del, SqlExpr.LiteralString("\\E"), SqlExpr.LiteralString("\\E\\\\E\\Q"))
                ),
                SqlExpr.LiteralString("\\E")
              )
            )
            SqlExpr.Function(name, Seq(str, quoted))
          case SplitFunction.NonRegex(name) =>
            def isEmpty(e: SqlExpr) = SqlExpr.BinaryOp("=", e, SqlExpr.LiteralString(""))
            val specialCase = SqlExpr.BinaryOp("AND", isEmpty(del), SqlExpr.not(isEmpty(str)))
            val split = SqlExpr.Function(name, Seq(str, del))
            SqlExpr.Case(
              Seq((
                condition = specialCase,
                result = SqlExpr.Function(
                  "array_concat",
                  Seq(split, makeArray(Seq(SqlExpr.LiteralString("")), Codec.string, dialect))
                )
              )),
              elseExpr = Some(split)
            )
        }
      case ExprNode.SizeSeq(operand) =>
        inner(operand).map(arr => SqlExpr.Function(dialect.arraySize, Seq(arr)))
      case ExprNode.DistinctSeq(operand) => inner(operand).map(arr =>
          dialect.arrayDistinct match {
            case SqlDialect.ArrayDistinct.Function(name) => SqlExpr.Function(name, Seq(arr))
            case SqlDialect.ArrayDistinct.Subquery(makeArray, unnest) =>
              val element = args.aliasGen.column()
              val elementIdent = SqlExpr.Ident(element)

              val unnestFrom = From.Expr(SqlExpr.Function(unnest, Seq(arr)), element)
              val selectQuery = Query.Select(
                select = NonEmpty(elementIdent),
                from = Some(unnestFrom),
                where = None,
                groupBy = Seq.empty,
                having = None,
                distinct = true
              )
              SqlExpr.Function(makeArray, Seq(SqlExpr.Subquery(selectQuery)))

          }
        )
      case ExprNode.ElementSeq(array, index) =>
        val adjusted =
          if dialect.arrayElement.zeroIndexed then index
          else ExprNode.Add[Int](summon, index, ExprNode.Literal(1))
        val element = for {
          arr <- inner(array)
          idx <- inner(adjusted)
        } yield dialect.arrayElement match {
          case SqlDialect.ArrayElement.Braces => SqlExpr.Index(arr, idx)
          case SqlDialect.ArrayElement.Function(name) => SqlExpr.Function(name, Seq(arr, idx))
        }
        element.map(unwrapArrayElement(_, array.codec.element, dialect))
      case ExprNode.Add(_, lhs, rhs) => binaryOp("+", lhs, rhs)
      case ExprNode.Quotient(CompatibleIntegral(), lhs, rhs) => for {
          lhs <- inner(lhs)
          rhs <- inner(rhs)
        } yield SqlExpr.Cast(
          SqlExpr.Function("div", Seq(lhs, rhs)),
          ToDdl.toDdlType(expr.codec, dialect.ddl, true, true).tpe
        )
      case ExprNode.Quotient(integral, _, _) =>
        Left(DatasetToSqlError.RequiresUdfCapability(s"Quotient uses custom integral instance $integral"))
      case ExprNode.Cast(value, _) => inner(value).map(cast(_, expr.codec, dialect))
      case ExprNode.TryCast(value, canTryCast) =>
        inner(value).map(tryCast(_, value.codec, canTryCast.codec, dialect))
      case ExprNode.TimestampToMicros(value) =>
        inner(value).map(v => SqlExpr.Function(dialect.extractTimestampMicros, Seq(v)))
      case ExprNode.MicrosToTimestamp(value) => dialect.makeTimestamp match {
          case SqlDialect.MakeTimestamp.Function(name) =>
            inner(value).map(v => SqlExpr.Function(name, Seq(v)))
        }
      case ExprNode.DurationToMicros(value) => dialect.makeDuration match {
          case SqlDialect.MakeDuration.DiffBigInt => inner(value)
          case SqlDialect.MakeDuration.Cast => for {
              sqlExpr <- inner(value)
              asDecimal =
                SqlExpr.Cast(sqlExpr, ToDdl.toDdlType(Codec[Decimal[38, 6]], dialect.ddl, false, true).tpe)
              micros = SqlExpr.BinaryOp("*", asDecimal, literalToSqlExpr(1000000, Codec.Int, dialect))
            } yield SqlExpr.Cast(micros, ToDdl.toDdlType(Codec.Long, dialect.ddl, false, true).tpe)
        }
      case ExprNode.MicrosToDuration(value) => dialect.makeDuration match {
          case SqlDialect.MakeDuration.DiffBigInt => inner(value)
          case SqlDialect.MakeDuration.Cast => for {
              sqlExpr <- inner(value)
              asDecimal =
                SqlExpr.Cast(sqlExpr, ToDdl.toDdlType(Codec[Decimal[38, 6]], dialect.ddl, false, true).tpe)
              duration = SqlExpr.BinaryOp("/", asDecimal, literalToSqlExpr(1000000, Codec.Int, dialect))
            } yield SqlExpr.Cast(duration, ToDdl.toDdlType(Codec[Duration], dialect.ddl, false, true).tpe)
        }
      case ExprNode.DateToDays(value) =>
        inner(value).map(v => SqlExpr.Function(dialect.extractDateDays, Seq(v)))
      case ExprNode.DaysToDate(value) => dialect.makeDate match {
          case SqlDialect.MakeDate.Function(name) => inner(value).map(v => SqlExpr.Function(name, Seq(v)))
        }
      case ExprNode.Udf(_, _, _) =>
        Left(DatasetToSqlError.RequiresUdfCapability("UDFs are not supported on SQL"))
      case ExprNode.ToRepr(expr, _) => inner(expr)
      case ExprNode.FromRepr(expr, _) => inner(expr)
      case node @ ExprNode.MakeMap(entries) =>
        val codecs = node.codec.element.elements
        for (pairs <- inner(entries)) yield makeMap(pairs, codecs(0), codecs(1), dialect)
      case ExprNode.MapEntries(map) => inner(map).map(sqlExpr =>
          dialect.mapSupport match {
            case SqlDialect.MapSupport.Native(mapEntries = name) => SqlExpr.Function(name, Seq(sqlExpr))
            case SqlDialect.MapSupport.Array => sqlExpr
          }
        )
      /* TODO: in the future this might be delegated to Expr[Seq[(key: K, value: V)]] api. For now, we are
       * missing the necessary expressiveness in the API to do that, but this might be improved in the future. */
      case ExprNode.MapGet(map, key) => for {
          mapExpr <- inner(map)
          keyExpr <- inner(key)
        } yield dialect.mapSupport match {
          case SqlDialect.MapSupport.Native(mapGet = getName, mapContains = containsName) =>
            val value = SqlExpr.Function(getName, Seq(mapExpr, keyExpr))
            map.codec match {
              case Codec.Map(_, Codec.Option(_)) => SqlExpr.Case(Seq((
                  SqlExpr.Function(containsName, Seq(mapExpr, keyExpr)),
                  makeStruct(Seq("value"), Seq(value), dialect)
                )))
              case _ => value
            }
          case SqlDialect.MapSupport.Array => dialect.arrayHigherOrderFunctions match {
              case SqlDialect.ArrayHigherOrderFunctions.Subquery(_, unnest) =>
                val entryName = args.aliasGen.column()
                val entryIdent = SqlExpr.Ident(entryName)
                val from = From.Expr(SqlExpr.Function(unnest, Seq(mapExpr)), entryName)
                val value = SqlExpr.FieldAccess(entryIdent, "value")
                val select = map.codec match {
                  case Codec.Map(_, Codec.Option(_)) => makeStruct(Seq("value"), Seq(value), dialect)
                  case _ => value
                }
                val query = Query.Select(
                  select = NonEmpty(select),
                  from = Some(from),
                  where = Some(SqlExpr.BinaryOp("=", SqlExpr.FieldAccess(entryIdent, "key"), keyExpr)),
                  groupBy = Seq.empty,
                  having = None,
                  distinct = false,
                  limit = None
                )
                SqlExpr.Subquery(query)
              case SqlDialect.ArrayHigherOrderFunctions.Lambda(map = _) => unreachable(
                  "MapGet with Array map support is not supported for lambda-based higher order functions"
                )
            }
        }
      case ExprNode.None(codec) =>
        val ddlType = ToDdl.toDdlType(codec, dialect.ddl, notNull = false, supportsNotNull = true).tpe
        Right(SqlExpr.Cast(SqlExpr.LiteralNull, ddlType))
      case ExprNode.ToJson(value) =>
        val hasNestedArray = Codec
          .iterate(value.codec)
          .exists {
            case Codec.Seq(CollectionOrNullableCollectionCodec(_)) => true
            case _ => false
          }
        if !dialect.ddl.supportsArrayAsArrayElement && hasNestedArray then {
          Left(DatasetToSqlError.NotImplemented(
            "toJson with nested arrays and no nested array support is not implemented"
          ))
        } else {
          val maybeOptions = dialect.toJson.options.map(optionsToSqlExpr)
          inner(value).map(v => SqlExpr.Function(dialect.toJson.functionName, Seq(v) ++ maybeOptions))
        }
      case ExprNode.FromJson(json, codec) => dialect.fromJson match {
          case SqlDialect.FromJsonSupport.Parser(fromJson, options) => for {
              jsonExpr <- inner(json)
              ddlType = ToDdl.toDdlType(codec, dialect.ddl, notNull = false, supportsNotNull = true).tpe
              optionsExpr = optionsToSqlExpr(options)
            } yield SqlExpr.Function(fromJson, Seq(jsonExpr, ddlAsSqlString(ddlType), optionsExpr))
          case extractors @ SqlDialect.FromJsonSupport.Extractors(_, _, _) =>
            inner(json).flatMap(expr => fromJson(extractors, expr, codec, args))
        }
    }
  }
  inner(fullExpr)
}

private def optionsToSqlExpr(options: Map[String, String]): SqlExpr =
  SqlExpr.Function(
    "map",
    options.flatMap((key, value) => Seq(SqlExpr.LiteralString(key), SqlExpr.LiteralString(value))).toSeq
  )
private def mapSeq(arr: SqlExpr, argName: String, fExpr: SqlExpr, dialect: SqlDialect): SqlExpr =
  dialect.arrayHigherOrderFunctions match {
    case SqlDialect.ArrayHigherOrderFunctions.Subquery(makeArray, unnest) =>
      val from = From.Expr(SqlExpr.Function(unnest, Seq(arr)), argName)
      val query = Query.Select(NonEmpty(fExpr), from)
      SqlExpr.Function(makeArray, Seq(SqlExpr.Subquery(query)))
    case SqlDialect.ArrayHigherOrderFunctions.Lambda(map = mapFunction) =>
      SqlExpr.Function(mapFunction, Seq(arr, SqlExpr.LambdaFunction(SqlExpr.Ident(argName), fExpr)))
  }

private def aggregateSeq[T, R](
    arr: SqlExpr,
    elementCodec: Codec[T],
    onEmpty: ExprNode[R],
    primitive: PrimitiveAggregate[T, R] & ExprNode.AggregateSeq.SupportedAggregates,
    args: UnparserArgs
): Result[SqlExpr] =
  args.dialect.arrayHigherOrderFunctions match {
    case SqlDialect.ArrayHigherOrderFunctions.Subquery(makeArray, unnest) => for {
        onEmptyExpr <- exprToSqlExpr(onEmpty, args)
        argName = args.aliasGen.column()
        arg = unwrapArrayElement(SqlExpr.Ident(argName), elementCodec, args.dialect)
        aggExpr <- primitiveAggregate(arg, primitive, args.dialect)(using elementCodec)
        from = From.Expr(SqlExpr.Function(unnest, Seq(arr)), argName)
        query = Query.Select(NonEmpty(coalesce(aggExpr, onEmptyExpr)), from)
      } yield SqlExpr.Subquery(query)
    case SqlDialect.ArrayHigherOrderFunctions.Lambda(aggregate = aggregate) =>
      val asFold = PrimitiveAggregateAsFold(onEmpty, primitive)(using elementCodec)
      for {
        initial <- exprToSqlExpr(asFold.initial, args)
        merge <- lambda2(asFold.merge, args)
        finish <- lambda(asFold.finish, args)
      } yield SqlExpr.Function(aggregate, Seq(arr, initial, merge, finish))
  }

private def cast(expr: SqlExpr, to: Codec[?], dialect: SqlDialect): SqlExpr =
  SqlExpr.Cast(expr, ToDdl.toDdlType(to, dialect.ddl, false, true).tpe)

private def tryCast(expr: SqlExpr, from: Codec[?], to: Codec[?], dialect: SqlDialect): SqlExpr =
  SqlExpr
    .Cast(dialect.tryCast, expr, ToDdl.toDdlType(to, dialect.ddl, false, true).tpe)
    .pipe(addControlCharCheckIfNeeded(expr, _, from, dialect))
    .pipe(addIntRangeCheckIfNeeded(_, to, dialect))
    .pipe(addDecimalRangeAndRoundingIfNeeded(_, to, dialect))

private def jsonPath(path: Seq[String]): SqlExpr = {
  def needEscaping(part: String): Boolean =
    part.isEmpty || part.head.isDigit || part.exists(c => !c.isLetterOrDigit && c != '_')
  def escape(part: String): String = if needEscaping(part) then s"\"$part\"" else part
  if path.isEmpty then SqlExpr.LiteralString("$")
  else SqlExpr.LiteralString("$." + path.map(escape).mkString("."))
}

private def fromJson[T](
    extractors: SqlDialect.FromJsonSupport.Extractors,
    fullJson: SqlExpr,
    codec: Codec[T],
    args: UnparserArgs
): Result[SqlExpr] = {
  val dialect = args.dialect

  def and(lhs: Option[SqlExpr], rhs: Option[SqlExpr]): Option[SqlExpr] =
    lhs.zip(rhs).map(SqlExpr.BinaryOp("AND", _, _)).orElse(lhs).orElse(rhs)

  def isNotJsonNull(expr: SqlExpr): SqlExpr = SqlExpr.BinaryOp("!=", expr, SqlExpr.LiteralString("null"))

  def nestedOption[T](
      element: Codec.Option[T],
      json: SqlExpr,
      path: Seq[String]
  ): Result[(value: SqlExpr, isValid: Option[SqlExpr])] = {
    given Codec[T] = element.element
    go(json, Codec[Option[(value: Option[T])]], path, nullable = true)
  }

  def go[A](
      json: SqlExpr,
      codec: Codec[A],
      path: Seq[String],
      nullable: Boolean
  ): Result[(value: SqlExpr, isValid: Option[SqlExpr])] =
    codec match {
      case _: Codec.Primitive[A] =>
        val extracted = SqlExpr.Function(extractors.extractScalar, Seq(json, jsonPath(path)))
        val casted =
          if codec == Codec.String then extracted else tryCast(extracted, Codec.String, codec, dialect)
        val valid = Option.when(!nullable)(SqlExpr.isNotNull(casted))
        Right((casted, valid))
      case Codec.Seq(element) =>
        val arrayOfJson = SqlExpr.Function(extractors.extractArray, Seq(json, jsonPath(path)))
        val validArray = Option.when(!nullable)(SqlExpr.isNotNull(arrayOfJson))
        val argName = args.aliasGen.column()
        go(unwrapArrayElement(SqlExpr.Ident(argName), element, dialect), element, Seq.empty, nullable = false)
          .flatMap { (elementExpr, maybeElementValid) =>
            val extractedArray = mapSeq(arrayOfJson, argName, elementExpr, dialect)
            val maybeAllElementsValid = maybeElementValid
              .map(elementValid => mapSeq(arrayOfJson, argName, elementValid, dialect))
              .map(arrayOfElementIsValid =>
                aggregateSeq(
                  arrayOfElementIsValid,
                  Codec.Boolean,
                  ExprNode.Literal(true),
                  PrimitiveAggregate.BoolAnd(),
                  args
                )
              )
            maybeAllElementsValid.fold(Right(
              (extractedArray, validArray)
            ))(_.map(allValid => (extractedArray, and(validArray, Some(allValid)))))
          }
      case Codec.Option(element @ Codec.Option(_)) => nestedOption(element, json, path)
      case Codec.Option(element) => go(json, element, path, nullable = true)
      case Codec.Product(_, fields, _) => for {
          (exprs, fieldsValid) <- fields.foldLeft0[Result[(Seq[SqlExpr], Option[SqlExpr])]](Right(
            (Seq.empty[SqlExpr], Option.empty[SqlExpr])
          ))([t] =>
            (acc, field) =>
              for {
                (fields, isValid) <- acc
                (fieldValue, fieldIsValid) <- go(json, field.codec, path :+ field.name, nullable = false)
              } yield (fields :+ fieldValue, and(isValid, fieldIsValid))
          )
          productIsNotNull =
            isNotJsonNull(SqlExpr.Function(extractors.extractObject, Seq(json, jsonPath(path))))
          makeProduct = makeStruct(fields.mapConst([t] => _.name), exprs, dialect)
        } yield
          if nullable then
            (SqlExpr.Case(Seq((condition = productIsNotNull, result = makeProduct))), fieldsValid)
          else (makeProduct, and(Some(productIsNotNull), fieldsValid))

      case Codec.Map(given Codec[k], given Codec[v]) => for {
          (mapAsArray, isValid) <- go(json, Codec[Seq[(key: k, value: v)]], path, nullable)
        } yield (makeMap(mapAsArray, Codec[k], Codec[v], dialect), isValid)
      case Codec.FromInjection(_, to) => go(json, to, path, nullable)
    }
  for {
    (value, isValid) <- go(fullJson, codec, Seq.empty, nullable = false)
  } yield isValid.fold(value)(valid => SqlExpr.Case(Seq((condition = valid, result = value))))
}

private def ddlAsSqlString(ddl: DdlType): SqlExpr = {
  val writer = new java.io.StringWriter()
  DdlWriter(writer, pretty = false).write(ddl, 0)
  SqlExpr.LiteralString(writer.toString)
}

private def addControlCharCheckIfNeeded(
    fromExpr: SqlExpr,
    casted: SqlExpr,
    fromCodec: Codec[?],
    dialect: SqlDialect
): SqlExpr =
  fromCodec match {
    case Codec.String if dialect.tryCastTrimsControlChars =>
      SqlExpr.Case(Seq((
        condition =
          SqlExpr.not(SqlExpr.Function(dialect.regexp, Seq(fromExpr, SqlExpr.LiteralString("\\p{Cc}")))),
        result = casted
      )))
    case _ => casted
  }

private def rangeCheck(value: SqlExpr, min: SqlExpr, max: SqlExpr): SqlExpr =
  SqlExpr.Case(Seq((
    condition =
      SqlExpr.BinaryOp("AND", SqlExpr.BinaryOp(">=", value, min), SqlExpr.BinaryOp("<=", value, max)),
    result = value
  )))

private def addIntRangeCheckIfNeeded(expr: SqlExpr, to: Codec[?], dialect: SqlDialect): SqlExpr = {
  def range[T](codec: Codec[T]): Option[(SqlExpr, SqlExpr)] = {
    val (min, max) = codec match {
      case Codec.Byte => (Byte.MinValue.toLong, Byte.MaxValue.toLong)
      case Codec.Short => (Short.MinValue.toLong, Short.MaxValue.toLong)
      case Codec.Int => (Int.MinValue.toLong, Int.MaxValue.toLong)
      case _ => return None
    }
    Some((literalToSqlExpr(min, Codec.Long, dialect), literalToSqlExpr(max, Codec.Long, dialect)))
  }
  (dialect.intergerSupport, range(to)) match {
    case (IntegerSupport.OnlyBigInt, Some(min, max)) => rangeCheck(expr, min, max)
    case _ => expr
  }
}

private def addDecimalRangeAndRoundingIfNeeded(expr: SqlExpr, to: Codec[?], dialect: SqlDialect): SqlExpr =
  (dialect.ddl.decimal, to) match {
    case (DecimalSupport.BigQuery(false), Codec.Decimal(precision, scale)) =>
      def toSqlExpr(v: BigInt): SqlExpr = {
        val decimal = Decimal[MaxPrecision, 0](BigDecimal(v)).getOrElse(unreachable(
          "Limits for all decimals are representable as Decimal[MaxPrecision, 0]"
        ))
        literalToSqlExpr(decimal, Codec.Decimal(38, 0), dialect)
      }
      val bigIntMax = BigInt(10).pow(precision - scale) - 1
      val max = toSqlExpr(bigIntMax)
      val min = toSqlExpr(-bigIntMax)
      val checked = rangeCheck(expr, min, max)
      SqlExpr.Function("round", Seq(checked, literalToSqlExpr(scale, Codec.Int, dialect)))
    case _ => expr
  }

private def coalesce(exprs: SqlExpr*): SqlExpr = SqlExpr.Function("coalesce", exprs)

private def makeStruct(names: Seq[String], fields: Seq[SqlExpr], dialect: SqlDialect): SqlExpr =
  assert(names.length == fields.length)
  dialect.makeStruct match {
    case SqlDialect.MakeStruct.Function(name) =>
      if names.length == 0 then {
        // Empty structs are generally poorly supported, so we create a dummy fields instead
        // In the long run we should probably disallow codec for empty structs.
        SqlExpr.Function(name, Seq(SqlExpr.LiteralString(Forbidden.column), SqlExpr.LiteralNull))
      } else {
        val nameExprs = names.map(n => SqlExpr.LiteralString(n))
        val args = nameExprs.zip(fields).flatMap { case (n, e) => Seq(n, e) }
        SqlExpr.Function(name, args)
      }
    case SqlDialect.MakeStruct.FunctionAndAlias(name) =>
      if names.length == 0 then {
        // Empty structs are generally poorly supported, so we create a dummy fields instead
        // In the long run we should probably disallow codec for empty structs.
        SqlExpr.Function(name, Seq(SqlExpr.As(SqlExpr.LiteralNull, Forbidden.column)))
      } else {
        val args = fields.zip(names).map { case (e, n) => SqlExpr.As(e, n) }
        SqlExpr.Function(name, args)
      }
  }

private def isFloatingPoint(codec: Codec[?]): Boolean =
  codec match {
    case Codec.Float | Codec.Double => true
    case _ => false
  }

private def makeArray[T](values: Seq[SqlExpr], element: Codec[T], dialect: SqlDialect): SqlExpr = {
  val array = dialect.makeArray match {
    case SqlDialect.MakeArray.Function(name) => SqlExpr.Function(name, values)
    case SqlDialect.MakeArray.Brackets => SqlExpr.Brackets(values)
  }
  if values.isEmpty then
    given Codec[T] = element
    SqlExpr.Cast(
      array,
      ToDdl.toDdlType(Codec[Seq[T]], dialect.ddl, notNull = true, supportsNotNull = false).tpe
    )
  else array
}

private def makeMap[K, V](pairs: SqlExpr, key: Codec[K], value: Codec[V], dialect: SqlDialect): SqlExpr =
  dialect.mapSupport match {
    case SqlDialect.MapSupport.Native(makeMap = name) => SqlExpr.Function(name, Seq(pairs))
    case SqlDialect.MapSupport.Array =>
      given Codec[K] = key
      given Codec[V] = value
      val ddl = ToDdl.toDdlType(Codec[Map[K, V]], dialect.ddl, notNull = true, supportsNotNull = false).tpe
      SqlExpr.Cast(pairs, ddl)
  }

object CompatibleSum {
  def unapply[T](magnet: SumMagnet[T]): Boolean =
    magnet match {
      case SumMagnet.AsLong(CompatibleNumeric()) | SumMagnet.AsDouble(CompatibleNumeric()) | SumMagnet
            .Nullable(CompatibleSum()) | SumMagnet.AsDecimal() => true
      case _ => false
    }
}

object CompatibleNumeric {
  def unapply[T](numeric: Numeric[T]): Boolean =
    numeric match {
      case CompatibleIntegral() => true
      case CompatibleFractional() => true
      case _ => false
    }
}

object CompatibleIntegral {
  def unapply[T](numeric: Numeric[T]): Boolean =
    numeric match {
      case Numeric.ByteIsIntegral => true
      case Numeric.ShortIsIntegral => true
      case Numeric.IntIsIntegral => true
      case Numeric.LongIsIntegral => true
      case _ => false
    }
}

object CompatibleFractional {
  def unapply[T](numeric: Numeric[T]): Boolean =
    numeric match {
      case Numeric.FloatIsFractional => true
      case Numeric.DoubleIsFractional => true
      case _ => false
    }
}

private def simpleAggregate(name: String, arg: SqlExpr): Result[SqlExpr] =
  Right(SqlExpr.Function(name, Seq(arg)))

private def primitiveAggregate[T: Codec](
    arg: SqlExpr,
    agg: PrimitiveAggregate[T, ?],
    dialect: SqlDialect
): Result[SqlExpr] = {
  def simple(name: String): Result[SqlExpr] = simpleAggregate(name, arg)
  def binaryAgg(name: String): Result[SqlExpr] =
    Right(SqlExpr.Function(name, Seq(SqlExpr.FieldAccess(arg, "_1"), SqlExpr.FieldAccess(arg, "_2"))))
  agg match {
    case PrimitiveAggregate.Count() => simple("count")
    case PrimitiveAggregate.BoolAnd() => simple(dialect.boolAndFunction)
    case PrimitiveAggregate.BoolOr() => simple(dialect.boolOrFunction)
    case agg @ (PrimitiveAggregate.Min(_)) if isFloatingPoint(Codec[T]) =>
      val function = "min"
      dialect.floatingAggregate match {
        case SqlDialect.FloatingAggregate.NaNIsLargest => simple(function)
        case SqlDialect.FloatingAggregate.NaNIsSmallestAndLargest =>
          val isNan = SqlExpr.Function(dialect.isNanFunction, Seq(arg))
          val minWithoutNan =
            SqlExpr.Function(function, Seq(SqlExpr.Case(Seq((condition = SqlExpr.not(isNan), result = arg)))))
          Right(coalesce(minWithoutNan, literalToSqlExpr(Float.NaN, Codec.Float, dialect)))
      }
    case PrimitiveAggregate.Max(_) => simple("max")
    case PrimitiveAggregate.Min(_) => simple("min")
    case PrimitiveAggregate.MaxBy(_) => binaryAgg("max_by")
    case PrimitiveAggregate.MinBy(_) => binaryAgg("min_by")
    case PrimitiveAggregate.Sum(CompatibleSum()) => simple("sum")
    case PrimitiveAggregate.Sum(magnet) =>
      Left(DatasetToSqlError.RequiresUdfCapability(s"Sum uses custom $magnet"))
    case PrimitiveAggregate.Sum(CompatibleNumeric()) => simple("sum")
    case PrimitiveAggregate.CountSome() => simple("count")
    case PrimitiveAggregate.Collect() =>
      if shouldWrapArrayElement(Codec[T], dialect.ddl) then
        simpleAggregate(dialect.collectFunction, wrapArrayElement(arg, dialect))
      else simple(dialect.collectFunction)
    case PrimitiveAggregate.Reduce(_) =>
      Left(DatasetToSqlError.RequiresUdfCapability("Reduce is not supported on SQL"))
  }
}

private def literalProductToFields[T](
    value: T,
    prod: Codec.Product[T],
    args: UnparserArgs
): Result[NonEmpty[Seq[SqlExpr]]] =
  prod
    .fields
    .foldLeft(value)(Seq.empty[Result[SqlExpr]])([t] =>
      (acc, f, elem) => acc :+ exprToSqlExpr(ExprNode.Literal.create(elem, f.codec), args)
    )
    .sequence
    .map(NonEmpty.from(_).getOrElse(NonEmpty(SqlExpr.LiteralNull)))

private def literalToSqlExpr[T](value: T, codec: Codec.Primitive[T], dialect: SqlDialect): SqlExpr = {
  def floatingPoint(value: Double, sqlType: DdlType, isFinite: Boolean): SqlExpr =
    if isFinite then SqlExpr.Cast(SqlExpr.LiteralNumeric(value.toString), sqlType)
    else SqlExpr.Cast(SqlExpr.LiteralString(value.toString), sqlType)

  codec match {
    case Codec.Float =>
      /* For engines like BigQuery where literals are parsed as double the result is more accurate if
       * converted to double before being parsed. For engines like Spark where literals are parsed as decimal
       * this does not matter. */
      floatingPoint(value.toDouble, ToDdl.toNullableDdlType(codec, dialect.ddl), isFinite = value.isFinite)
    case Codec.Double =>
      floatingPoint(value, ToDdl.toNullableDdlType(codec, dialect.ddl), isFinite = value.isFinite)
    case Codec.Byte | Codec.Short | Codec.Int | Codec.Long =>
      val sqlValue = SqlExpr.LiteralNumeric(value.toString)
      SqlExpr.Cast(sqlValue, ToDdl.toNullableDdlType(codec, dialect.ddl))
    case Codec.Decimal(_, _) =>
      val sqlValue = SqlExpr.LiteralString(value.toString)
      SqlExpr.Cast(sqlValue, ToDdl.toNullableDdlType(codec, dialect.ddl))
    case Codec.String => SqlExpr.LiteralString(value.toString)
    case Codec.Bytes => dialect.binaryLiteral match {
        case SqlDialect.BinaryLiteral.HexString => SqlExpr.LiteralHexString(value)
      }
    case Codec.Boolean => SqlExpr.LiteralBool(value)
    case Codec.TimestampMicros => dialect.makeTimestamp match {
        case SqlDialect.MakeTimestamp.Function(name) =>
          SqlExpr.Function(name, Seq(literalToSqlExpr(value.toMicros, Codec.Long, dialect)))
      }
    case Codec.DurationMicros => dialect.makeDuration match {
        case SqlDialect.MakeDuration.DiffBigInt => SqlExpr.BinaryOp(
            "-",
            literalToSqlExpr(value.toMicros, Codec.Long, dialect),
            literalToSqlExpr(0L, Codec.Long, dialect)
          )
        case SqlDialect.MakeDuration.Cast =>
          val valueAsDecimal = Decimal[38, 6](BigDecimal(value.toMicros) / 1_000_000).getOrElse(unreachable(
            s"All Long values should fit in Decimal[38,6] failed for $value"
          ))
          val valueAsExpr = literalToSqlExpr(valueAsDecimal, Codec.Decimal(38, 6), dialect)
          SqlExpr.Cast(valueAsExpr, ToDdl.toNullableDdlType(Codec.DurationMicros, dialect.ddl))
      }
    case Codec.Date => dialect.makeDate match {
        case SqlDialect.MakeDate.Function(name) =>
          SqlExpr.Function(name, Seq(literalToSqlExpr(value.daysSinceEpoch, Codec.Int, dialect)))
      }
  }
}

/** Generate a dummy value for the given codec. This is used to create a empty
  * relation with the correct schema.
  *
  * Because Sparks DDL type description can not represent `NOT NULL` in all
  * cases we use an actuall dummy value and let spark infer the nullability.
  */
private def dummyValue[T](
    codec: Codec[T],
    args: UnparserArgs
): Result[(NonEmpty[Seq[SqlExpr]], NonEmpty[Seq[String]])] =
  codec match {
    case Codec.Product(_, _, Some(_)) => Right((NonEmpty(SqlExpr.LiteralNull), NonEmpty(Forbidden.column)))
    case Codec.Product(_, fields, _) =>
      val names = NonEmpty.from(fields.mapConst[String]([t] => _.name))
      fields
        .mapConst([t] => f => dummyExpr(f.codec, args))
        .sequence
        .map(NonEmpty.from(_))
        .map(exprs =>
          (exprs, names) match {
            case (Some(e), Some(n)) => (e, n)
            case _ => unreachable("Only singletons should have no fields")
          }
        )
    case sum @ Codec.Sum(_, _) =>
      val names = sum.reprFields.map(_.name)
      sum.reprFields.map(f => dummyExpr(f.codec, args)).sequence.map((_, names))
    case other => dummyExpr(other, args).map(expr => (NonEmpty(expr), NonEmpty("value")))
  }

private def dummyExpr[T](codec: Codec[T], args: UnparserArgs): Result[SqlExpr] =
  exprToSqlExpr(NotNullNonEmptyDummyLiteral.create(codec), args)
