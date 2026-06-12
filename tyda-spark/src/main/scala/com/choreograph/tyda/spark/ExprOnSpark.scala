package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.aggregate
import org.apache.spark.sql.functions.array
import org.apache.spark.sql.functions.array_distinct
import org.apache.spark.sql.functions.call_function
import org.apache.spark.sql.functions.coalesce
import org.apache.spark.sql.functions.concat
import org.apache.spark.sql.functions.date_from_unix_date
import org.apache.spark.sql.functions.element_at
import org.apache.spark.sql.functions.endswith
import org.apache.spark.sql.functions.filter
import org.apache.spark.sql.functions.from_json
import org.apache.spark.sql.functions.isnan
import org.apache.spark.sql.functions.length
import org.apache.spark.sql.functions.lit
import org.apache.spark.sql.functions.map_contains_key
import org.apache.spark.sql.functions.map_entries
import org.apache.spark.sql.functions.map_from_entries
import org.apache.spark.sql.functions.raise_error
import org.apache.spark.sql.functions.rand
import org.apache.spark.sql.functions.replace
import org.apache.spark.sql.functions.sequence
import org.apache.spark.sql.functions.size
import org.apache.spark.sql.functions.startswith
import org.apache.spark.sql.functions.struct
import org.apache.spark.sql.functions.timestamp_micros
import org.apache.spark.sql.functions.to_json
import org.apache.spark.sql.functions.transform
import org.apache.spark.sql.functions.trim
import org.apache.spark.sql.functions.unix_date
import org.apache.spark.sql.functions.unix_micros
import org.apache.spark.sql.functions.when

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Errors
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.rewrite.ArrayCodec
import com.choreograph.tyda.rewrite.IsNone
import com.choreograph.tyda.rewrite.Nullable
import com.choreograph.tyda.rewrite.PrimitiveAggregateAsFold
import com.choreograph.tyda.rewrite.SparkJsonCompatability
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.spark.CodecToCatalystType.catalystType
import com.choreograph.tyda.spark.PrimitiveAggregateOnSpark.CompatibleIntegral
import com.choreograph.tyda.unreachable

private[spark] object ExprOnSpark {

  /** Convert [[Expr]] into the corresponding Spark Column.
    *
    * All column references will be resolved to the provided Dataset.
    */
  def resolved[T: Codec](ds: Dataset[T], compiled: CompiledExpr[T, ?])(using SparkSession): Column =
    new ExprOnSpark(Map(compiled.arg -> ColumnFactory(ds))).convert(compiled.expr)

  /** Convert [[Expr]] into the corresponding Spark Column.
    *
    * All column references will be resolved through the provided ColumnFactory.
    */
  def resolved[T](cf: ColumnFactory[T], compiled: CompiledExpr[T, ?])(using SparkSession): Column =
    new ExprOnSpark(Map(compiled.arg -> cf)).convert(compiled.expr)

  /** Convert [[Expr]] into the corresponding Spark Column.
    *
    * All column references will be resolved to the provided Dataset.
    */
  def resolved[T1, T2](cf1: ColumnFactory[T1], cf2: ColumnFactory[T2], compiled: CompiledExpr2[T1, T2, ?])(
      using SparkSession
  ): Column = new ExprOnSpark(Map(compiled.arg1 -> cf1, compiled.arg2 -> cf2)).convert(compiled.expr)

  /** Convert a [[CompiledAggregateExpr]] into the corresponding Spark Column.
    *
    * All column references will be resolved to provided ColumnFactory.
    */
  def resolved[T, R](cf: ColumnFactory[T], compiled: CompiledAggregateExpr[T, R])(using
      SparkSession
  ): Column = new ExprOnSpark(Map(compiled.arg -> cf)).convert(compiled.expr)

  /** Convert [[Expr]] into the corresponding Spark Column.
    *
    * All column references will be unresolved. So care needs to be taken to
    * avoid ambiguous references.
    */
  def unresolved[T: Codec](compiled: CompiledExpr[T, ?])(using SparkSession): Column =
    new ExprOnSpark(Map(compiled.arg -> ColumnFactory.unresolved)).convert(compiled.expr)

  private def wrapNestedSome(value: Column): Column = struct(value.as("value"))
  private val jsonOptions: Map[String, String] = Map("mode" -> "PERMISSIVE") ++ DatasetOnSpark.jsonOptions
}

/** Contains logic for converting a Expr[T] into a Spark Column. */
private class ExprOnSpark[T](cfs: Map[ExprNode.Reference[?], ColumnFactory[?]]) {
  import ExprOnSpark.{wrapNestedSome, jsonOptions}

  private def literal[T](value: T, codec: Codec.Primitive[T])(using SparkSession): Column =
    codec match {
      case Codec.Boolean | Codec.Byte | Codec.Short | Codec.Int | Codec.Long | Codec.Float | Codec.Double |
          Codec.String | Codec.Bytes => lit(value)
      case Codec.TimestampMicros =>
        convert(ExprNode.MicrosToTimestamp(ExprNode.Literal(value.toMicros, Codec.Long)))
      case Codec.Date => convert(ExprNode.DaysToDate(ExprNode.Literal(value.daysSinceEpoch, Codec.Int)))
      case Codec.DurationMicros => lit(value.toMicros)
      case Codec.Decimal(_, _) => lit(value).cast(catalystType(codec))
    }

  private def cfFromRef(ref: ExprNode.Reference[?]): ColumnFactory[?] =
    cfs.get(ref).getOrElse(Errors.failUnexpectedReference(ref, cfs.keys))

  private def transformArgs[T](seq: ExprNode[Seq[T]], compiled: CompiledExpr[T, ?])(using
      spark: SparkSession
  ) = (
    convert(seq),
    (elem: Column) => {
      val elemCf = ColumnFactory(elem)(using compiled.arg.codec)
      new ExprOnSpark[T](cfs + (compiled.arg -> elemCf)).convert(compiled.expr)
    }
  )

  def convert(expr: ExprNode[?])(using spark: SparkSession): Column =
    expr match {
      case ExprNode.Select(ref: ExprNode.Reference[?], name) => cfFromRef(ref).column(name)
      case ExprNode.Select(p, name) => convert(p)(name)
      case ref @ ExprNode.Reference(_, _) => cfFromRef(ref).row
      case ExprNode.Literal(value, codec) => literal(value, codec)

      // Empty struct triggers assertion failure is Spark 3.5.x fixed in
      // https://github.com/apache/spark/pull/44527
      // We work around it by adding a dummy column that will never be used
      case ExprNode.MakeProduct(values, Codec.Product(_, _, Some(_))) =>
        struct(lit(null).as(Forbidden.column))

      case ExprNode.None(_) => lit(null).cast(catalystType(expr.codec))
      case ExprNode.MakeProduct(values, codec) =>
        val fieldExprs = tupleInstances(values).mapConst([t] => convert(_))
        val names = codec.fields.mapConst[String]([t] => _.name)
        struct(fieldExprs.zip(names).map { case (expr, name) => expr.as(name) }*)
      case ExprNode.Range(start, end) =>
        val startCol = convert(start)
        val endCol = convert(end)
        when(startCol < endCol, sequence(startCol, endCol - lit(1))).otherwise(array())
      case ExprNode.MakeSeq(values, _) =>
        val fieldExprs = values.map(convert(_))
        val arr = array(fieldExprs*)
        if values.isEmpty then arr.cast(catalystType(expr.codec)) else arr
      case ExprNode.ConcatSeq(lhs, rhs) => concat(convert(lhs), (convert(rhs)))
      case ExprNode.MapSeq(seq, f) => transform.tupled(transformArgs(seq, f))
      case ExprNode.FlatMapSeq(seq, f) =>
        val transformed = transform.tupled(transformArgs(seq, f))
        org.apache.spark.sql.functions.flatten(transformed)
      case ExprNode.FilterSeq(seq, predicate) => filter.tupled(transformArgs(seq, predicate))
      case ExprNode.AggregateSeq(seq, onEmpty, agg) =>
        val asFold = PrimitiveAggregateAsFold(onEmpty, agg)(using seq.codec.element)
        aggregate(
          convert(seq),
          convert(asFold.initial),
          (acc, elem) => {
            val compiled = asFold.merge
            val elemCf = ColumnFactory(elem)(using seq.codec.element)
            val accCf = ColumnFactory(acc)(using asFold.initial.codec)
            val newCfs = cfs + (compiled.arg1 -> elemCf) + (compiled.arg2 -> accCf)
            new ExprOnSpark[T](newCfs).convert(compiled.expr)
          },
          res => {
            val compiled = asFold.finish
            val accCf = ColumnFactory(res)(using asFold.initial.codec)
            val newCfs = cfs + (compiled.arg -> accCf)
            new ExprOnSpark[T](newCfs).convert(compiled.expr)
          }
        )
      case ExprNode.And(lhs, rhs) => convert(lhs) && convert(rhs)
      case ExprNode.Or(lhs, rhs) => convert(lhs) || convert(rhs)
      case ExprNode.Not(e) => !convert(e)
      case IsNone(e) => convert(e).isNull
      /* We do not want Option[?] to get the sql null semantics and have to use the null safe comparision. But
       * we do not use it for all types bacause of how spark handles null safe equals in joins and it would
       * potentially lead to extra shuffles. */
      case ExprNode.Equals(Nullable(lhs), Nullable(rhs)) => convert(lhs) <=> convert(rhs)
      case ExprNode.Equals(lhs, rhs) => convert(lhs) === convert(rhs)
      case ExprNode.LessThan(_, lhs, rhs) => convert(lhs) < convert(rhs)
      case ExprNode.LessThanOrEqual(_, lhs, rhs) => convert(lhs) <= convert(rhs)
      case ExprNode.UpcastToIterable(e) => e.codec match {
          // The cast he is to rename the struct fields from key, value to _1, _2
          case _: Codec.Map[?, ?] => map_entries(convert(e)).cast(catalystType(expr.codec))
          case ArrayCodec(_) => convert(e)
          case codec => unreachable(s"UpcastToIterable only get codecs of Map and Iterable not $codec")
        }
      case ExprNode.OptionToIterable(e) =>
        val column = convert(e)
        val elementColumn = e.codec match {
          case Codec.Option(Codec.Option(_)) => column("value")
          case _ => column
        }
        when(column.isNotNull, array(elementColumn)).otherwise(array())
      case ExprNode.Udf(e, f, codec) => createUdf(f, convert(e))(using e.codec, codec)
      case ExprNode.MakeSome(Nullable(e)) => wrapNestedSome(convert(e))
      case ExprNode.MakeSome(e) => convert(e)
      case ExprNode.Coalesce(operands) => coalesce(operands.map(convert(_))*)
      case Nullable(ExprNode.KnownNotNull(e)) => convert(e)("value")
      case ExprNode.KnownNotNull(e) => convert(e)
      case ExprNode.RaiseError(message, codec) => raise_error(convert(message)).cast(catalystType(codec))
      case ExprNode.ScalarSubquery(ds) =>
        // TODO: When only supporting Spark 4.0+ we can use the improve subquery apis from
        // https://github.com/apache/spark/pull/48664 and plan this as a scalar subquery
        // instead of executing it eagerly.
        val values = DatasetOnSpark(ds).take(2)
        assert(
          values.size == 1,
          s"Scalar subquery ${ds} returned ${values.size} rows but expected exactly one row"
        )
        createUdf(() => values.head, name = Some("scalar-subquery-result"))(using ds.codec)
      case ExprNode.ExistsSubquery(ds) =>
        // TODO: Use subquery apis from Spark 4.0+ as mentioned for ScalarSubquery.
        val exists = DatasetOnSpark(ds).take(1).nonEmpty
        lit(exists)
      case ExprNode.Aggregate(arg, agg) =>
        val argCf = ColumnFactory(convert(arg))(using arg.codec)
        PrimitiveAggregateOnSpark.resolved(argCf, agg)
      case ExprNode.Cases(whenThenExpr, whenThenExprs, elseExpr) =>
        val initial = when(convert(whenThenExpr.whenExpr), convert(whenThenExpr.thenExpr))
        val cases = whenThenExprs.foldLeft(initial)((acc, branch) =>
          acc.when(convert(branch.whenExpr), convert(branch.thenExpr))
        )
        cases.otherwise(convert(elseExpr))
      case ExprNode.StartsWith(string, prefix) => startswith(convert(string), convert(prefix))
      case ExprNode.Trim(string) => trim(convert(string))
      case ExprNode.EndsWith(string, suffix) => endswith(convert(string), convert(suffix))
      case ExprNode.ConcatString(strings) => concat(strings.map(convert)*)
      case ExprNode.Split(string, delimiter) =>
        // This is an implemenation of java.util.regex.Pattern.quote using spark expr api.
        val quotedDelimiter =
          concat(lit("\\Q"), replace(convert(delimiter), lit("\\E"), lit("\\E\\\\E\\Q")), lit("\\E"))
        /* In Spark 4.0.0+ (https://github.com/apache/spark/pull/46045) we can call split function directly
         * instead of using call_function */
        call_function("split", convert(string), quotedDelimiter)
      case SparkJsonCompatability.AdaptToJson(adapted) => convert(adapted)
      case SparkJsonCompatability.ConvertFromJson(converted) => convert(converted)
      case ExprNode.ToJson(inner) => to_json(convert(inner), jsonOptions)
      case ExprNode.FromJson(inner, codec) => from_json(convert(inner), catalystType(codec), jsonOptions)
      case ExprNode.SizeSeq(operand) => size(convert(operand))
      case ExprNode.ElementSeq(array, index) =>
        val idx = convert(index)
        val adjustedIdx =
          when(idx >= lit(0), idx + lit(1)).otherwise(raise_error(lit("Negative array index not supported")))
        call_function("element_at", convert(array), adjustedIdx)
      case ExprNode.Add(additive, lhs, rhs) => convert(lhs) + convert(rhs)
      case ExprNode.Quotient(CompatibleIntegral(), lhs, rhs) =>
        call_function("div", convert(lhs), convert(rhs)).cast(catalystType(expr.codec))
      case ExprNode.Quotient(integral, lhs, rhs) =>
        createUdf(integral.quot, convert(lhs), convert(rhs), s"$integral.quot")(using lhs.codec, lhs.codec)
      case ExprNode.Cast(from, canCast) => convert(from).cast(catalystType(expr.codec))
      case ExprNode.TryCast(from, canTryCast) =>
        val casted = tryCast(convert(from), expr.codec)
        if from.codec == Codec.String then when(!convert(from).rlike("\\p{Cc}"), casted) else casted
      case ExprNode.TimestampToMicros(inner) => unix_micros(convert(inner))
      case ExprNode.MicrosToTimestamp(inner) => timestamp_micros(convert(inner))
      case ExprNode.DurationToMicros(inner) => convert(inner)
      case ExprNode.MicrosToDuration(inner) => convert(inner)
      case ExprNode.DateToDays(inner) => unix_date(convert(inner))
      case ExprNode.DaysToDate(inner) => date_from_unix_date(convert(inner))
      case ExprNode.BytesLength(inner) => length(convert(inner))
      case ExprNode.ToRepr(inner, _) => convert(inner)
      case ExprNode.FromRepr(inner, _) => convert(inner)
      case ExprNode.MakeMap(pairs) => map_from_entries(convert(pairs))
      case ExprNode.MapEntries(map) => map_entries(convert(map))
      case ExprNode.MapGet(map, key) =>
        val mapCol = convert(map)
        val keyCol = convert(key)
        val value = element_at(mapCol, keyCol)
        map.codec match {
          case Codec.Map(_, Codec.Option(_)) => when(map_contains_key(mapCol, keyCol), wrapNestedSome(value))
          case _ => value
        }
      case ExprNode.DistinctSeq(operand) => array_distinct(convert(operand))
      case ExprNode.Rand() => rand()
      case ExprNode.IsNaN(operand) => isnan(convert(operand))
    }
}
