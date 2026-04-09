package com.choreograph.tyda.sql

import com.choreograph.tyda.Format
import com.choreograph.tyda.rewrite.ActionRule
import com.choreograph.tyda.rewrite.CheckArrayIndexPositive
import com.choreograph.tyda.rewrite.CheckMapKeysDistinct
import com.choreograph.tyda.rewrite.DatasetRule
import com.choreograph.tyda.rewrite.DisfavorIsNotDistinctFrom
import com.choreograph.tyda.rewrite.DistributeProductAndSeqEquals
import com.choreograph.tyda.rewrite.ExprRule
import com.choreograph.tyda.rewrite.RemoveArrayCasts
import com.choreograph.tyda.rewrite.RemoveNullSafeEqualsInJoinCondition
import com.choreograph.tyda.rewrite.SparkJsonCompatability
import com.choreograph.tyda.rewrite.WrapOptionInCollect
import com.choreograph.tyda.sql.DdlDialect.DecimalSupport
import com.choreograph.tyda.sql.DdlDialect.DurationSupport

/** The SQL dialect to use when unparsing a Dataset.
  *
  * This is meant to capture all SQL features that might vary between different
  * SQL implementations.
  */
final case class SqlDialect(
    arrayDistinct: SqlDialect.ArrayDistinct,
    arrayConcat: String,
    arrayElement: SqlDialect.ArrayElement,
    arraySize: String,
    arrayHigherOrderFunctions: SqlDialect.ArrayHigherOrderFunctions,
    binaryLiteral: SqlDialect.BinaryLiteral,
    boolAndFunction: String,
    boolOrFunction: String,
    collectFunction: String,
    countIfFunction: String,
    ddl: DdlDialect,
    errorFunction: String,
    explode: SqlDialect.ExplodeSupport,
    extractDateDays: String,
    extractTimestampMicros: String,
    floatingAggregate: SqlDialect.FloatingAggregate,
    floatingCompare: SqlDialect.FloatingCompare,
    fromJson: SqlDialect.FromJsonSupport,
    intergerSupport: SqlDialect.IntegerSupport,
    isNanFunction: String,
    makeArray: SqlDialect.MakeArray,
    makeDate: SqlDialect.MakeDate,
    makeDuration: SqlDialect.MakeDuration,
    makeStruct: SqlDialect.MakeStruct,
    makeTimestamp: SqlDialect.MakeTimestamp,
    mapSupport: SqlDialect.MapSupport,
    range: SqlDialect.Range,
    regexp: String,
    endsWithFunction: String,
    splitFunction: SqlDialect.SplitFunction,
    trimFunction: SqlDialect.TrimFunction,
    startsWithFunction: String,
    toJson: SqlDialect.ToJsonSupport,
    tryCast: String,
    tryCastTrimsControlChars: Boolean = false,
    // BigQuery complains when the struct functions is used in GROUP BY clause but has no problem
    // when done as part of a subquery first.
    useSubqueryToAvoidStructInGroupBy: Boolean = false,
    // BigQuery does not support EXCEPT DISTINCT when any SELECT column is of STRUCT type.
    // When false, the Distinct(LeftAntiJoin(_ == _)) pattern uses NOT EXISTS + DISTINCT instead.
    supportsExceptDistinctOnStructColumns: Boolean = true,
    values: SqlDialect.Values,
    writeSupport: SqlDialect.WriteSupport,
    rand: String,

    /* A list of potentially non-idempotent expressions passes that should be applied to the Dataset once as
     * part of unparsing.
     *
     * The rules here are commonly not for optimization, but for adding nodes that make sure the generated SQL
     * have the correct semantics. */
    correctnessRules: Seq[ActionRule | DatasetRule | ExprRule]
)

object SqlDialect {
  enum Values {

    /** Supports the SQL standard `VALUES` clause in the FROM clause. e.g.
      * ```
      * SELECT * FROM VALUES (1, 'a'), (2, 'b') as t(col1, col2)
      * ```
      */
    case Native

    /** When VALUES clause is not supported in the FROM clause, we can emulate
      * it using a series of `UNION ALL` selects. e.g.
      * ```
      * SELECT 1 AS col1, 'a' AS col2
      * UNION ALL
      * SELECT 2 AS col1, 'b' AS col2
      * ```
      */
    case SelectUnionAll
  }

  enum MakeStruct {

    /** A function taking the fields names and values as arguments. e.g.
      * ```
      * struct('a', 1, 'b', 2)
      * ```
      * Would create a struct with fields 'a' and 'b' with values 1 and 2
      * respectively.
      */
    case Function(name: String)

    /** A function taking columns where the name comes from the column alias.
      * e.g.
      * ```
      * struct(col1 AS a, col2 AS b)
      * ```
      * Would create a struct with fields 'a' and 'b' with values from col1 and
      * col2 respectively.
      */
    case FunctionAndAlias(name: String)
  }

  enum MakeTimestamp {

    /** A function that takes a single argument of Long containing the number of
      * microseconds since the epoch unix epoch.
      */
    case Function(name: String)
  }

  enum MakeDuration {

    /** A duration is constructed by casting a decimal value in seconds with
      * microsecond fractional part. e.g.
      * ```
      * CAST(duration_in_seconds AS INTERVAL)
      * ```
      */
    case Cast

    /** Duration is not supported natively, but can be simulated using just a
      * raw int corresponding to microsecond duration, e.g.
      * ```
      * end_micros_since_epoch - beginning_micros_since_epoch
      * ```
      */
    case DiffBigInt
  }

  enum ArrayDistinct {

    /** Array distinct is supported via a function. e.g.
      * ```
      * array_distinct(array_column)
      * ```
      */
    case Function(name: String)

    /** Array distinct needs to be implemented using a subquery. e.g.
      * ```
      * ARRAY(SELECT DISTINCT element FROM UNNEST(array_column) AS element)
      * ```
      */
    case Subquery(makeArray: String, unnest: String)
  }

  enum MapSupport {
    case Array
    case Native(makeMap: String, mapEntries: String, mapGet: String, mapContains: String)
  }

  enum IntegerSupport {

    /** Supports all integer sizes (8, 16, 32, 64 bit) */
    case AllSizes

    /** Only supports BigInt (64 bit integers) */
    case OnlyBigInt
  }

  final case class ToJsonSupport(functionName: String, options: Option[Map[String, String]])
  enum FromJsonSupport {
    case Parser(fromJson: String, options: Map[String, String])
    case Extractors(extractScalar: String, extractArray: String, extractObject: String)
  }

  enum MakeArray {

    /** A function taking a variable number of arguments. e.g.
      * ```
      * array(1, 2, 3)
      * ```
      */
    case Function(name: String)

    /** An array can be created by enclosing a comma-separated list of values in
      * brackets. e.g.
      * ```
      * [1, 2, 3]
      * ```
      */
    case Brackets
  }

  enum ExplodeSupport {

    /** Explode is supported via a function. e.g.
      * ```
      * explode(array_column)
      * ```
      */
    case Function(name: String, supportMultipleExplodes: Boolean)

    /** Explode is supported by doing a inner join with the array column. e.g.
      * ```
      * FROM table JOIN table.array_column AS exploded_column
      * ```
      */
    case InnerJoin
  }

  enum FloatingCompare {

    /** Comparison operator follows IEEE semantics any comparision with NaN is
      * false
      */
    case Ieee

    /** Comparison operator follows the more common SQL semantics where NaN is
      * considered larger than all other values and -0.0 == 0.0.
      */
    case NaNIsLargest
  }

  enum FloatingAggregate {
    case NaNIsSmallestAndLargest
    case NaNIsLargest
  }

  enum ArrayHigherOrderFunctions {

    /** Supports higher order functions using lambda syntax. e.g.
      * ```
      * array_transform(array_column, x -> x + 1)
      * ```
      */
    case Lambda(map: String, aggregate: String, filter: String)

    /** Higher order functions can be implemented using a subquery e.g.
      * ```
      * SELECT array(SELECT x + 1 FROM unnest(array_column) AS x)
      * ```
      */
    case Subquery(makeArray: String, unnest: String)
  }

  enum MakeDate {

    /** A function that takes a single argument of Int containing the number of
      * days since the unix epoch.
      */
    case Function(name: String)
  }

  enum BinaryLiteral {

    /** A binary literal is represented as a hex string prefixed with `X`. e.g.
      * ```
      * X'0A0B0C'
      * ```
      */
    case HexString
  }

  enum Range {

    /** A range is inclusive of the upper bound. e.g.
      * ```
      * sequence(start, end)
      * ```
      * will include `end` in the resulting array.
      *
      * If `errorOnEmpty` is true, that means the underlying function will raise
      * an error if `start` > `end` and a check will be added to enure empty
      * result instead.
      */
    case Inclusive(name: String, errorOnEmpty: Boolean)
  }

  enum SplitFunction {

    /** A function that have interface and semantics similar to
      * `java.lang.String.split`.
      */
    case Java(name: String)

    /** A function that does not take a regex */
    case NonRegex(name: String)
  }

  enum TrimFunction {

    /** Trim leading and trailing spaces using the function default behavior. */
    case Default(name: String)

    /** Trim a dialect-specific set of characters. */
    case Characters(name: String, chars: String)
  }

  enum WriteSupport {

    /** Write using CREATE TABLE statement (Spark-style). */
    case CreateTable(options: Map[Format, Map[String, String]])

    /** Write using EXPORT DATA statement (BigQuery-style). */
    case ExportData
  }

  enum ArrayElement {

    /** Element access using braces and the index is zero based.
      *
      * e.g
      * ```
      * array_column[0]
      * ```
      */
    case Braces

    /** Access using a function taking an array and index.
      * ```
      * element_at(array_column, 1)
      * ```
      */
    case Function(functionName: String)

    def zeroIndexed: Boolean =
      this match {
        case Braces => true
        case Function(_) => false
      }
  }

  val BigQuery: SqlDialect = SqlDialect(
    startsWithFunction = "STARTS_WITH",
    arrayDistinct = ArrayDistinct.Subquery("array", "unnest"),
    arrayConcat = "array_concat",
    arrayElement = ArrayElement.Braces,
    arraySize = "array_length",
    arrayHigherOrderFunctions = ArrayHigherOrderFunctions.Subquery("array", "unnest"),
    binaryLiteral = BinaryLiteral.HexString,
    boolAndFunction = "logical_and",
    boolOrFunction = "logical_or",
    collectFunction = "array_agg",
    countIfFunction = "countif",
    ddl = DdlDialect(
      decimal = DecimalSupport.BigQuery(parameterized = false),
      duration = DurationSupport.Long,
      map = DdlDialect.MapSupport.Array,
      supportsNotNullColumn = false,
      supportsNotNullArrayElement = false,
      emptyStructFieldType = "BOOL",
      floatType = "FLOAT64",
      doubleType = "FLOAT64",
      supportsArrayAsArrayElement = false
    ),
    errorFunction = "error",
    explode = ExplodeSupport.InnerJoin,
    extractDateDays = "unix_date",
    extractTimestampMicros = "unix_micros",
    floatingAggregate = FloatingAggregate.NaNIsSmallestAndLargest,
    floatingCompare = FloatingCompare.Ieee,
    fromJson = FromJsonSupport.Extractors(
      extractScalar = "json_value",
      extractArray = "json_query_array",
      extractObject = "json_query"
    ),
    intergerSupport = IntegerSupport.OnlyBigInt,
    isNanFunction = "is_nan",
    makeArray = MakeArray.Brackets,
    makeDate = MakeDate.Function("date_from_unix_date"),
    makeDuration = MakeDuration.DiffBigInt,
    makeStruct = MakeStruct.FunctionAndAlias("struct"),
    makeTimestamp = MakeTimestamp.Function("timestamp_micros"),
    mapSupport = MapSupport.Array,
    range = Range.Inclusive("generate_array", errorOnEmpty = false),
    regexp = "regexp_contains",
    endsWithFunction = "ends_with",
    splitFunction = SplitFunction.NonRegex("split"),
    trimFunction = TrimFunction.Characters("trim", " "),
    toJson = SqlDialect.ToJsonSupport("to_json_string", None),
    tryCast = "SAFE_CAST",
    useSubqueryToAvoidStructInGroupBy = true,
    supportsExceptDistinctOnStructColumns = false,
    values = Values.SelectUnionAll,
    rand = "RAND",
    writeSupport = WriteSupport.ExportData,
    correctnessRules = Seq(
      CheckMapKeysDistinct,
      DistributeProductAndSeqEquals,
      DisfavorIsNotDistinctFrom,
      RemoveArrayCasts,
      RemoveNullSafeEqualsInJoinCondition
    )
  )

  private val sparkJsonOptions =
    Map("timeZone" -> "UTC", "timestampFormat" -> "yyyy-MM-dd'T'HH:mm[:ss][.SSSSSS][XXX]")
  val Spark: SqlDialect = SqlDialect(
    startsWithFunction = "startswith",
    arrayDistinct = ArrayDistinct.Function("array_distinct"),
    arrayConcat = "concat",
    arrayElement = ArrayElement.Function("element_at"),
    arraySize = "size",
    arrayHigherOrderFunctions = ArrayHigherOrderFunctions.Lambda("transform", "aggregate", "filter"),
    binaryLiteral = BinaryLiteral.HexString,
    boolAndFunction = "bool_and",
    boolOrFunction = "bool_or",
    collectFunction = "collect_list",
    countIfFunction = "count_if",
    ddl = DdlDialect.Spark,
    errorFunction = "raise_error",
    explode = ExplodeSupport.Function("explode", supportMultipleExplodes = false),
    extractDateDays = "unix_date",
    extractTimestampMicros = "unix_micros",
    floatingAggregate = FloatingAggregate.NaNIsLargest,
    floatingCompare = FloatingCompare.NaNIsLargest,
    fromJson = FromJsonSupport.Parser("from_json", Map("mode" -> "PERMISSIVE") ++ sparkJsonOptions),
    intergerSupport = IntegerSupport.AllSizes,
    isNanFunction = "isnan",
    makeArray = MakeArray.Function("array"),
    makeDate = MakeDate.Function("date_from_unix_date"),
    makeDuration = MakeDuration.Cast,
    makeStruct = MakeStruct.Function("named_struct"),
    makeTimestamp = MakeTimestamp.Function("timestamp_micros"),
    mapSupport = MapSupport.Native(
      makeMap = "map_from_entries",
      mapEntries = "map_entries",
      mapGet = "element_at",
      mapContains = "map_contains_key"
    ),
    range = Range.Inclusive("sequence", errorOnEmpty = true),
    regexp = "regexp",
    endsWithFunction = "endswith",
    splitFunction = SplitFunction.Java("split"),
    trimFunction = TrimFunction.Default("trim"),
    tryCast = "TRY_CAST",
    tryCastTrimsControlChars = true,
    toJson = SqlDialect.ToJsonSupport("to_json", Some(sparkJsonOptions)),
    values = Values.Native,
    rand = "rand",
    writeSupport =
      WriteSupport.CreateTable(Map(Format.Json -> (Map("mode" -> "FAILFAST") ++ sparkJsonOptions))),
    correctnessRules = Seq(
      CheckArrayIndexPositive,
      WrapOptionInCollect,
      SparkJsonCompatability.AdaptReads,
      SparkJsonCompatability.ConvertWrites,
      SparkJsonCompatability.AdaptToJson,
      SparkJsonCompatability.ConvertFromJson
    )
  )
}
