package com.choreograph.tyda.spark

import java.util.Collections
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentMap
import java.util.concurrent.atomic.AtomicLong

import scala.ref.ReferenceQueue
import scala.ref.WeakReference

import com.google.common.collect.MapMaker
import org.apache.hadoop.fs.Path
import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.DataFrameReader
import org.apache.spark.sql.Dataset as SparkDataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.count
import org.apache.spark.sql.functions.explode
import org.apache.spark.sql.functions.lit
import org.apache.spark.sql.functions.struct
import org.apache.spark.sql.functions.when
import org.apache.spark.sql.types.StructField
import org.apache.spark.sql.types.StructType
import org.slf4j.LoggerFactory
import shapeless3.deriving.K0

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.Format
import com.choreograph.tyda.HivePartitionParser
import com.choreograph.tyda.TableLocation
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.functions.some
import com.choreograph.tyda.rewrite.ExplodeOptionToFilter
import com.choreograph.tyda.rewrite.ReplacementMap
import com.choreograph.tyda.rewrite.ReplacementMap.Replacement
import com.choreograph.tyda.rewrite.SparkJsonCompatability
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.spark.DataFrameOps.unpivotAs

/** For certain operations like joins materlizing the nested "typed structure"
  * causing Spark to no longer reuse the shuffle files for consecutive
  * operations. So we delay that by keeping a [[DataFrame]] and the column
  * unmaterlized instead.
  */
private enum IntermediateDataset[T] {
  case Dataset(ds: SparkDataset[T], codec: Codec[T])
  case DataframeAndColumnFactory(df: DataFrame, cf: ColumnFactory[T])

  given Codec[T] =
    this match {
      case Dataset(_, codec) => codec
      case DataframeAndColumnFactory(_, cf) => cf.codec
    }

  def toDataset: SparkDataset[T] =
    this match {
      case Dataset(ds, _) => ds
      case DataframeAndColumnFactory(df, cf) => DatasetOnSpark.selectAndUnpack(df, cf.row)
    }

  def toDataFrameAndColumnFactory: (DataFrame, ColumnFactory[T]) =
    this match {
      case Dataset(ds, _) => (ds.toDF(), ColumnFactory(ds))
      case DataframeAndColumnFactory(df, cf) => (df, cf)
    }

  def alias(prefix: String): IntermediateDataset[T] = {
    val identifier = s"${prefix}_${DatasetOnSpark.nextId}"
    val df = toDataset.toDF().alias(identifier)
    IntermediateDataset.DataframeAndColumnFactory(df, ColumnFactory.fromIdentitfier[T](identifier))
  }
}

private object IntermediateDataset {
  def apply[T: Codec](ds: SparkDataset[T]): IntermediateDataset[T] = IntermediateDataset.Dataset(ds, Codec[T])

  def apply[T: Codec](df: DataFrame, col: Column): IntermediateDataset[T] =
    IntermediateDataset.DataframeAndColumnFactory(df, ColumnFactory(col))

  def apply[T](df: DataFrame, cf: ColumnFactory[T]): IntermediateDataset[T] =
    IntermediateDataset.DataframeAndColumnFactory(df, cf)
}

object DatasetOnSpark {
  import CodecToEncoder.convert

  private val logger = LoggerFactory.getLogger(DatasetOnSpark.getClass)
  private val idCounter = AtomicLong()
  private[spark] def nextId: Long = idCounter.getAndIncrement()

  private final class UnpersistTask(
      ds: Dataset[?],
      val sparkDs: SparkDataset[?],
      queue: ReferenceQueue[Dataset[?]]
  ) extends WeakReference(ds, queue)

  /** This is to make sure a given [[com.choreograph.tyda.Dataset]] returns the
    * same [[org.apache.spark.sql.Dataset]] each time the [[apply]] method is
    * called. This is important to make sure caching works correctly in all
    * cases. Specifically it seems that udaf contains Encoders that does not
    * compare using ==.
    *
    * Note: We use weak references to avoid memory leaks. This means that if the
    * [[com.choreograph.tyda.Dataset]] is not used anywhere it can be garbage
    * collected and removed from the map.
    */
  private val existingConversions: ConcurrentMap[Dataset[?], IntermediateDataset[?]] =
    MapMaker().weakKeys().makeMap()

  /* We use a reference queue to unpersist the Spark Datasets where the corresponding Dataset has been garbage
   * collected. */
  //
  // This is the same approach as is used in Spark for cleaning up persisted RDDs
  /* https://github.com/apache/spark/blob/4e5ed454fb292bc22cbdb6fc69b7de322e0afeff/core/src/main/scala/org/apache/spark/ContextCleaner.scala#L76 */
  private val referenceQueue = new ReferenceQueue[Dataset[?]]()
  private val unpersistTasks = Collections.newSetFromMap[UnpersistTask](ConcurrentHashMap())

  def perform(action: Dataset.Action)(using spark: SparkSession): Unit =
    def write[T](ds: SparkDataset[T], format: Format, path: String): Unit =
      ds.write.format(format.toString.toLowerCase).options(formatOptions(format)).save(path)
    action match {
      case SparkJsonCompatability.ConvertWrites(converted) => perform(converted)
      case Dataset.Action.Write(input, path, format) => write(DatasetOnSpark(input), format, path)
    }

  def apply[T](ds: Dataset[T])(using spark: SparkSession): SparkDataset[T] = {
    // Check if there is any unreferenced Dataset that needs to be unpersisted
    synchronized {
      Iterator
        .continually(referenceQueue.poll)
        .takeWhile(_.isDefined)
        .foreach {
          case Some(task: UnpersistTask) =>
            unpersistTasks.remove(task)
            task.sparkDs.unpersist()
          case _ =>
        }
    }
    withAnsiMode(toIntermediate(ds).toDataset)
  }

  private def withAnsiMode[T](f: => T)(using spark: SparkSession): T = {
    val oldSetting = spark.conf.getOption("spark.sql.ansi.enabled")
    try {
      spark.conf.set("spark.sql.ansi.enabled", value = true)
      f
    } finally oldSetting.foreach(mode => spark.conf.set("spark.sql.ansi.enabled", mode))
  }

  private def readWithFlatPartitions[P: Codec, V: Codec](
      read: StructType => DataFrame
  ): IntermediateDataset[(P, V)] = {
    val partitionFields = convert[P].schema.fields.toSeq
    val modelFields = convert[V].schema.fields.toSeq
    val schema = StructType(partitionFields ++ modelFields)
    val df = read(schema)
    def isSingleton[T: Codec]: Boolean =
      Codec[T] match {
        case Codec.Product(_, _, Some(_)) => true
        case _ => false
      }
    def structFromFields(fields: Seq[StructField], isSingleton: Boolean): Column =
      if isSingleton then {
        // Empty struct triggers assertion failure is Spark 3.5.x fixed in
        // https://github.com/apache/spark/pull/44527
        // We work around it by adding a dummy column that will never be used
        struct(lit(null).as(Forbidden.column))
      } else { struct(fields.map(_.name).map(df(_))*) }

    val value = struct(
      structFromFields(partitionFields, isSingleton[P]).as("_1"),
      structFromFields(modelFields, isSingleton[V]).as("_2")
    )
    IntermediateDataset(df, ColumnFactory[(P, V)](value))
  }

  private[spark] val jsonOptions: Map[String, String] =
    Map("timeZone" -> "UTC", "timestampFormat" -> "yyyy-MM-dd'T'HH:mm[:ss][.SSSSSS][XXX]")

  private def formatOptions(format: Format): Map[String, String] =
    format match {
      case Format.Parquet => Map.empty
      case Format.Json => Map("mode" -> "FAILFAST") ++ jsonOptions
    }

  extension (reader: DataFrameReader)
    private def configure(format: Format, filenameGlobFilter: String): DataFrameReader =
      reader
        .format(format.toString.toLowerCase)
        .option("pathGlobFilter", filenameGlobFilter)
        .options(formatOptions(format))

  private def toIntermediate[T](ds: Dataset[T])(using spark: SparkSession): IntermediateDataset[T] = {
    import CodecToEncoder.convert
    given codec: Codec[T] = ds.codec
    def compute: IntermediateDataset[T] =
      ds match {
        case SparkJsonCompatability.AdaptReads(adapted) => toIntermediate(adapted)
        case read @ Dataset.ReadTable(identifier, location, _, _) => readWithFlatPartitions(_ =>
            location match {
              case TableLocation.Native => spark.read.table(identifier)
              case TableLocation.BigQuery => spark.read.format("bigquery").load(identifier)
            }
          )(using read.partitionCodec, read.modelCodec)
        case Dataset.ReadPath(path, format, false, filenameGlobFilter, _) =>
          val schema = convert[T].schema
          IntermediateDataset(
            spark.read.schema(schema).configure(format, filenameGlobFilter).load(path).as[T]
          )

        case Dataset.ReadPath(path, format, true, filenameGlobFilter, _) =>
          IntermediateDataset(spark.read.configure(format, filenameGlobFilter).load(path).unpivotAs[T](true))
        case read @ Dataset.ReadPathWithHivePartitions(basePath, path, format, filenameGlobFilter, _, _) =>
          readWithFlatPartitions(schema =>
            spark
              .read
              .schema(schema)
              .option("basePath", basePath)
              .configure(format, filenameGlobFilter)
              .load(path)
          )(using read.partitionCodec, read.modelCodec)
        case Dataset.ReadWithMetadata(read) =>
          val (df, cf) = toIntermediate(read).toDataFrameAndColumnFactory
          IntermediateDataset(df.select(cf.column("_metadata").as("_1"), cf.row.as("_2")).as[T])
        case Dataset.ReadPartitionsPaths(p, _) =>
          val path = new Path(p)
          val fs = path.getFileSystem(spark.sparkContext.hadoopConfiguration)
          val partitions = Option(
            fs.globStatus(path)
          ).iterator.flatten.filter(_.isDirectory()).map(_.getPath.toString).toSeq
          val parser = HivePartitionParser.makeParser
          val decoded = partitions.map(parser)
          IntermediateDataset(spark.createDataset(decoded))
        case Dataset.ReadTablePartitionsPaths(identifier, location, _) =>
          val df = location match {
            case TableLocation.Native =>
              val dfRaw = spark.sql(s"SHOW PARTITIONS `${identifier}`")
              val parser = HivePartitionParser.makeParser
              dfRaw.select(createUdf(parser, dfRaw("partition"))).as[T]
            case TableLocation.BigQuery =>
              // TODO: We should generate the approriate query using tyda-sql here.
              throw new RuntimeException("Reading table partition paths is not supported for BigQuery tables")
          }
          IntermediateDataset(df)
        case Dataset.FromSeq(values, Codec.Product(_, _, Some(_))) =>
          IntermediateDataset(spark.range(values.size).select(lit(null).as(Forbidden.column)).as[T])
        case Dataset.FromSeq(values, _) => IntermediateDataset(spark.createDataset(values))
        case Dataset.Filter(input, p) =>
          val dsInput = toIntermediate(input).toDataset
          val filtered = p match {
            case CompiledExpr(arg, ExprNode.Udf[T @unchecked, Boolean](ref, f, _)) if arg == ref =>
              dsInput.filter(f)
            case _ => dsInput.filter(ExprOnSpark.resolved(dsInput, p))
          }
          IntermediateDataset(filtered)
        case ExplodeOptionToFilter(ds) => toIntermediate(ds)
        case select: Dataset.Select1[?, T] => IntermediateDataset(select1(select))
        // Workaround for https://issues.apache.org/jira/browse/SPARK-47241 that is present in Spark 3.5.1
        // The bug means that multiple explodes in the same select lead to planning issues, but planning them
        // as concecutive selects works.
        case selectN: Dataset.SelectN[?, ?] if multipleExplodes(selectN) =>
          val input = toIntermediate(selectN.input).toDataset
          val exprs = tupleInstances(selectN.exprs)
          val rowColumnName = "row"
          val (df, _) =
            exprs.foldLeft0((input.select(struct("*").as(rowColumnName)), 0)) { [t] => (dsAndCount, expr) =>
              val (ds, resultExprCount) = dsAndCount
              val cf = ColumnFactory(ds(rowColumnName))(using selectN.input.codec)
              val select = convertExplodeExpr(cf, expr)
              val existingResultColumns = (1 to resultExprCount).map(i => ds(s"_$i"))
              val columns = existingResultColumns ++
                Seq(select.as(s"_${resultExprCount + 1}"), ds(rowColumnName))
              (ds.select(columns*), resultExprCount + 1)
            }
          val columns = (1 to exprs.arity).map(i => df(s"_$i"))
          IntermediateDataset(df.select(columns*).as[T])
        case selectN: Dataset.SelectN[?, ?] =>
          val (input, cf) = toIntermediate(selectN.input).toDataFrameAndColumnFactory
          val columns = tupleInstances(selectN.exprs)
            .mapConst([t] => convertExplodeExpr(cf, _))
            .zipWithIndex
            .map { case (column, i) => column.as(s"_${i + 1}") }
          IntermediateDataset(input.select(columns*).as[T])
        case Dataset.MapPartitions(input, f, _) =>
          val ds = toIntermediate(input).toDataset
          IntermediateDataset(ds.mapPartitions(f))
        case Dataset.Distinct(input) => IntermediateDataset(toIntermediate(input).toDataset.distinct())
        case inner: Dataset.Join[?, ?] =>
          val left = toIntermediate(inner.left)
          val right = toIntermediate(inner.right)
          join(left, right, inner.p, "inner", isSelfJoin(inner.left, inner.right))
        case join: Dataset.LeftOuterJoin[?, ?] => leftJoin(join.left, join.right, join.p)
        case join: Dataset.FullOuterJoin[?, ?] => fullJoin(join.left, join.right, join.p)
        case join: Dataset.LeftAntiJoin[?, ?] => leftAntiJoin(join.left, join.right, join.p)
        case Dataset.Aggregate(input, aggExpr) => aggregate(input, aggExpr)
        case Dataset.GroupedAggregate(input, key, aggExpr) =>
          val (df, cf) = toIntermediate(input).toDataFrameAndColumnFactory
          val keyExprs = key
            .expr
            .fold(Seq.empty[ExprNode[?]])((acc, node) =>
              node match {
                case ExprNode.MakeProduct(_, _) | ExprNode.MakeSome(_) => Continue(acc)
                case other => Skip(acc :+ other)
              }
            )
            .distinct
          val keyOutputCodec = Codec.tuple(tupleInstances(
            // TYPE SAFETY: Each value in the array is of type Codec
            Tuple.fromArray(keyExprs.map(_.codec).toArray).asInstanceOf[Tuple.Map[Tuple, Codec]]
          ))
          val keyOutputRef = ExprNode.Reference()(using keyOutputCodec)
          val (replacements, keyColumns) = keyExprs
            .zipWithIndex
            .map { case (expr, index) =>
              val name = s"_${index + 1}"
              val replacement = Replacement(expr, ExprNode.Select(keyOutputRef, name))
              val column = ExprOnSpark.resolved(cf, CompiledExpr(key.arg, expr)).as(name)
              replacement -> column
            }
            .unzip
          val replacementMap = ReplacementMap(replacements)
          val keyOutput = key
            .expr
            .transformDown([t] => _ match { case expr => Continue(replacementMap.getOrElse(expr, expr)) })
          val aggregate = ExprOnSpark.resolved(cf, aggExpr)
          val aggDf = df.groupBy(keyColumns*).agg(aggregate.as("agg"))
          val keyOutputColumn = ExprOnSpark.resolved(
            ColumnFactory.fromDF(aggDf, keyOutputCodec),
            CompiledExpr(keyOutputRef, keyOutput)
          )
          IntermediateDataset(aggDf, struct(keyOutputColumn.as("_1"), aggDf("agg").as("_2")))
        case Dataset.Union(left, right) =>
          val dsLeft = toIntermediate(left).toDataset
          val dsRight = toIntermediate(right).toDataset
          IntermediateDataset(dsLeft.unionByName(dsRight))
        case Dataset.Cache(input) =>
          val persisted = toIntermediate(input).toDataset.persist()
          unpersistTasks.add(UnpersistTask(ds, persisted, referenceQueue))
          IntermediateDataset(persisted)
        case Dataset.Limit(input, n) =>
          if (n > 10_000) {
            logger.warn(s"""
              | limit(n) was called with a large value of n=$n using the Spark backend.
              | Spark collects all limited rows into a single partition, which can 
              | become huge if both n and the physical size of rows are large. This can
              | lead to out of memory errors and other performance issues.
              |
              | Consider using a smaller limit or an alternative approach to reduce
              | the number of rows.
              |
              | See: https://github.com/apache/spark/blob/v3.1.1/sql/core/src/main/scala/org/apache/spark/sql/execution/limit.scala#L44
            """.stripMargin)
          }
          IntermediateDataset(toIntermediate(input).toDataset.limit(n))
      }
    // TYPE SAFETY: The type parameter of the key is the same as the value
    existingConversions.computeIfAbsent(ds, _ => compute).asInstanceOf[IntermediateDataset[T]]
  }

  private def select1[T, U: Codec](select1: Dataset.Select1[T, U])(using SparkSession): SparkDataset[U] = {
    val input = toIntermediate(select1.input)
    select1.expr match {
      /* Use map/flatMap for row level operations instead of UDFs as this allows Spark to potentially optimize
       * away serialization between operations. */
      case CompiledExpr(arg, ExprNode.Udf[T @unchecked, U](ref, f, _)) if arg == ref => input.toDataset.map(f)
      case CompiledExplodeExpr(arg, ExprNode.Udf[T @unchecked, Iterable[U]](ref, f, _)) if arg == ref =>
        input.toDataset.flatMap(f)
      case other =>
        val (df, cf) = input.toDataFrameAndColumnFactory
        val column = convertExplodeExpr(cf, other)
        selectAndUnpack(df, column)
    }
  }

  private def multipleExplodes(select: Dataset.SelectN[?, ?]): Boolean = {
    val explodes = tupleInstances(select.exprs).foldLeft0(0)([t] =>
      (count, expr) =>
        expr match {
          case CompiledExplodeExpr(_, _) => count + 1
          case _ => count
        }
    )
    explodes > 1
  }

  private def isSelfJoin(left: Dataset[?], right: Dataset[?]): Boolean = {
    def joinInputs(ds: Dataset[?]): Set[Dataset[?]] =
      ds match {
        case Dataset.Join(left, right, _) => joinInputs(left) ++ joinInputs(right)
        case Dataset.LeftOuterJoin(left, right, _) => joinInputs(left) ++ joinInputs(right)
        case Dataset.FullOuterJoin(left, right, _) => joinInputs(left) ++ joinInputs(right)
        case Dataset.LeftAntiJoin(left, right, _) => joinInputs(left) ++ joinInputs(right)
        case other => Set(other)
      }
    // Because we do not "materialize" the Spark Dataset after joins any
    // potential join input overlap will result in self join.
    val rightInputs = joinInputs(right)
    left.exists(rightInputs.contains)
  }

  private def convertExplodeExpr[T, E](cf: ColumnFactory[T], expr: CompiledExprOrExplode[T, E])(using
      SparkSession
  ): Column =
    expr match {
      case c: CompiledExpr[T, E] => ExprOnSpark.resolved(cf, c)
      case c: CompiledExplodeExpr[T, E] => explode(ExprOnSpark.resolved(cf, c.asCompiledExpr))
    }

  private def leftJoin[T, U](leftDs: Dataset[T], rightDs: Dataset[U], p: CompiledExpr2[T, U, Boolean])(using
      SparkSession
  ): IntermediateDataset[(T, Option[U])] = {
    given Codec[T] = leftDs.codec
    given Codec[U] = rightDs.codec
    val left = toIntermediate(leftDs)
    val right = toIntermediate(rightDs.select(some(_)))

    join(
      left,
      right,
      p.compose(CompiledExpr[T, T](identity), CompiledExpr[Option[U], U](Expr.knownNotNull(_))),
      "left_outer",
      isSelfJoin(leftDs, rightDs)
    )
  }

  private def fullJoin[T, U](leftDs: Dataset[T], rightDs: Dataset[U], p: CompiledExpr2[T, U, Boolean])(using
      SparkSession
  ): IntermediateDataset[(Option[T], Option[U])] = {
    given Codec[T] = leftDs.codec
    given Codec[U] = rightDs.codec
    val left = toIntermediate(leftDs.select(some(_)))
    val right = toIntermediate(rightDs.select(some(_)))

    join(
      left,
      right,
      p.compose(
        CompiledExpr[Option[T], T](Expr.knownNotNull(_)),
        CompiledExpr[Option[U], U](Expr.knownNotNull(_))
      ),
      "full_outer",
      isSelfJoin(leftDs, rightDs)
    )
  }

  private def leftAntiJoin[T, U](leftDs: Dataset[T], rightDs: Dataset[U], p: CompiledExpr2[T, U, Boolean])(
      using SparkSession
  ): IntermediateDataset[T] = {
    given Codec[T] = leftDs.codec
    val left = toIntermediate(leftDs)
    val right = toIntermediate(rightDs)
    join(left, right, p, "left_anti", isSelfJoin(leftDs, rightDs), outputOnlyLeft = true)
  }

  private def join[T, U, R: Codec](
      intermidateLeft: IntermediateDataset[T],
      intermidateRight: IntermediateDataset[U],
      p: CompiledExpr2[T, U, Boolean],
      join_type: String,
      isSelfJoin: Boolean,
      outputOnlyLeft: Boolean = false
  )(using SparkSession): IntermediateDataset[R] = {
    def aliasIfNeeded[A](ds: IntermediateDataset[A]): IntermediateDataset[A] =
      if isSelfJoin then ds.alias("self_join") else ds

    /* For self joins we need to alias one side to avoid it being ambiguous which side columns are refering
     * to. */
    val (left, leftCf) = aliasIfNeeded(intermidateLeft).toDataFrameAndColumnFactory
    val (right, rightCf) = aliasIfNeeded(intermidateRight).toDataFrameAndColumnFactory
    val column = ExprOnSpark.resolved(leftCf, rightCf, p)
    val outputColumn =
      if outputOnlyLeft then leftCf.row else struct(leftCf.row.as("_1"), rightCf.row.as("_2"))
    IntermediateDataset[R](left.join(right, column, join_type), outputColumn)
  }

  /** When doing a select to a Product we need to flatten it to get the expect
    * schema.
    */
  private[spark] def selectAndUnpack[R: Codec](ds: SparkDataset[?], column: Column): SparkDataset[R] =
    Codec[R] match {
      case _: Codec.Product[R] | _: Codec.Sum[R, ?] => ds.select(column.as("tmp")).select("tmp.*").as[R]
      case _ => ds.select(column.as("value")).as[R]
    }

  private def aggregate[T, R](input: Dataset[T], aggExpr: CompiledAggregateExpr[T, R])(using
      SparkSession
  ): IntermediateDataset[Option[R]] = {
    given Codec[R] = aggExpr.codec
    val (df, cf) = toIntermediate(input).toDataFrameAndColumnFactory
    val aggExprAsOption = aggExpr.andThen(CompiledExpr(some(_)))
    val result = ExprOnSpark.resolved(cf, aggExprAsOption)
    val aggDf = df.agg(count(lit(1)).as("count"), result.as("_1"))
    IntermediateDataset(aggDf, when(aggDf("count") =!= 0, aggDf("_1")).otherwise(lit(null)))
  }
}
