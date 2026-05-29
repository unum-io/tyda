package com.choreograph.tyda.iterator
import scala.util.Using

import com.github.plokhotnyuk.jsoniter_scala.core.ReaderConfig
import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import com.github.plokhotnyuk.jsoniter_scala.core.writeToStream
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.hadoop.io.Text
import org.apache.hadoop.util.LineReader
import org.apache.parquet.hadoop.ParquetReader
import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

import com.choreograph.tyda.Aggregator
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Date
import com.choreograph.tyda.Format
import com.choreograph.tyda.HivePartitionParser
import com.choreograph.tyda.PartitionEncoding
import com.choreograph.tyda.iterator.AggregateExprEvaluation.aggregator
import com.choreograph.tyda.iterator.ExprEvaluation.lambda
import com.choreograph.tyda.json.CodecToJsoniter
import com.choreograph.tyda.parquet.CodecParquetWriter
import com.choreograph.tyda.parquet.CodecReadSupport
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances

object DatasetOnIterator {
  def perform[T](ds: Dataset.Action): Unit =
    ds match {
      case Dataset.Action.Write(input, path, Format.Parquet) => writeParquetToPath(input, path, input.codec)
      case Dataset.Action.Write(input, path, Format.Json) => writeJsonToPath(input, path, input.codec)
    }

  def apply[T](ds: Dataset[T]): Iterator[T] =
    ds match {
      case Dataset.ReadPath(path, format, false, filenameGlobFilter, codec) =>
        readFromPath(path, filenameGlobFilter, format)(using codec)
      case Dataset.ReadPathWithHivePartitions(
            basePath,
            path,
            format,
            filenameGlobFilter,
            partitionCodec,
            modelCodec
          ) =>
        readWithHivePartitions(basePath, path, filenameGlobFilter, format)(using partitionCodec, modelCodec)
      case _: (Dataset.Read[?] | Dataset.ReadWithMetadata[?] | Dataset.ReadPartitionsPaths |
            Dataset.ReadTablePartitionsPaths) => throw new NotImplementedError(
          "DatasetOnIterator currently only has limited support for read operations. Only Parquet format is partly supported."
        )
      case Dataset.FromSeq(values, _) => values.iterator
      case Dataset.Filter(input, p) => apply(input).filter(lambda(p))
      case Dataset.Select1(input, compiled) => compiled match {
          case c: CompiledExpr[?, ?] => apply(input).map(lambda(c))
          case c: CompiledExplodeExpr[?, ?] => apply(input).flatMap(lambda(c.asCompiledExpr))
        }
      case Dataset.SelectN(input, exprs) => selectN(apply(input), exprs)
      case Dataset.MapPartitions(input, f, _) => f(apply(input))
      case Dataset.Distinct(input) => apply(input).distinct
      case Dataset.Join(left, right, p) => JoinSelection.join(apply(left), apply(right), p)
      case Dataset.LeftOuterJoin(left, right, p) => JoinSelection.leftOuterJoin(apply(left), apply(right), p)
      case Dataset.FullOuterJoin(left, right, p) => JoinSelection.fullOuterJoin(apply(left), apply(right), p)
      case Dataset.LeftAntiJoin(left, right, p) => JoinSelection.leftAntiJoin(apply(left), apply(right), p)
      case Dataset.Aggregate(input, aggregateExpr) =>
        Iterator.single(aggregate(apply(input), aggregator(aggregateExpr)))
      case Dataset.GroupedAggregate(input, keyExpr, aggregateExpr) =>
        groupedAggregate(apply(input), lambda(keyExpr), aggregator(aggregateExpr))
      case Dataset.Union(left, right) => apply(left) ++ apply(right)
      case Dataset.Cache(input) => apply(input).iterator
      case Dataset.Limit(input, n) => apply(input).take(n)
    }

  private def selectN[T, R <: Tuple](
      input: Iterator[T],
      exprs: Tuple.Map[R, [X] =>> CompiledExprOrExplode[T, X]]
  ): Iterator[R] = {
    def asLambda[R](compiled: CompiledExprOrExplode[T, R]): T => Vector[R] =
      compiled match {
        case c: CompiledExpr[T, R] => lambda[T, R](c).andThen(Vector(_))
        case c: CompiledExplodeExpr[T, R] => lambda[T, Iterable[R]](c.asCompiledExpr).andThen(_.toVector)
      }
    val lambdas = tupleInstances(exprs).mapK[[X] =>> T => Vector[X]]([t] => asLambda(_))
    input.flatMap(row => lambdas.constructCartesianProduct([t] => _(row)))
  }

  extension [F[_], T](instances: K0.ProductInstances[F, T]) {
    private def constructCartesianProduct(f: [t] => F[t] => Vector[t]): Vector[T] = {
      val pure = [A] => (a: A) => Vector(a)
      val map = [A, B] => (a: Vector[A], f: A => B) => a.map(f)
      val ap = [A, B] => (fs: Vector[A => B], xs: Vector[A]) => fs.flatMap(xs.map)
      instances.constructA(f)(pure, map, ap)
    }
  }

  private def groupedAggregate[T, K, I, R](
      input: Iterator[T],
      key: T => K,
      agg: Aggregator[T, I, R]
  ): Iterator[(K, R)] =
    input.toSeq.groupMapReduce(key)(agg.reduce(agg.zero, _))(agg.merge).view.mapValues(agg.finish).iterator

  private def aggregate[T, R](input: Iterator[T], agg: Aggregator[T, ?, R]): Option[R] =
    input.map(agg.reduce(agg.zero, _)).reduceOption(agg.merge).map(agg.finish)

  private def filesMatching(path: String, filenameGlobFilter: String): Iterator[Path] = {
    val conf = new Configuration()
    val hadoopPath = new Path(path, filenameGlobFilter)
    val fs = hadoopPath.getFileSystem(conf)
    val parquetFiles = fs.globStatus(hadoopPath).iterator.filter(_.isFile).map(_.getPath)
    parquetFiles.iterator
  }

  private def readFromPath[T: Codec](path: String, filenameGlobFilter: String, format: Format): Iterator[T] =
    filesMatching(path, filenameGlobFilter).flatMap(fileReader(format))

  private def readParquetFile[T: Codec](filePath: Path): Iterator[T] = {
    val reader = ParquetReader.builder(CodecReadSupport[T](), filePath).build()
    Iterator.continually(reader.read()).takeWhile(_ != null)
  }

  private def readJsonFile[T: Codec](filePath: Path): Iterator[T] = {
    val jsonCodec = CodecToJsoniter.create[T]
    val jsonConfig = ReaderConfig.withCheckForEndOfInput(false)
    val hadoopConfig = new Configuration()
    val fs = filePath.getFileSystem(hadoopConfig)
    val inputStream = fs.open(filePath)
    val reader = LineReader(inputStream)
    val text = new Text()
    Iterator
      .continually(reader.readLine(text))
      .takeWhile(_ > 0)
      .map(_ => readFromArray(text.getBytes, jsonConfig)(using jsonCodec))
  }

  private def fileReader[T: Codec](format: Format): Path => Iterator[T] =
    format match {
      case Format.Parquet => readParquetFile(_)
      case Format.Json => readJsonFile(_)
    }

  private def readWithHivePartitions[P: Codec, M: Codec](
      basePath: String,
      path: String,
      filenameGlobFilter: String,
      format: Format
  ): Iterator[(P, M)] =
    val decoder = partionParser[P](basePath)
    val readFile = fileReader[M](format)
    filesMatching(path, filenameGlobFilter).flatMap { filePath =>
      val partition = decoder(filePath)
      readFile(filePath).map(model => (partition, model))
    }

  private def fieldParser[T](codec: Codec[T]): String => T =
    codec match {
      case Codec.Byte => _.toByte
      case Codec.Short => _.toShort
      case Codec.Int => _.toInt
      case Codec.Long => _.toLong
      case Codec.Boolean => _.toBoolean
      case Codec.Float => _.toFloat
      case Codec.Double => _.toDouble
      case Codec.String => PartitionEncoding.decode(_)
      case Codec.Date => str =>
          Date.fromIsoString(str).getOrElse(throw new RuntimeException(s"Unable to decode $str as a Date"))
      case Codec.FromInjection(inj, to) => fieldParser(to).andThen(inj.invert)
      case Codec.Bytes | Codec.TimestampMicros | Codec.DurationMicros | Codec.Seq(_) | Codec.Map(_, _) | Codec
            .Option(_) | Codec.Decimal(_, _) | Codec.Product(_, _, _) | Codec.FromInjection(_, _) =>
        throw new RuntimeException(s"Unsupported codec for hive partition decoding: $codec")
    }

  private def partionParser[P: Codec](basePath: String): Path => P =
    Codec[P] match {
      case Codec.Product(tag, fields, _) =>
        given Labelling[P] =
          Labelling(tag.getClass.getSimpleName, fields.mapConst([t] => _.name).toIndexedSeq)
        val fieldParsers = fields.mapK([t] => f => fieldParser(f.codec))
        val parser = HivePartitionParser.make(fieldParsers)
        path =>
          parser(path.toString.stripPrefix(basePath)).match {
            case Right(partition) => partition
            case Left(error) => throw new RuntimeException(error)
          }
      case Codec.FromInjection(inj, to) => partionParser(basePath)(using to).andThen(inj.invert)
      case (_: Codec.Primitive[?]) | Codec.Bytes | Codec.TimestampMicros | Codec.DurationMicros |
          Codec.Seq(_) | Codec.Map(_, _) | Codec.Option(_) | Codec.Decimal(_, _) | Codec.Product(_, _, _) |
          Codec.FromInjection(_, _) =>
        throw new RuntimeException(s"Unsupported partition type for Hive partition decoding: ${Codec[
            P
          ]}. Only Product types are supported.")
    }

  private def writeParquetToPath[T](input: Dataset[T], path: String, codec: Codec[T]): Unit = {
    given Codec[T] = codec
    val hadoopPath = new Path(path, "data.parquet")
    Using.resource(CodecParquetWriter[T](hadoopPath))(writer => apply(input).foreach(writer.write))
  }

  private def writeJsonToPath[T](input: Dataset[T], path: String, codec: Codec[T]): Unit = {
    /* Check if output directory exists and contains and fail if it does to avoid accidentally overwriting
     * data. This is similar to Spark's behavior when writing to a path. */
    val conf = new Configuration()
    val fs = new Path(path).getFileSystem(conf)
    if fs.exists(new Path(path)) then
      throw new RuntimeException(
        s"Output path $path already exists. Please remove it before writing or choose a different path."
      )
    val dataPath = new Path(path, "data.json")
    Using.resource(fs.create(dataPath)) { outputStream =>
      val jsonCodec = CodecToJsoniter.create(using codec)
      apply(input).foreach { value =>
        writeToStream(value, outputStream)(using jsonCodec)
        outputStream.write('\n')
      }
    }
  }
}
