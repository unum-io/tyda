package com.choreograph.tyda.table

import java.nio.file.FileSystems

import scala.collection.MapView
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.FileMetadata
import com.choreograph.tyda.Format
import com.choreograph.tyda.NumericsReadMode
import com.choreograph.tyda.TableLocation
import com.choreograph.tyda.functions.explode

enum Source[M, P <: Partitioner] {

  /** Source that is read from a path on cloud storage.
    *
    * @param basePath
    *   The path to the file(s) to read from.
    * @param format
    *   The data format of the file(s) to read from.
    * @param unpivot
    *   When set to true instead of reading directly to `M` the data will be
    *   unpivoted based on the provided model. The model should have n > 1
    *   fields, where the first n - 2 will be used as id columns that are not
    *   unpivoted and the second to last column will store the column names
    *   (must be a String) and the last column will be used for the values.
    *
    * @param filenameGlobFilter
    *   The glob filter to apply to the filenames in the path. This is used to
    *   filter out files that should not be read. The glob filter is applied to
    *   the filenames in the path, not the full path.
    */
  case Path(
      basePath: String,
      format: Format = Format.Parquet,
      unpivot: Boolean = false,
      filenameGlobFilter: String = "*.parquet",
      numericsReadMode: NumericsReadMode = NumericsReadMode.Exact
  ) extends Source[M, P]

  case Table(identifier: String, location: TableLocation = TableLocation.Native)

  /** Source that is read from in a unit test.
    *
    * @param data
    *   The data contained in the source.
    */
  case Test(data: TestValues[M], metadata: FileMetadata) extends Source[M, P]
}

object Source {
  extension [M, P <: Partitioner](source: Source[M, P]) {
    /* This is here for backwards compatibility, maybe uses should be migrated to read path from the metadata.
     * This is will also contain a much more detailed path. */
    def path: String =
      source match {
        case source: Source.Path[M, P] => source.basePath
        case Source.Table(identifier, _) => identifier
        case Source.Test(_, metadata) => metadata.file_path
      }
  }

  object Test {
    def apply[M, P <: Partitioner](data: Seq[M], metadata: FileMetadata = FileMetadata("")): Source[M, P] =
      new Test(TestValues.Fixed(data), metadata)

    def apply[M, V, P <: Partitioner: Partitioner.Creator.From[V] as creator](
        data: (V, Seq[M])*
    ): Source[M, P] =
      new Test(
        TestValues.Partitioned(data.map((k, v) => createTestPath(creator.create(k)) -> v).toMap),
        FileMetadata("")
      )
  }

  private def createTestPath(p: Partitioner): String =
    // We add a dummy file extension so that java PathMatcher will match correctly
    p.path("/") + "file"

  private def matchingValues[M](dataMap: Map[String, Seq[M]], p: Partitioner): MapView[String, Seq[M]] =
    val fs = FileSystems.getDefault()
    val readPath = createTestPath(p)
    val matcher = fs.getPathMatcher("glob:" + readPath)
    dataMap.view.filterKeys(s => matcher.matches(java.nio.file.Path.of(s)))

  /* Currently this is restricted to Hive partitioners since for tables that the only thing that will work.
   * But if we make readTablePartitions return the value instead of a then we should be able to relax this
   * constraint. */
  extension [M, V: Codec](
      source: Source[M, Partitioner.Hive[V]]
  )(using decoder: Partitioner.Determinator[V, Partitioner.Hive[V]]) {
    def asPartitionDataset(p: Partitioner.Hive[V]): Dataset[V] =
      source match {
        case Source.Path(basePath, _, _, _, _) => Dataset.readPartitionsPaths[V](p.path(basePath))
        case Source.Table(identifier, location) =>
          Dataset.readTablePartitions[V](identifier, location).where(decoder.predicate(p))
        case Source.Test(testValues, metadata) =>
          val paths = testValues match {
            case TestValues.Fixed(_) => Seq(metadata.file_path)
            case TestValues.Partitioned(dataMap) => matchingValues(dataMap, p).keys.toSeq
          }
          Dataset.from(paths).select(_.udf(decoder.decode))
      }
  }

  extension [M: Codec, V, P <: Partitioner: Partitioner.Determinator.Into[V] as decoder](
      source: Source[M, P]
  ) {

    def asDataset(p: P)(using Codec[V]): ReadDatasetWrapper[V, M] =
      source match {
        case path @ Source.Path(_, _, _, _, NumericsReadMode.WidenBigQuery) =>
          val wideCodecAndCast = NumericsReadMode.widenBigQuery(Codec[M])
          path
            .copy(numericsReadMode = NumericsReadMode.Exact)
            .asDataset(p)(using wideCodecAndCast.codec)
            .select(wideCodecAndCast.cast)
        case Source.Path(basePath, format, unpivot, filenameGlobFilter, NumericsReadMode.Exact) => p match {
            case _: Partitioner.Hive[?] if !unpivot =>
              val partioned =
                Dataset.readWithHivePartitions[V, M](basePath, p.path(basePath), format, filenameGlobFilter)
              val justValues = Dataset.read[M](p.path(basePath), format, unpivot, filenameGlobFilter)
              ReadDatasetWrapper(partioned, justValues)
            case _ =>
              val ds = Dataset.read[M](p.path(basePath), format, unpivot, filenameGlobFilter).withMetadata
              ReadDatasetWrapper(ds.select(_._1, _._1.file_path.udf(decoder.decode), _._2))
          }
        case Source.Table(identifier, location) =>
          ReadDatasetWrapper(Dataset.readTable[V, M](identifier, location), decoder.predicate(p))

        case Source.Test(testData, metadata) => testData match {
            // Special case for empty to not require a valid metadata for that case.
            case TestValues.Fixed(Seq()) => ReadDatasetWrapper(Dataset.empty[(FileMetadata, V, M)])
            case TestValues.Fixed(data) =>
              val ds = Dataset.from(data).select(_ => metadata, identity)
              Try(decoder.decode(metadata.file_path)) match {
                case Success(partitionValue) => ReadDatasetWrapper(ds.select(_._1, _ => partitionValue, _._2))
                /* We delay the exception here so that jobs that do not use the partition values do not need
                 * to provide valid metadata. */
                case Failure(e) => ReadDatasetWrapper.UnableToParsePartitions(ds, e)
              }
            case TestValues.Partitioned(dataMap) =>
              val data = matchingValues(dataMap, p).toSeq

              if data.isEmpty then {
                val specifiedPaths = dataMap.keys.mkString(", ")
                throw new RuntimeException(
                  s"$p does not match any of the specified partitions ${specifiedPaths}"
                )
              } else {
                ReadDatasetWrapper(
                  Dataset.from(data).select(_ => metadata, _._1.udf(decoder.decode(_)), explode(_._2))
                )
              }
          }

      }
  }
}
