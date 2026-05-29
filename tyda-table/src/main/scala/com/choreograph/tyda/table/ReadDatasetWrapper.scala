package com.choreograph.tyda.table

import scala.annotation.targetName

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.FileMetadata

sealed trait ReadDatasetWrapper[P, T] {
  def values: Dataset[T]

  def withFileMetadata: Dataset[(FileMetadata, T)]
  def withPartitionValues: Dataset[(P, T)]
  def withFileMetadataAndPartitionValues: Dataset[(FileMetadata, P, T)]

  private[table] def select[U](f: Expr[T] => Expr[U]): ReadDatasetWrapper[P, U] =
    ReadDatasetWrapper.Select(this, f)
}

object ReadDatasetWrapper {
  def apply[P, T](ds: Dataset[(FileMetadata, P, T)]): ReadDatasetWrapper[P, T] = FromFull(ds)
  @targetName("applyRead")
  def apply[P, T](partitioned: Dataset.Read[(P, T)], justValues: Dataset[T]): ReadDatasetWrapper[P, T] =
    FromReadWithPartitions(partitioned, justValues)

  def apply[P, T](ds: Dataset.Read[(P, T)], predicate: Expr[P] => Expr[Boolean]): ReadDatasetWrapper[P, T] =
    Filtered(ds, predicate)

  /* This is to avoid materializing the metadata column when not needed. Seems like Spark currently does not
   * optimize it away fully. */
  private[table] final case class Filtered[P, T](
      ds: Dataset.Read[(P, T)],
      predicate: Expr[P] => Expr[Boolean]
  ) extends ReadDatasetWrapper[P, T] {
    val filtered = ds.where { case Expr(p, _) => predicate(p) }
    def values: Dataset[T] = filtered.select(_._2)
    def withFileMetadata: Dataset[(FileMetadata, T)] = withFileMetadataAndPartitionValues.select(_._1, _._3)
    def withPartitionValues: Dataset[(P, T)] = filtered
    def withFileMetadataAndPartitionValues: Dataset[(FileMetadata, P, T)] =
      ds.withMetadata.where { case Expr(_, Expr(p, _)) => predicate(p) }.select(_._1, _._2._1, _._2._2)
  }

  private[table] final case class FromFull[P, T](ds: Dataset[(FileMetadata, P, T)])
      extends ReadDatasetWrapper[P, T] {
    def values: Dataset[T] = ds.select(_._3)

    def withFileMetadata: Dataset[(FileMetadata, T)] = ds.select(_._1, _._3)
    def withPartitionValues: Dataset[(P, T)] = ds.select(_._2, _._3)
    def withFileMetadataAndPartitionValues: Dataset[(FileMetadata, P, T)] = ds
  }

  /** Implementation when the partitions can be read without parsing the file
    * metadata.
    *
    * Since spark does not always fully optimize away accessing the metadata
    * column this implementation is preferable when `P` is available without
    * reading it from the file metadata.
    *
    * Note: Currently we do not support readWithHivePartitions in
    * DatasetOnIterator. So justValues is included here to be able to read just
    * the values for that case. When readWithHivePartitions is supported in
    * DatasetOnIterator, we can remove justValues and use
    * partitioned.select(_._2) instead of justValues in values method.
    */
  private final case class FromReadWithPartitions[P, T](
      partitioned: Dataset.Read[(P, T)],
      justValues: Dataset[T]
  ) extends ReadDatasetWrapper[P, T] {
    def values: Dataset[T] = justValues

    def withFileMetadata: Dataset[(FileMetadata, T)] = partitioned.withMetadata.select(_._1, _._2._2)
    def withPartitionValues: Dataset[(P, T)] = partitioned
    def withFileMetadataAndPartitionValues: Dataset[(FileMetadata, P, T)] =
      partitioned.withMetadata.select(_._1, _._2._1, _._2._2)
  }

  private[table] final case class UnableToParsePartitions[P, T](ds: Dataset[(FileMetadata, T)], e: Throwable)
      extends ReadDatasetWrapper[P, T] {
    def values: Dataset[T] = ds.select(_._2)

    def withFileMetadata: Dataset[(FileMetadata, T)] = ds
    def withPartitionValues: Nothing =
      throw new RuntimeException(
        "If job reads metadata or partition values, it must provide a FileMetadata with valid paths to Source.Test.",
        e
      )
    def withFileMetadataAndPartitionValues: Dataset[(FileMetadata, P, T)] = withPartitionValues
  }

  private final case class Select[P, T, U](from: ReadDatasetWrapper[P, T], f: Expr[T] => Expr[U])
      extends ReadDatasetWrapper[P, U] {
    def values: Dataset[U] = from.values.select(f)
    def withFileMetadata: Dataset[(FileMetadata, U)] = from.withFileMetadata.select(_._1, f.compose(_._2))
    def withPartitionValues: Dataset[(P, U)] = from.withPartitionValues.select(_._1, f.compose(_._2))
    def withFileMetadataAndPartitionValues: Dataset[(FileMetadata, P, U)] =
      from.withFileMetadataAndPartitionValues.select(_._1, _._2, f.compose(_._3))
  }
}
