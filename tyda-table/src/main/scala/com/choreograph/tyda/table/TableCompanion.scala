package com.choreograph.tyda.table

import com.choreograph.tyda.Format
import com.choreograph.tyda.TableLocation

trait TableBase {
  def format: Format = Format.Parquet
}

private def withSlash(path: String): String = if (path.endsWith("/")) path else path + "/"

/** Base trait for tables that can be read from.
  *
  * The type parameter `P` is constrained to [[Partitioner.Hive]] or
  * [[Partitioner.None]] to prevent custom partitioners from being used with
  * table-based sources. Custom partitioners are only supported for path-based
  * sources.
  */
trait ReadableTable[M, P <: Partitioner.Hive[?] | Partitioner.None] extends TableBase {
  type Source = com.choreograph.tyda.table.Source[M, P]

  def source(identifier: String, location: TableLocation): Source.Table[M, P] =
    Source.Table(identifier, location)
}

trait ReadablePathTableBase[M, P <: Partitioner] extends TableBase {
  type Source = com.choreograph.tyda.table.Source[M, P]

  def filenameGlobFilter: String =
    format match {
      case Format.Parquet => "*.parquet"
      case Format.Json => "*.json"
    }
  def unpivot: Boolean = false
}

trait ReadablePathTable[M, P <: Partitioner] extends ReadablePathTableBase[M, P] {
  def prefix: String

  def source(path: String): Source.Path[M, P] =
    Source.Path[M, P](withSlash(withSlash(path) + prefix), format, unpivot, filenameGlobFilter)
}

trait WritablePathTable[M, P <: Partitioner] extends TableBase {
  type Sink = com.choreograph.tyda.table.Sink[M, P]

  def prefix: String

  def sink(path: String): Sink.Path[M, P] = Sink.Path[M, P](withSlash(withSlash(path) + prefix), format)
}

trait PathTable[M, P <: Partitioner] extends ReadablePathTable[M, P], WritablePathTable[M, P]

@deprecated("This trait has been renamed to PathTable, use that instead")
type Table[M, P <: Partitioner] = PathTable[M, P]
