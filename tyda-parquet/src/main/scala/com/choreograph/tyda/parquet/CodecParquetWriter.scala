package com.choreograph.tyda.parquet

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.apache.parquet.hadoop.ParquetWriter
import org.apache.parquet.hadoop.util.HadoopOutputFile

import com.choreograph.tyda.Codec

object CodecParquetWriter {
  def apply[T: Codec](path: Path): ParquetWriter[T] = new Builder[T](path).build()

  private def fromPath(path: Path): HadoopOutputFile = HadoopOutputFile.fromPath(path, new Configuration())

  final class Builder[T: Codec](path: Path) extends ParquetWriter.Builder[T, Builder[T]](fromPath(path)) {
    override protected def self(): Builder[T] = this

    override protected def getWriteSupport(conf: Configuration): CodecWriteSupport[T] =
      new CodecWriteSupport[T]()
  }
}
