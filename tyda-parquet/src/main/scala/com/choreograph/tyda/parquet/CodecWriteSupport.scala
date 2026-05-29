package com.choreograph.tyda.parquet

import scala.compiletime.uninitialized

import org.apache.hadoop.conf.Configuration
import org.apache.parquet.hadoop.api.WriteSupport
import org.apache.parquet.hadoop.api.WriteSupport.WriteContext
import org.apache.parquet.io.api.RecordConsumer
import org.apache.parquet.schema.MessageType

import com.choreograph.tyda.Codec

class CodecWriteSupport[T: Codec] extends WriteSupport[T] {
  private var recordConsumer: RecordConsumer = uninitialized
  private val writer: Writer[T] = Writer.create(Codec[T])

  override def init(configuration: Configuration): WriteContext = {
    val schema: MessageType = CodecToMessageType.convert[T]
    val metadata = new java.util.HashMap[String, String]()
    new WriteContext(schema, metadata)
  }

  override def prepareForWrite(recordConsumer: RecordConsumer): Unit = this.recordConsumer = recordConsumer

  def write(record: T): Unit = writer(recordConsumer, record)
}
