package com.choreograph.tyda.parquet

import org.apache.hadoop.conf.Configuration
import org.apache.parquet.hadoop.api.InitContext
import org.apache.parquet.hadoop.api.ReadSupport
import org.apache.parquet.hadoop.api.ReadSupport.ReadContext
import org.apache.parquet.hadoop.example.GroupReadSupport
import org.apache.parquet.io.api.RecordMaterializer
import org.apache.parquet.schema.MessageType

import com.choreograph.tyda.Codec

class CodecReadSupport[T: Codec] extends ReadSupport[T] {
  private val groupReadSupport = GroupReadSupport()

  override def init(context: InitContext): ReadContext = groupReadSupport.init(context)

  def prepareForRead(
      configuration: Configuration,
      keyValueMetaData: java.util.Map[String, String],
      fileSchema: MessageType,
      readContext: ReadContext
  ): RecordMaterializer[T] =
    new RecordMaterializer[T] {
      private val groupMaterializer =
        groupReadSupport.prepareForRead(configuration, keyValueMetaData, fileSchema, readContext)
      private val groupDecoder = GroupDecoder(Codec[T], fileSchema)

      override def getCurrentRecord: T = groupDecoder(groupMaterializer.getCurrentRecord)

      override def getRootConverter = groupMaterializer.getRootConverter
    }
}
