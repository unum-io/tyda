package com.choreograph.tyda.spark

import org.apache.spark.sql.Encoder
import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder

import com.choreograph.tyda.Codec
import com.choreograph.tyda.spark.CodecToExpressionEncoder.createDeserializer
import com.choreograph.tyda.spark.CodecToExpressionEncoder.createSerializer

object CodecToEncoder {
  given convert[T: Codec]: Encoder[T] = convertInternal[T]

  private[spark] def convertInternal[T: Codec]: ExpressionEncoder[T] =
    new ExpressionEncoder[T](createSerializer(Codec[T]), createDeserializer(Codec[T]), Codec[T].classTag)
}
