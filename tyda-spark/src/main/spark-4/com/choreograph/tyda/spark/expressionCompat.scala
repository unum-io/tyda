package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoder
import org.apache.spark.sql.tydashim.udf

import com.choreograph.tyda.Codec
import com.choreograph.tyda.spark.CodecToCatalystType.catalystType

private def createUdf[T: Codec, U: Codec](f: (T, T) => U, arg1: Column, arg2: Column, name: String) = {
  given AgnosticEncoder[T] = CodecToEncoder.toAgnostic(Codec[T])
  given AgnosticEncoder[U] = CodecToEncoder.toAgnostic(Codec[U])
  udf(f, name)(arg1, arg2)
}

private def createUdf[T: Codec, U: Codec](f: T => U, arg: Column) = {
  given AgnosticEncoder[T] = CodecToEncoder.toAgnostic(Codec[T])
  given AgnosticEncoder[U] = CodecToEncoder.toAgnostic(Codec[U])
  udf(f)(arg)
}

private def createUdf[T: Codec](f: () => T, name: Option[String] = None) = {
  given AgnosticEncoder[T] = CodecToEncoder.toAgnostic(Codec[T])
  udf(f, name)()
}

private def tryCast(from: Column, codec: Codec[?]): Column = from.try_cast(catalystType(codec))
