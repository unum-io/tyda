package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.Cast
import org.apache.spark.sql.catalyst.expressions.EvalMode
import org.apache.spark.sql.tydashim.createScalaUDF
import org.apache.spark.sql.tydashim.udf

import com.choreograph.tyda.Codec
import com.choreograph.tyda.spark.CodecToCatalystType.catalystType

private def createUdf[T: Codec, U: Codec](f: (T, T) => U, arg1: Column, arg2: Column, name: String) =
  new Column(createScalaUDF(
    udf(
      f,
      name,
      CodecToEncoder.convertInternal,
      CodecToEncoder.convertInternal,
      CodecToEncoder.convertInternal,
      catalystType(Codec[U])
    ),
    Seq(arg1, arg2)
  ))

private def createUdf[T: Codec, U: Codec](f: T => U, arg: Column) =
  new Column(createScalaUDF(
    udf[T, U](f, CodecToEncoder.convertInternal, CodecToEncoder.convertInternal, catalystType(Codec[U])),
    Seq(arg)
  ))

private def createUdf[U: Codec](f: () => U, name: Option[String] = None) =
  new Column(createScalaUDF(udf(f, name, CodecToEncoder.convertInternal, catalystType(Codec[U])), Seq()))

private def tryCast(from: Column, codec: Codec[?]): Column = {
  val tryCast = Cast(from.expr, catalystType(codec), None, EvalMode.TRY)
  new Column(tryCast)
}
