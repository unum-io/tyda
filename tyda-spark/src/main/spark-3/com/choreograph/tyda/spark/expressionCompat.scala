package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.Cast
import org.apache.spark.sql.catalyst.expressions.EvalMode
import org.apache.spark.sql.catalyst.expressions.ScalaUDF

import com.choreograph.tyda.Codec
import com.choreograph.tyda.spark.CodecToCatalystType.catalystType

private def createUdf[T: Codec, U: Codec](f: (T, T) => U, arg1: Column, arg2: Column, name: String) = {
  val udf = ScalaUDF(
    function = f,
    dataType = catalystType(Codec[U]),
    children = Seq(arg1.expr, arg2.expr),
    inputEncoders = Seq(
      CodecToEncoder.convertInternal(using Codec[T]),
      CodecToEncoder.convertInternal(using Codec[T])
    ).map(Some(_)),
    outputEncoder = Some(CodecToEncoder.convertInternal(using Codec[U])),
    udfName = Some(name)
  )
  new Column(udf)
}

private def createUdf[T: Codec, U: Codec](f: T => U, arg: Column) = {
  val udf = ScalaUDF(
    function = f,
    dataType = catalystType(Codec[U]),
    children = Seq(arg.expr),
    inputEncoders = Seq(Some(CodecToEncoder.convertInternal(using Codec[T]))),
    outputEncoder = Some(CodecToEncoder.convertInternal(using Codec[U]))
  )
  new Column(udf)
}

private def createUdf[U: Codec](f: () => U, name: Option[String] = None) = {
  val udf = ScalaUDF(
    function = f,
    dataType = catalystType(Codec[U]),
    children = Seq(),
    outputEncoder = Some(CodecToEncoder.convertInternal(using Codec[U])),
    udfName = name
  )
  new Column(udf)
}

private def tryCast(from: Column, codec: Codec[?]): Column = {
  val tryCast = Cast(from.expr, catalystType(codec), None, EvalMode.TRY)
  new Column(tryCast)
}
