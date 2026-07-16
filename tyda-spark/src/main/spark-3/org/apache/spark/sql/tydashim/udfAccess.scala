package org.apache.spark.sql.tydashim
import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.ScalaUDF
import org.apache.spark.sql.expressions.SparkUserDefinedFunction

import com.choreograph.tyda.Codec
import com.choreograph.tyda.spark.CodecToCatalystType.catalystType
import com.choreograph.tyda.spark.CodecToEncoder

def createScalaUDF(udf: SparkUserDefinedFunction, exprs: Seq[Column]): ScalaUDF =
  udf.createScalaUDF(exprs.map(_.expr))

def udf[T1: Codec, T2: Codec, U: Codec](f: (T1, T2) => U, name: String): SparkUserDefinedFunction = {
  val inputEncoder1 = CodecToEncoder.convertInternal(using Codec[T1])
  val inputEncoder2 = CodecToEncoder.convertInternal(using Codec[T2])
  val outputEncoder = CodecToEncoder.convertInternal(using Codec[U])
  SparkUserDefinedFunction(
    f = f,
    dataType = catalystType(Codec[U]),
    inputEncoders = Seq(Some(inputEncoder1), Some(inputEncoder2)),
    outputEncoder = Some(outputEncoder),
    name = Some(name)
  )
}

def udf[T: Codec, U: Codec](f: T => U): SparkUserDefinedFunction = {
  val inputEncoder = CodecToEncoder.convertInternal(using Codec[T])
  val outputEncoder = CodecToEncoder.convertInternal(using Codec[U])
  SparkUserDefinedFunction(
    f = f,
    dataType = catalystType(Codec[U]),
    inputEncoders = Seq(Some(inputEncoder)),
    outputEncoder = Some(outputEncoder)
  )
}

def udf[U: Codec](f: () => U, name: Option[String]): SparkUserDefinedFunction = {
  val outputEncoder = CodecToEncoder.convertInternal(using Codec[U])
  SparkUserDefinedFunction(
    f = f,
    dataType = catalystType(Codec[U]),
    inputEncoders = Seq.empty,
    outputEncoder = Some(outputEncoder),
    name = name
  )
}
