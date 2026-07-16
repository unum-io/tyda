package org.apache.spark.sql.tydashim
import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.ScalaUDF
import org.apache.spark.sql.expressions.SparkUserDefinedFunction

import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
import org.apache.spark.sql.types.DataType

def createScalaUDF(udf: SparkUserDefinedFunction, exprs: Seq[Column]): ScalaUDF =
  udf.createScalaUDF(exprs.map(_.expr))

def udf[T1, T2, U](
    f: (T1, T2) => U,
    name: String,
    inputEncoder1: ExpressionEncoder[T1],
    inputEncoder2: ExpressionEncoder[T2],
    outputEncoder: ExpressionEncoder[U],
    dataType: DataType
): SparkUserDefinedFunction =
  SparkUserDefinedFunction(
    f = f,
    dataType = dataType,
    inputEncoders = Seq(Some(inputEncoder1), Some(inputEncoder2)),
    outputEncoder = Some(outputEncoder),
    name = Some(name)
  )

def udf[T, U](
    f: T => U,
    inputEncoder: ExpressionEncoder[T],
    outputEncoder: ExpressionEncoder[U],
    dataType: DataType
): SparkUserDefinedFunction =
  SparkUserDefinedFunction(
    f = f,
    dataType = dataType,
    inputEncoders = Seq(Some(inputEncoder)),
    outputEncoder = Some(outputEncoder)
  )

def udf[U](
    f: () => U,
    name: Option[String],
    outputEncoder: ExpressionEncoder[U],
    dataType: DataType
): SparkUserDefinedFunction =
  SparkUserDefinedFunction(
    f = f,
    dataType = dataType,
    inputEncoders = Seq.empty,
    outputEncoder = Some(outputEncoder),
    name = name
  )
