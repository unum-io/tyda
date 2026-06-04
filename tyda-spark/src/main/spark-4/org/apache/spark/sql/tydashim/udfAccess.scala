package org.apache.spark.sql.tydashim
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoder
import org.apache.spark.sql.expressions.SparkUserDefinedFunction
import org.apache.spark.sql.expressions.UserDefinedFunction

def udf[T1: AgnosticEncoder, T2: AgnosticEncoder, U: AgnosticEncoder](
    f: (T1, T2) => U,
    name: String
): UserDefinedFunction = {
  val inputEncoder1 = summon[AgnosticEncoder[T1]]
  val inputEncoder2 = summon[AgnosticEncoder[T2]]
  val outputEncoder = summon[AgnosticEncoder[U]]
  SparkUserDefinedFunction(
    f = f,
    dataType = outputEncoder.dataType,
    inputEncoders = Seq(Some(inputEncoder1), Some(inputEncoder2)),
    outputEncoder = Some(outputEncoder),
    givenName = Some(name)
  )
}

def udf[T: AgnosticEncoder, U: AgnosticEncoder](f: T => U): UserDefinedFunction = {
  val inputEncoder = summon[AgnosticEncoder[T]]
  val outputEncoder = summon[AgnosticEncoder[U]]
  SparkUserDefinedFunction(
    f = f,
    dataType = outputEncoder.dataType,
    inputEncoders = Seq(Some(inputEncoder)),
    outputEncoder = Some(outputEncoder)
  )
}

def udf[U: AgnosticEncoder](f: () => U, name: Option[String]): UserDefinedFunction = {
  val outputEncoder = summon[AgnosticEncoder[U]]
  SparkUserDefinedFunction(
    f = f,
    outputEncoder.dataType,
    inputEncoders = Seq.empty,
    outputEncoder = Some(outputEncoder),
    givenName = name
  )
}
