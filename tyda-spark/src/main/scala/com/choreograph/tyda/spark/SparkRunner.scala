package com.choreograph.tyda.spark

import scala.collection.immutable.ArraySeq

import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.execution.ExtendedMode

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs

class SparkRunner(using spark: SparkSession) extends Runner {
  def execute(action: Dataset.Action): Unit = DatasetOnSpark.perform(action)
  def collect[T](ds: Dataset[T]): Seq[T] = ArraySeq.unsafeWrapArray(DatasetOnSpark(ds).collect())
  def explain[T](ds: Dataset[T]): String = DatasetOnSpark(ds).queryExecution.explainString(ExtendedMode)
  def explain(action: Dataset.Action): String =
    action match {
      case Dataset.Action.Write(input, path, format) => "Write to {path} in format {format}:\n" +
          explain(input)
    }
}

object SparkRunner {
  extension (b: SparkSession.Builder) {
    private def withOpt[T](
        opt: Option[T],
        f: (SparkSession.Builder, T) => SparkSession.Builder
    ): SparkSession.Builder = opt.fold(b)(f(b, _))
  }

  def apply(name: String, args: RunnerArgs.Spark): SparkRunner =
    val spark = SparkSession
      .builder()
      .appName(name)
      .withOpt(args.master, _.master(_))
      .withOpt(args.logLevel.map(_.toSparkConf), _.config("spark.log.level", _))
      .getOrCreate()
    new SparkRunner(using spark)
}
