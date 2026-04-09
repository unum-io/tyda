package com.choreograph.tyda.sparksql

import org.apache.spark.sql.SparkSession
import org.scalatest.Assertions.fail
import org.scalatest.Assertions.pending

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Format
import com.choreograph.tyda.Runner
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.spark.CodecToEncoder.convert
import com.choreograph.tyda.sql.DatasetToSqlError
import com.choreograph.tyda.sql.SqlDialect
import com.choreograph.tyda.sql.toSql
import com.choreograph.tyda.testsuites.DatasetAggregatesSuite
import com.choreograph.tyda.testsuites.DatasetBasicSuite
import com.choreograph.tyda.testsuites.DatasetJoinSuite
import com.choreograph.tyda.testsuites.DatasetReadWriteSuite
import com.choreograph.tyda.testsuites.DatasetSubquerySuite
import com.choreograph.tyda.testsuites.DatasetSuite
import com.choreograph.tyda.testsuites.ExprEvaluationSuite
import com.choreograph.tyda.unreachable

trait SharedSparkSession {
  lazy given spark: SparkSession =
    SparkSession
      .builder()
      .master("local[2]")
      .appName("SparkSqlIntegrationSuite")
      .config("spark.log.level", "ERROR")
      .config("spark.sql.shuffle.partitions", "1")
      .config("spark.sql.parquet.outputTimestampType", "TIMESTAMP_MICROS")
      .config("spark.sql.parquet.datetimeRebaseModeInWrite", "CORRECTED")
      .config("spark.sql.parquet.datetimeRebaseModeInRead", "CORRECTED")
      .config("spark.sql.legacy.timeParserPolicy", "CORRECTED")
      .config("spark.sql.ansi.enabled", "true")
      /* By default Spark does variable substitution before parsing, this have obvious problems if the sql
       * contains any user controllable value. */
      .config("spark.sql.variable.substitute", "false")
      .getOrCreate()
}

class SparkSqlRunner(using spark: SparkSession) extends Runner {
  def sql(ds: Dataset[?] | Dataset.Action): String = {
    toSql(ds, SqlDialect.Spark) match {
      case Left(DatasetToSqlError.RequiresUdfCapability(msg)) =>
        // Using assume which cancels the test is counted as failure by some test runners like bloop
        /* so instaed we use pending. We should probably move specs that require UDFs to a separate suite
         * instead. */
        pending
        unreachable("Test should be skipped by pending")
      case Left(DatasetToSqlError.NotImplemented(msg)) => fail(s"Unimplemented feature: $msg")
      case Right(plan) => plan
    }
  }
  def collect[T](ds: Dataset[T]): Seq[T] = {
    given Codec[T] = ds.codec
    withAnsiMode { spark.sql(sql(ds)).as[T].collect().toSeq }
  }
  def execute(ds: Dataset.Action): Unit = withAnsiMode { spark.sql(sql(ds)): Unit }
  def explain[T](ds: Dataset[T]): String = sql(ds)
  def explain(action: Dataset.Action): String = sql(action)

  private def withAnsiMode[T](f: => T)(using spark: SparkSession): T = {
    val oldSetting = spark.conf.getOption("spark.sql.ansi.enabled")
    try {
      spark.conf.set("spark.sql.ansi.enabled", value = true)
      f
    } finally oldSetting.foreach(mode => spark.conf.set("spark.sql.ansi.enabled", mode))
  }
}

private trait SparkSuiteRunner extends DatasetSuite, SharedSparkSession {
  override def reference: Runner = IteratorRunner
  def implementation: Runner = SparkSqlRunner(using spark)
}

class DatasetBasicSuiteSparkSql extends DatasetBasicSuite, SparkSuiteRunner
class DatasetJoinSuiteSparkSql extends DatasetJoinSuite, SparkSuiteRunner
class DatasetAggregatesSuiteSparkSql extends DatasetAggregatesSuite, SparkSuiteRunner
class DatasetSubquerySuiteSparkSql extends DatasetSubquerySuite, SparkSuiteRunner
class DatasetReadWriteParquetSuiteSparkSql extends DatasetReadWriteSuite, SparkSuiteRunner {
  override def includeReadTests: Boolean = false
  override def format = Format.Parquet
}
class DatasetReadWriteJsonSuiteSparkSql extends DatasetReadWriteSuite, SparkSuiteRunner {
  override def includeReadTests: Boolean = false
  override def format = Format.Json
}

class ExprEvaluationSuiteSparkSql extends ExprEvaluationSuite, SharedSparkSession {
  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] =
    SparkSqlRunner(using spark).collect(Dataset.from(values).select(expr))
  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String =
    SparkSqlRunner(using spark).explain(Dataset.from(values).select(expr))
}
