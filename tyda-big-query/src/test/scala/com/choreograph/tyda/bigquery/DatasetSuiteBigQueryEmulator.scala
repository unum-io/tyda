package com.choreograph.tyda.bigquery

import scala.jdk.CollectionConverters.*

import com.google.cloud.NoCredentials
import com.google.cloud.bigquery.BigQuery
import com.google.cloud.bigquery.BigQueryOptions
import com.google.cloud.bigquery.DataFormatOptions
import com.google.cloud.bigquery.QueryJobConfiguration
import org.scalatest.Assertions.pending

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Runner
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.sql.DatasetToSqlError
import com.choreograph.tyda.sql.SqlDialect
import com.choreograph.tyda.sql.toSql
import com.choreograph.tyda.testsuites.BigQueryIntegrationTestEnvVariables
import com.choreograph.tyda.testsuites.DatasetAggregatesSuite
import com.choreograph.tyda.testsuites.DatasetBasicSuite
import com.choreograph.tyda.testsuites.DatasetJoinSuite
import com.choreograph.tyda.testsuites.DatasetSubquerySuite
import com.choreograph.tyda.testsuites.DatasetSuite
import com.choreograph.tyda.testsuites.ExprEvaluationSuite
import com.choreograph.tyda.unreachable
import com.google.cloud.bigquery.JobInfo
import com.google.cloud.bigquery.BigQueryException
import com.google.cloud.bigquery.Job
import com.google.cloud.bigquery.JobStatistics.QueryStatistics

/** A Runner backed by the bigquery-emulator. Generates BigQuery SQL locally and
  * submits it to the emulator for execution, allowing the results to be
  * compared against the reference (IteratorRunner) implementation.
  *
  * This validates that the locally generated BigQuery SQL is semantically
  * correct BigQuery SQL.
  */
class BigQueryEmulatorRunner(bigQuery: BigQuery) extends Runner {
  import BigQueryEmulatorRunner.formatPlan

  def sql(ds: Dataset[?] | Dataset.Action): String =
    toSql(ds, SqlDialect.BigQuery) match {
      case Left(DatasetToSqlError.RequiresUdfCapability(_)) =>
        pending
        unreachable("Test should be skipped by pending")
      case Left(DatasetToSqlError.NotImplemented(msg)) =>
        pending
        unreachable(s"Unimplemented feature: $msg")
      case Right(sql) => sql
    }

  def collect[T](ds: Dataset[T]): Seq[T] = {
    val queryConfig = QueryJobConfiguration.newBuilder(sql(BigQueryCollectionRewrites.rewrite(ds))).build()
    val result = bigQuery.query(queryConfig)
    assert(!(result.getSchema == null), "Query did not return a schema; unable to decode results.")
    result.iterateAll().asScala.map(createDecoder(ds.codec, result.getSchema().getFields())).toSeq
  }

  def execute(ds: Dataset.Action): Unit = {
    val queryConfig = QueryJobConfiguration.newBuilder(sql(ds)).build()
    bigQuery.query(queryConfig): Unit
  }

  def explain[T](ds: Dataset[T]): String = explainImpl(ds)
  def explain(action: Dataset.Action): String = explainImpl(action)

  private def explainImpl[T](ds: Dataset[T] | Dataset.Action): String = {
    val sqlStr = sql(ds)
    val queryConfig = QueryJobConfiguration.newBuilder(sqlStr).setDryRun(true).build()
    val planOrError =
      try formatPlan(bigQuery.create(JobInfo.of(queryConfig)))
      // We should return an explain string even if the query is invalid.
      catch case e: BigQueryException => e.getMessage()
    s"""SQL:
       |$sqlStr
       |
       |Query plan:
       |$planOrError
       |""".stripMargin
  }
}

object BigQueryEmulatorRunner {

  /** Builds a runner connected to a bigquery-emulator instance at the given
    * HTTP endpoint (e.g. `http://localhost:9050`) using the given project ID.
    */
  def apply(connection: BigQueryEmulatorContainer.EmulatorConnection): BigQueryEmulatorRunner = {
    // The default config has rounding bugs: https://github.com/googleapis/java-bigquery/issues/1648
    val dataFormatOptions = DataFormatOptions.newBuilder().useInt64Timestamp(true).build()
    val client = BigQueryOptions
      .newBuilder()
      .setProjectId(connection.projectId)
      .setHost(connection.endpoint)
      .setCredentials(NoCredentials.getInstance())
      .setDataFormatOptions(dataFormatOptions)
      .build()
      .getService
    new BigQueryEmulatorRunner(client)
  }

  private def formatPlan(job: Job): String = {
    val statistics = job.getStatistics[QueryStatistics]()
    Option(statistics.getQueryPlan)
      .map(
        _.asScala
          .map { stage =>
            val steps = stage
              .getSteps()
              .asScala
              .map(step => s"${step.getName()} ${step.getSubsteps().asScala.mkString(", ")}")
              .mkString("\n  ")
            s"${stage.getName()}\n  $steps"
          }
          .mkString("\n")
      )
      .getOrElse("No query plan available")
  }
}

/** Mixin that provides a `BigQueryEmulatorRunner` by connecting to the
  * bigquery-emulator. The emulator is started automatically via Docker if
  * `TYDA_BQ_EMULATOR_ENDPOINT` is not set.
  *
  * Tests are skipped when `TYDA_BIGQUERY_TEST_PROJECT_ID` is set, deferring to
  * the real BigQuery integration suite in that case. If neither Docker nor a
  * pre-started emulator endpoint is available, tests are also skipped.
  */
trait WithBigQueryEmulator {
  private lazy val _connection: BigQueryEmulatorContainer.EmulatorConnection = {
    // TODO: Can be skipped but are still marked as failed, so should probably skip elsewhere.
    BigQueryIntegrationTestEnvVariables.skipIfProjectIsSet()
    val conn = BigQueryEmulatorContainer.connectOrSkip()
    conn match {
      case c: BigQueryEmulatorContainer.StartedContainer =>
        Runtime.getRuntime.addShutdownHook(Thread(() => c.close()))
      case _ =>
    }
    conn
  }

  lazy val emulatorRunner: BigQueryEmulatorRunner = BigQueryEmulatorRunner(_connection)
}

private trait BigQueryEmulatorSuiteRunner extends DatasetSuite, WithBigQueryEmulator {
  def reference: Runner = IteratorRunner
  def implementation: Runner = emulatorRunner
}

class DatasetBasicSuiteBigQueryEmulator extends DatasetBasicSuite, BigQueryEmulatorSuiteRunner
class DatasetJoinSuiteBigQueryEmulator extends DatasetJoinSuite, BigQueryEmulatorSuiteRunner
class DatasetAggregatesSuiteBigQueryEmulator extends DatasetAggregatesSuite, BigQueryEmulatorSuiteRunner
class DatasetSubquerySuiteBigQueryEmulator extends DatasetSubquerySuite, BigQueryEmulatorSuiteRunner

// TODO:
// abstract class DatasetReadWriteSuiteBigQuery extends DatasetReadWriteSuite, BigQuerySuiteRunner class
// DatasetReadWriteParquetSuiteBigQuery extends DatasetReadWriteSuiteBigQuery class
// DatasetReadWriteJsonSuiteBigQuery extends DatasetReadWriteSuiteBigQuery class
// DatasetReadBigQueryTableSuiteBigQuery extends DatasetReadBigQueryTableSuite, BigQuerySuiteRunner

class ExprEvaluationSuiteBigQueryEmulator extends ExprEvaluationSuite, WithBigQueryEmulator {
  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] =
    emulatorRunner.collect(Dataset.from(values).select(expr))
  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String =
    emulatorRunner.sql(Dataset.from(values).select(expr))
}
