package com.choreograph.tyda.bigquery

import scala.annotation.unused
import scala.jdk.CollectionConverters.*

import com.google.cloud.bigquery.BigQuery
import com.google.cloud.bigquery.BigQueryException
import com.google.cloud.bigquery.BigQueryOptions
import com.google.cloud.bigquery.BigQueryRetryConfig
import com.google.cloud.bigquery.DataFormatOptions
import com.google.cloud.bigquery.Job
import com.google.cloud.bigquery.JobInfo
import com.google.cloud.bigquery.JobStatistics.QueryStatistics
import com.google.cloud.bigquery.QueryJobConfiguration
import com.google.cloud.bigquery.TableDefinition
import com.google.cloud.bigquery.TableId
import org.slf4j.LoggerFactory

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.RunnerArgs.ValidateSchema
import com.choreograph.tyda.bigquery.BigQueryRunner.buildClient
import com.choreograph.tyda.sql.SqlDialect
import com.choreograph.tyda.sql.toSql

class BigQueryRunner(args: RunnerArgs.BigQuery) extends Runner {
  import BigQueryRunner.{logger, formatPlan}
  val bigQuery = buildClient(args.projectId)

  def sql(ds: Dataset[?] | Dataset.Action): String =
    toSql(ds, SqlDialect.BigQuery) match {
      case Right(sql) => sql
      case Left(err) => throw new RuntimeException(s"Failed to unparse dataset to SQL: $err")
    }

  def execute(ds: Dataset.Action): Unit = {
    validateReadTableSchemas(ds, args.validateSchemas)
    val query = sql(ds)
    logger.info(s"Executing query:\n$query")
    val queryConfig = QueryJobConfiguration.newBuilder(query).build()
    // Do we need error handling here, or will those always result in exceptions?
    try bigQuery.query(queryConfig, BigQueryRunner.retryJobOption): Unit
    catch
      case e: BigQueryException =>
        throw new RuntimeException(s"Failed to execute query:\n${explain(ds)}\nError: ${e.getMessage}", e)
  }

  def collect[T](ds: Dataset[T]): Seq[T] = {
    validateReadTableSchemas(ds, args.validateSchemas)
    val queryConfig = QueryJobConfiguration.newBuilder(sql(BigQueryCollectionRewrites.rewrite(ds))).build()
    val result = bigQuery.query(queryConfig, BigQueryRunner.retryJobOption)
    assert(!(result.getSchema == null), "Query did not return a schema unable to decode results.")
    result.iterateAll().asScala.map(createDecoder(ds.codec, result.getSchema().getFields())).toSeq
  }

  def explain[T](ds: Dataset[T]): String = explainImpl(BigQueryCollectionRewrites.rewrite(ds))
  def explain(action: Dataset.Action): String = explainImpl(action)

  /** Provides a human-readable explanation of the execution plan for the given
    * dataset.
    */
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
       |Query Plan:
       |$planOrError
       |""".stripMargin
  }

  private def extractTableId(identifier: String): Option[TableId] =
    identifier.split('.') match {
      case Array(project, dataset, table) => Some(TableId.of(project, dataset, table))
      case _ => None
    }

  private def isInvalidOrUncheckable(
      identifier: String,
      partitionCodec: Codec[?],
      modelCodec: Codec[?],
      log: String => Unit
  ): Boolean = {
    val id = extractTableId(identifier) match {
      case Some(tableId) => tableId
      case None =>
        log(s"Unable to extract dataset and table from identifier '$identifier', unable to validate schemas")
        return true
    }
    val table = bigQuery.getTable(id)
    if (table == null) {
      log(s"Table '$id' does not exist, unable to validate schemas")
      return true
    }
    val definition: TableDefinition = table.getDefinition()
    val schema = definition.getSchema()
    if (schema == null) {
      log(s"Table '$identifier' does not have a schema, unable to validate schemas")
      return true
    }
    val fields = schema.getFields()
    val errors = validateSchema(partitionCodec, fields) ++ validateSchema(modelCodec, fields)
    errors.foreach(e => log(e.formatted(identifier)))
    errors.nonEmpty
  }

  private def validateReadTableSchemas[T](
      ds: Dataset[T] | Dataset.Action,
      validateSchemas: ValidateSchema
  ): Unit = {
    val failOnSchemaIssue = validateSchemas match {
      case ValidateSchema.Off => return
      case ValidateSchema.Warn => false
      case ValidateSchema.Strict => true
    }
    def log(message: String): Unit = if (failOnSchemaIssue) logger.error(message) else logger.warn(message)
    val check: Dataset[?] => Boolean = _ match {
      case Dataset.ReadTable(identifier, _, partitionSchema, modelSchema) =>
        isInvalidOrUncheckable(identifier, partitionSchema, modelSchema, log)
      case _ => false
    }
    val foundError = ds match {
      case action: Dataset.Action => action.exists(check)
      case dataset: Dataset[T] => dataset.exists(check)
    }
    if (foundError && failOnSchemaIssue) {
      throw new RuntimeException("Schema validation errors found, see logs for details.")
    }
  }
}

object BigQueryRunner {
  private val logger = LoggerFactory.getLogger(getClass)

  private val retryJobOption: BigQuery.JobOption =
    val retryConfig = BigQueryRetryConfig
      .newBuilder()
      .retryOnMessage(
        "Visibility check was unavailable",
        "Error encountered during execution. Retrying may solve the problem."
      )
      .build()
    BigQuery.JobOption.bigQueryRetryConfig(retryConfig)

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

  /** Factory method for reflection-based runner creation. */
  def apply(
      @unused
      name: String,
      args: RunnerArgs.BigQuery
  ): Runner = {
    logger.info(s"Initializing BigQueryRunner with project ID: ${args.projectId}")
    new BigQueryRunner(args)
  }

  def buildClient(projectId: String): BigQuery = {
    // The default config has rounding bugs: https://github.com/googleapis/java-bigquery/issues/1648
    val dataFormatOptions = DataFormatOptions.newBuilder().useInt64Timestamp(true).build()
    BigQueryOptions
      .newBuilder()
      .setProjectId(projectId)
      .setDataFormatOptions(dataFormatOptions)
      .build()
      .getService

  }
}
