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
import com.google.cloud.bigquery.Schema
import com.google.cloud.bigquery.TableDefinition
import com.google.cloud.bigquery.TableId
import org.slf4j.LoggerFactory

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.RunnerArgs.ValidateSchema
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Control
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.bigquery.BigQueryRunner.buildClient
import com.choreograph.tyda.rewrite.Coercion
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

  def execute(action: Dataset.Action): Unit = {
    val coerced = validateAndCoerceReadTableSchemas(action, args.validateSchemas)
    val query = sql(coerced)
    logger.info(s"Executing query:\n$query")
    val queryConfig = QueryJobConfiguration.newBuilder(query).build()
    // Do we need error handling here, or will those always result in exceptions?
    try bigQuery.query(queryConfig, BigQueryRunner.retryJobOption): Unit
    catch
      case e: BigQueryException => throw new RuntimeException(
          s"Failed to execute query:\n${explain(coerced)}\nError: ${e.getMessage}",
          e
        )
  }

  def collect[T](ds: Dataset[T]): Seq[T] = {
    val coerced = validateAndCoerceReadTableSchemas(ds, args.validateSchemas)
    val queryConfig = QueryJobConfiguration
      .newBuilder(sql(BigQueryCollectionRewrites.rewrite(coerced)))
      .build()
    val result = bigQuery.query(queryConfig, BigQueryRunner.retryJobOption)
    assert(!(result.getSchema == null), "Query did not return a schema unable to decode results.")
    result.iterateAll().asScala.map(createDecoder(coerced.codec, result.getSchema().getFields())).toSeq
  }

  def explain[T](ds: Dataset[T]): String =
    explainImpl(
      validateAndCoerceReadTableSchemas(BigQueryCollectionRewrites.rewrite(ds), args.validateSchemas)
    )
  def explain(action: Dataset.Action): String =
    explainImpl(validateAndCoerceReadTableSchemas(action, args.validateSchemas))

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

  private def getSchema(identifier: String, log: String => Unit): Option[Schema] = {
    val id = extractTableId(identifier) match {
      case Some(tableId) => tableId
      case None =>
        log(s"Unable to extract dataset and table from identifier '$identifier', unable to validate schemas")
        return None
    }
    val table = bigQuery.getTable(id)
    if (table == null) {
      log(s"Table '$id' does not exist, unable to validate schemas")
      return None
    }
    val definition: TableDefinition = table.getDefinition()
    val schema = definition.getSchema()
    if (schema == null) {
      log(s"Table '$identifier' does not have a schema, unable to validate schemas")
      return None
    }
    Option(schema)
  }

  private def checkAndCoerce(
      validateSchemas: ValidateSchema
  ): [t] => (Boolean, Dataset[t]) => Control[(Boolean, Dataset[t])] = {
    val failOnSchemaIssue = validateSchemas match {
      case ValidateSchema.Off => return [t] => (acc, ds) => Continue(acc, ds)
      case ValidateSchema.Warn => false
      case ValidateSchema.Strict => true
    }

    def log(message: String): Unit = if (failOnSchemaIssue) logger.error(message) else logger.warn(message)
    def coerce[P, M](
        schema: Schema,
        foundError: Boolean,
        read: Dataset.ReadTable[P, M]
    ): Control[(Boolean, Dataset[(P, M)])] = {
      val physicalCodec = SchemaToCodec(schema)
      Coercion(physicalCodec, read.partitionCodec, read.modelCodec) match {
        case Coercion.Exact => Continue(foundError, read)
        case Coercion.Widen(cast) => Skip(
            foundError,
            read
              .copy(partitionCodec = Codec[EmptyTuple], modelCodec = physicalCodec.codec)
              .select(_._2)
              .select(cast)
          )
        case Coercion.Incompatible(errors) =>
          log(errors.fmt)
          Continue(true, read)
      }
    }

    [t] =>
      (foundError, ds) =>
        ds match {
          case read @ Dataset.ReadTable(identifier = identifier) =>
            getSchema(identifier, log).fold(Continue(true, ds))(coerce(_, foundError, read))
          case _ => Continue(foundError, ds)
        }
  }

  private def checkValidation(foundError: Boolean, validateSchemas: ValidateSchema): Unit =
    if (foundError && validateSchemas == ValidateSchema.Strict) {
      throw new RuntimeException("Schema validation errors found, see logs for details.")
    }

  private def validateAndCoerceReadTableSchemas[T](
      ds: Dataset[T],
      validateSchemas: ValidateSchema
  ): Dataset[T] = {
    val (foundError, coerced) = ds.transformAccumulateDown(false)(checkAndCoerce(validateSchemas))
    checkValidation(foundError, validateSchemas)
    coerced
  }
  private def validateAndCoerceReadTableSchemas(
      action: Dataset.Action,
      validateSchemas: ValidateSchema
  ): Dataset.Action = {
    val (foundError, coerced) = action.transformAccumulateDown(false)(checkAndCoerce(validateSchemas))
    checkValidation(foundError, validateSchemas)
    coerced
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
