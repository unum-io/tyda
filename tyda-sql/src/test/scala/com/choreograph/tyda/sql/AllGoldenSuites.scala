package com.choreograph.tyda.sql

import com.choreograph.tyda.Format
import com.choreograph.tyda.testsuites.DatasetAggregatesSuite
import com.choreograph.tyda.testsuites.DatasetBasicSuite
import com.choreograph.tyda.testsuites.DatasetJoinSuite
import com.choreograph.tyda.testsuites.DatasetOrderBySuite
import com.choreograph.tyda.testsuites.DatasetSubquerySuite

trait SparkSqlGoldenSuite {
  self: SqlGoldenTestSuite =>
  def dialect = SqlDialect.Spark
}
class UnparserSparkSqlGoldenSuite extends UnparserSuite, SparkSqlGoldenSuite
class ExprEvaluationSparkSqlGoldenSuite extends ExprEvaluationSuiteAsGoldenSuite, SparkSqlGoldenSuite
class DatasetAggregatesSparkSqlGoldenSuite
    extends DatasetSuiteAsGoldenSuite, SparkSqlGoldenSuite, DatasetAggregatesSuite
class DatasetBasicSparkSqlGoldenSuite
    extends DatasetSuiteAsGoldenSuite, SparkSqlGoldenSuite, DatasetBasicSuite
class DatasetJoinSparkSqlGoldenSuite extends DatasetSuiteAsGoldenSuite, SparkSqlGoldenSuite, DatasetJoinSuite
class DatasetSubquerySparkSqlGoldenSuite
    extends DatasetSuiteAsGoldenSuite, SparkSqlGoldenSuite, DatasetSubquerySuite
class DatasetOrderBySparkSqlGoldenSuite
    extends DatasetSuiteAsGoldenSuite, SparkSqlGoldenSuite, DatasetOrderBySuite
class DatasetReadWriteParquetSparkSqlGoldenSuite
    extends DatasetReadWriteSuiteAsGoldenSuite, SparkSqlGoldenSuite {
  override def format = Format.Parquet
}
class DatasetReadWriteJsonSparkSqlGoldenSuite
    extends DatasetReadWriteSuiteAsGoldenSuite, SparkSqlGoldenSuite {
  override def format = Format.Json
}

trait BigQueryGoldenSuite extends SqlGoldenTestSuite {
  def dialect = SqlDialect.BigQuery
}
class UnparserBigQueryGoldenSuite extends UnparserSuite, BigQueryGoldenSuite
class ExprEvaluationBigQueryGoldenSuite extends ExprEvaluationSuiteAsGoldenSuite, BigQueryGoldenSuite
class DatasetAggregatesBigQueryGoldenSuite
    extends DatasetSuiteAsGoldenSuite, BigQueryGoldenSuite, DatasetAggregatesSuite
class DatasetBasicBigQueryGoldenSuite
    extends DatasetSuiteAsGoldenSuite, BigQueryGoldenSuite, DatasetBasicSuite
class DatasetJoinBigQueryGoldenSuite extends DatasetSuiteAsGoldenSuite, BigQueryGoldenSuite, DatasetJoinSuite
class DatasetSubqueryBigQueryGoldenSuite
    extends DatasetSuiteAsGoldenSuite, BigQueryGoldenSuite, DatasetSubquerySuite
class DatasetOrderByBigQueryGoldenSuite
    extends DatasetSuiteAsGoldenSuite, BigQueryGoldenSuite, DatasetOrderBySuite
class DatasetReadWriteParquetBigQueryGoldenSuite
    extends DatasetReadWriteSuiteAsGoldenSuite, BigQueryGoldenSuite {
  override def format = Format.Parquet
}
class DatasetReadWriteJsonBigQueryGoldenSuite
    extends DatasetReadWriteSuiteAsGoldenSuite, BigQueryGoldenSuite {
  override def format = Format.Json
}
