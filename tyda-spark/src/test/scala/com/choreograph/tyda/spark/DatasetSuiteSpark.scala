package com.choreograph.tyda.spark

import com.choreograph.tyda.Format
import com.choreograph.tyda.Runner
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.testsuites.DatasetAggregatesSuite
import com.choreograph.tyda.testsuites.DatasetBasicSuite
import com.choreograph.tyda.testsuites.DatasetJoinSuite
import com.choreograph.tyda.testsuites.DatasetOrderBySuite
import com.choreograph.tyda.testsuites.DatasetReadBigQueryTableSuite
import com.choreograph.tyda.testsuites.DatasetReadWriteSuite
import com.choreograph.tyda.testsuites.DatasetSubquerySuite
import com.choreograph.tyda.testsuites.DatasetSuite

private trait SparkSuiteRunner extends DatasetSuite, SharedSparkSession {
  override def reference: Runner = IteratorRunner
  def implementation: Runner = new SparkRunner(using spark)
}

class DatasetBasicSuiteSpark extends DatasetBasicSuite, SparkSuiteRunner
class DatasetJoinSuiteSpark extends DatasetJoinSuite, SparkSuiteRunner
class DatasetAggregatesSuiteSpark extends DatasetAggregatesSuite, SparkSuiteRunner
class DatasetSubquerySuiteSpark extends DatasetSubquerySuite, SparkSuiteRunner
class DatasetOrderBySuiteSpark extends DatasetOrderBySuite, SparkSuiteRunner
class DatasetReadWriteParquetSuiteSpark extends DatasetReadWriteSuite, SparkSuiteRunner {
  override def format = Format.Parquet
}
class DatasetReadWriteJsonSuiteSpark extends DatasetReadWriteSuite, SparkSuiteRunner {
  override def format = Format.Json
}
class DatasetReadBigQueryTableSuiteSpark extends DatasetReadBigQueryTableSuite, SparkSuiteRunner
