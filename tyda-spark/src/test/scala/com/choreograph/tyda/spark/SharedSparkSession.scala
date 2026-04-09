package com.choreograph.tyda.spark

import org.apache.spark.sql.SparkSession

import com.choreograph.tyda.testsuites.BigQueryIntegrationTestEnvVariables

private[spark] trait SharedSparkSession {
  lazy given spark: SparkSession =
    // In the same jvm this will return the same SparkSession because of how getOrCreate works
    SparkSession
      .builder()
      .master("local[2]")
      .appName("SharedSparkSession")
      .config("spark.log.level", "ERROR")
      .config("spark.sql.shuffle.partitions", "1")
      .config("spark.sql.parquet.outputTimestampType", "TIMESTAMP_MICROS")
      .config("spark.sql.ansi.enabled", "true")
      // TODO: Remove this once we only use Spark >4.0.0 where this is the default
      .config("spark.sql.parquet.datetimeRebaseModeInWrite", "CORRECTED")
      .config("spark.sql.parquet.datetimeRebaseModeInRead", "CORRECTED")
      .config("spark.sql.legacy.timeParserPolicy", "CORRECTED")
      .getOrCreate()

  BigQueryIntegrationTestEnvVariables
    .getProjectId
    .foreach { projectId => spark.conf.set("spark.datasource.bigquery.parentProject", projectId) }

  // Based on what is done in the spark codebase
  /* https://github.com/apache/spark/blob/4e5ed454fb292bc22cbdb6fc69b7de322e0afeff/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/SQLConfHelper.scala#L34-L37 */
  //
  /* It quite ugly as it modifies the global state of the shared SparkSession, so depends no the test runner
   * not running individual test in a suite in parallel. */
  def withConf[T](pairs: (String, String)*)(f: => T): T = {
    val oldConf = pairs.map { case (k, _) => k -> spark.conf.get(k) }.toMap
    pairs.foreach { case (k, v) => spark.conf.set(k, v) }
    try f
    finally oldConf.foreach { case (k, v) => spark.conf.set(k, v) }
  }
}
