package com.choreograph.tyda.bigquery

import scala.reflect.ClassTag

import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.Path
import org.scalactic.Equality
import org.scalatest.Assertions.pending
import org.scalatest.ParallelTestExecution
import org.slf4j.LoggerFactory

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Codec.bigInt
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Format
import com.choreograph.tyda.NumericsReadMode
import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.rewrite.ArrayCodec
import com.choreograph.tyda.sql.DatasetToSqlError
import com.choreograph.tyda.sql.SqlDialect
import com.choreograph.tyda.sql.toSql
import com.choreograph.tyda.testsuites.BigQueryIntegrationTestEnvVariables
import com.choreograph.tyda.testsuites.DatasetAggregatesSuite
import com.choreograph.tyda.testsuites.DatasetBasicSuite
import com.choreograph.tyda.testsuites.DatasetJoinSuite
import com.choreograph.tyda.testsuites.DatasetOrderBySuite
import com.choreograph.tyda.testsuites.DatasetReadBigQueryTableSuite
import com.choreograph.tyda.testsuites.DatasetReadWriteSuite
import com.choreograph.tyda.testsuites.DatasetSubquerySuite
import com.choreograph.tyda.testsuites.DatasetSuite
import com.choreograph.tyda.testsuites.ExprEvaluationSuite
import com.choreograph.tyda.unreachable

class BigQueryTestRunner(args: RunnerArgs.BigQuery) extends BigQueryRunner(args) {
  override def sql(ds: Dataset[?] | Dataset.Action): String = {
    toSql(ds, SqlDialect.BigQuery) match {
      case Left(DatasetToSqlError.RequiresUdfCapability(msg)) =>
        pending
        unreachable("Test should be skipped by pending")
      case Left(DatasetToSqlError.NotImplemented(msg)) =>
        pending
        unreachable(s"Unimplemented feature: $msg")
      case Right(plan) => plan
    }
  }
}

trait WithConfiguredBigQueryTestRunner {
  def runner: BigQueryTestRunner = {
    val projectId = BigQueryIntegrationTestEnvVariables.getProjectIdOrSkip
    new BigQueryTestRunner(RunnerArgs.BigQuery(projectId, RunnerArgs.ValidateSchema.Off))
  }
}

private trait BigQuerySuiteRunner
    extends DatasetSuite, WithConfiguredBigQueryTestRunner, ParallelTestExecution {
  def reference: Runner = IteratorRunner
  def implementation: Runner = runner
}

class DatasetBasicSuiteBigQuery extends DatasetBasicSuite, BigQuerySuiteRunner
class DatasetJoinSuiteBigQuery extends DatasetJoinSuite, BigQuerySuiteRunner
class DatasetAggregatesSuiteBigQuery extends DatasetAggregatesSuite, BigQuerySuiteRunner
class DatasetSubquerySuiteBigQuery extends DatasetSubquerySuite, BigQuerySuiteRunner
class DatasetOrderBySuiteBigQuery extends DatasetOrderBySuite, BigQuerySuiteRunner
abstract class DatasetReadWriteSuiteBigQuery extends DatasetReadWriteSuite, BigQuerySuiteRunner {
  override def includeReadTests: Boolean = false

  override def tmpDir: String = BigQueryIntegrationTestEnvVariables.getTmpDirOrSkip

  override def cleanupPath(writeDir: String): Unit = {
    val path = Path(writeDir)
    val fs = path.getFileSystem(Configuration())
    if !fs.delete(path, true) then
      val logger = LoggerFactory.getLogger(this.getClass)
      logger.warn(s"Failed to delete path $writeDir")
  }

  private def supported[T](codec: Codec[T]): Boolean =
    Codec
      .iterate(codec)
      .forall {
        // No native support for maps
        case Codec.Map(_, _) => false
        // Nullable arrays are changed into empty arrays on write
        case Codec.Option(ArrayCodec(_)) => false
        // Nullable elements in arrays are not supported
        case ArrayCodec(Codec.Option(_)) => false
        // Nested arrays are not supported
        case ArrayCodec(ArrayCodec(_)) => false
        // We should consider dropping this and just use Decimal(38, 0)
        // But currently fails due to incorrect bytes handling in the bigquery dialect
        case `bigInt` => false
        case _ => true
      }
  override def numericsReadModeForWriteTests: NumericsReadMode = NumericsReadMode.WidenBigQuery
  override def testReadWrite[T: Arbitrary: Codec: TypeName: Equality]: Unit =
    if supported(Codec[T]) then super.testReadWrite[T]

  // Extra tests checking that we can read the output using the WidenBigQuery mode
  testReadWrite[(Int, Seq[(Byte, Long)])]
  {
    // Exclude NaN since we are comparing using universal equality
    given Arbitrary[Float] = Arbitrary[Float].filter(!_.isNaN)
    testReadWrite[(Float, Seq[(Float)])]
  }
}
class DatasetReadWriteParquetSuiteBigQuery extends DatasetReadWriteSuiteBigQuery {
  override def format: Format = Format.Parquet
}
class DatasetReadWriteJsonSuiteBigQuery extends DatasetReadWriteSuiteBigQuery {
  override def format: Format = Format.Json
}
class DatasetReadBigQueryTableSuiteBigQuery extends DatasetReadBigQueryTableSuite, BigQuerySuiteRunner

class ExprEvaluationSuiteBigQuery
    extends ExprEvaluationSuite, WithConfiguredBigQueryTestRunner, ParallelTestExecution {
  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] =
    runner.collect(Dataset.from(values).select(expr))
  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String =
    runner.sql(Dataset.from(values).select(expr))
}
