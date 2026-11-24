package com.choreograph.tyda.bigquery

import org.scalatest.ParallelTestExecution

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Runner
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.testsuites.DatasetAggregatesSuite
import com.choreograph.tyda.testsuites.DatasetBasicSuite
import com.choreograph.tyda.testsuites.DatasetJoinSuite
import com.choreograph.tyda.testsuites.DatasetOrderBySuite
import com.choreograph.tyda.testsuites.DatasetSubquerySuite
import com.choreograph.tyda.testsuites.DatasetSuite
import com.choreograph.tyda.testsuites.ExprEvaluationSuite

private trait WithGoogleSqlTestRunner {
  def runner: GoogleSqlTestRunner = GoogleSqlTestRunner.configuredOrSkip
}

private trait GoogleSqlSuiteRunner extends DatasetSuite, WithGoogleSqlTestRunner, ParallelTestExecution {
  def reference: Runner = IteratorRunner
  def implementation: Runner = runner
}

class DatasetBasicSuiteGoogleSql extends DatasetBasicSuite, GoogleSqlSuiteRunner
class DatasetJoinSuiteGoogleSql extends DatasetJoinSuite, GoogleSqlSuiteRunner
class DatasetAggregatesSuiteGoogleSql extends DatasetAggregatesSuite, GoogleSqlSuiteRunner
class DatasetSubquerySuiteGoogleSql extends DatasetSubquerySuite, GoogleSqlSuiteRunner
class DatasetOrderBySuiteGoogleSql extends DatasetOrderBySuite, GoogleSqlSuiteRunner

class ExprEvaluationSuiteGoogleSql
    extends ExprEvaluationSuite, WithGoogleSqlTestRunner, ParallelTestExecution {
  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] =
    runner.collect(Dataset.from(values).select(expr))

  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String =
    runner.sql(Dataset.from(values).select(expr)).single
}
