package com.choreograph.tyda.sql
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.testsuites.GoldenTestSuite
import com.choreograph.tyda.unreachable

trait SqlGoldenTestSuite extends GoldenTestSuite {
  def dialect: SqlDialect
  private def toSqlOrFail(ds: Dataset[?] | Dataset.Action, dialect: SqlDialect): String =
    toSql(ds, dialect) match {
      case Right(sql) => sql
      case Left(err) => fail(s"Failed to unparse dataset to SQL: $err")
    }

  def testSql(name: String)(ds: Dataset[?] | Dataset.Action): Unit =
    goldenTest(name) { toSqlOrFail(ds, dialect) }

  def testSqlOrSkip(name: String)(ds: Dataset[?] | Dataset.Action): Unit =
    goldenTest(name) {
      toSql(ds, dialect) match {
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
