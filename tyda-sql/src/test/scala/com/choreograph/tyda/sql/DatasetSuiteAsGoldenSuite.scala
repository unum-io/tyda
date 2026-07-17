package com.choreograph.tyda.sql
import org.scalactic.Equality

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.testsuites.DatasetSuite
import com.choreograph.tyda.unreachable

trait DatasetSuiteAsGoldenSuite extends DatasetSuite, SqlGoldenTestSuite {

  private val tablePrefix = "table"

  override def reference: Runner = unreachable("Should only generate SQL, not run datasets")
  override def implementation: Runner = unreachable("Should only generate SQL, not run datasets")

  override def test[T: Arbitrary: Codec, R: Equality](
      name: String,
      computation: Dataset[T] => Dataset[R],
      inputs: Seq[T]*
  ): Unit = {
    testSqlOrSkip(name) { computation(Dataset.readTable[T, EmptyTuple](s"${tablePrefix}1").select(_._1)) }
  }

  override def test[T1: Arbitrary: Codec, T2: Arbitrary: Codec, R](
      name: String,
      computation: (Dataset[T1], Dataset[T2]) => Dataset[R]
  ): Unit =
    testSqlOrSkip(name) {
      computation(
        Dataset.readTable[T1, EmptyTuple](s"${tablePrefix}1").select(_._1),
        Dataset.readTable[T2, EmptyTuple](s"${tablePrefix}2").select(_._1)
      )
    }

  override def test[T1: Arbitrary: Codec, T2: Arbitrary: Codec, T3: Arbitrary: Codec, R](
      name: String,
      computation: (Dataset[T1], Dataset[T2], Dataset[T3]) => Dataset[R]
  ): Unit =
    testSqlOrSkip(name) {
      computation(
        Dataset.readTable[T1, EmptyTuple](s"${tablePrefix}1").select(_._1),
        Dataset.readTable[T2, EmptyTuple](s"${tablePrefix}2").select(_._1),
        Dataset.readTable[T3, EmptyTuple](s"${tablePrefix}3").select(_._1)
      )
    }

  override def testFailure[T: Codec, R](
      name: String,
      input: Seq[T],
      computation: Dataset[T] => Dataset[R],
      expectedError: String
  ): Unit = {
    testSqlOrSkip(name) { computation(Dataset.readTable[T, EmptyTuple](s"${tablePrefix}1").select(_._1)) }
  }

  override def testOrdered[T: Arbitrary: Codec, R: Equality](
      name: String,
      computation: Dataset[T] => Dataset[R],
      inputs: Seq[T]*
  ): Unit =
    testSqlOrSkip(name) { computation(Dataset.readTable[T, EmptyTuple](s"${tablePrefix}1").select(_._1)) }

  override def testOrdered[T1: Arbitrary: Codec, T2: Arbitrary: Codec, R: Equality](
      name: String,
      computation: (Dataset[T1], Dataset[T2]) => Dataset[R]
  ): Unit =
    testSqlOrSkip(name) {
      computation(
        Dataset.readTable[T1, EmptyTuple]("t1").select(_._1),
        Dataset.readTable[T2, EmptyTuple]("t2").select(_._1)
      )
    }
}
