package com.choreograph.tyda.sql
import org.scalactic.Equality

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.testsuites.DatasetReadWriteSuite
import com.choreograph.tyda.unreachable

object DatasetReadWriteSuiteAsGoldenSuite {
  private val GoldenWritePath = "/tmp/dataset-read-write"
}

trait DatasetReadWriteSuiteAsGoldenSuite extends SqlGoldenTestSuite, DatasetReadWriteSuite {
  import DatasetReadWriteSuiteAsGoldenSuite.GoldenWritePath

  override def includeReadTests: Boolean = false
  override def includeOverwriteTests: Boolean = false
  override def reference: Runner = unreachable("Should only generate SQL, not run datasets")
  override def implementation: Runner = unreachable("Should only generate SQL, not run datasets")

  override def testReadWrite[T: Arbitrary: Codec: TypeName: Equality]: Unit = {
    testSqlOrSkip(s"write ${TypeName.name[T]}") {
      Dataset.readTable[T, EmptyTuple]("t1").select(_._1).writeToPath(GoldenWritePath, format)
    }
    if !includeReadTests then return ()
    testSqlOrSkip(s"read ${TypeName.name[T]}") { Dataset.read(GoldenWritePath, format, false, "*.parquet") }
  }

  override def testReadExtended[Old: Arbitrary: Codec: TypeName, New: Codec: TypeName: Equality](
      _update: Old => New
  ): Unit = ()
}
