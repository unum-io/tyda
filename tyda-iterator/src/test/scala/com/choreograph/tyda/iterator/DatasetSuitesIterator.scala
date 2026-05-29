package com.choreograph.tyda.iterator

import com.choreograph.tyda.Format
import com.choreograph.tyda.Runner
import com.choreograph.tyda.testsuites.DatasetReadWriteSuite
import com.choreograph.tyda.testsuites.DatasetSuite

private trait IteratorSuiteRunner extends DatasetSuite {
  override def reference: Runner = IteratorRunner
  def implementation: Runner = IteratorRunner
}

class DatasetReadWriteParquetSuiteIterator extends DatasetReadWriteSuite, IteratorSuiteRunner {
  override def format = Format.Parquet
}
class DatasetReadWriteJsonSuiteIterator extends DatasetReadWriteSuite, IteratorSuiteRunner {
  override def format = Format.Json
}
