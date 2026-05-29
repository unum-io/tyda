package com.choreograph.tyda.bigquery

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs

class BigQueryRunnerSpec extends AnyFunSuite {
  test("createRunner returns BigQueryRunner") {
    val runner = RunnerArgs.createRunner(RunnerArgs.BigQuery("test-project-id"), "test-app")
    assert(runner.isInstanceOf[BigQueryRunner])
  }
}
