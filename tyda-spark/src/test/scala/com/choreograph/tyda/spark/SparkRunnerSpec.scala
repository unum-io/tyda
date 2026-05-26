package com.choreograph.tyda.spark

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.RunnerArgs

class SparkRunnerSpec extends AnyFunSuite with SharedSparkSession {
  test("createRunner returns SparkRunner") {
    // Force shared spark session initialization before calling createRunner
    // This should be replaced by making RunnerArg.Spark contain the neccessary args.
    val _ = spark
    val runner = RunnerArgs.createRunner(RunnerArgs.Spark, "test-app")
    assert(runner.isInstanceOf[SparkRunner])
  }
}
