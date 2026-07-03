package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

class RunnerArgSpec extends AnyFunSuite {
  test("should throw helpful error when runner class is not on classpath") {
    val exception =
      intercept[RuntimeException] { RunnerArgs.createRunner(Arbitrary[RunnerArgs.Spark](), "test-app") }
    assert(exception.getMessage.contains("tyda-spark"))
    assert(exception.getMessage.contains("classpath"))
  }
}
