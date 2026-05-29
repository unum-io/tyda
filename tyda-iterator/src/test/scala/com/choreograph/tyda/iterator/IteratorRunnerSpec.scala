package com.choreograph.tyda.iterator

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs

class IteratorRunnerSpec extends AnyFunSuite {
  test("createRunner returns IteratorRunner") {
    val runner = RunnerArgs.createRunner(RunnerArgs.Iterator, "test-app")
    assert(runner eq IteratorRunner)
  }
}
