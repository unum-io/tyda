package com.choreograph.tyda.testsuites

final class GoldenTestSuiteExample extends GoldenTestSuite {

  goldenTest("simple addition") { (1 + 1).toString }

  goldenTest("string concatenation") { "hello" + " " + "world" }

  goldenTest("multi-line output") {
    """line 1
      |line 2
      |line 3""".stripMargin
  }

  goldenTest("empty output") { "" }

  goldenTest("complex calculation") {
    val list = List(1, 2, 3, 4, 5)
    val sum = list.sum
    val product = list.product
    s"Sum: $sum, Product: $product"
  }
}
