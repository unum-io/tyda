package com.choreograph.tyda.iterator

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Format
import com.choreograph.tyda.aggregates.max
import com.choreograph.tyda.aggregates.sum
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.functions.seq
import com.choreograph.tyda.functions.some
import com.choreograph.tyda.testsuites.GoldenTestSuite

class ExplainGoldenSuite extends GoldenTestSuite {
  val path = "/tmp/test"
  val root = Dataset.read[Int](path, Format.Parquet, false, "*")

  goldenTest("read") { explain(root) }

  goldenTest("read+filter") {
    val ds = root.where(_ < 10)
    explain(ds)
  }

  goldenTest("read+select+filter+join") {
    val rightPath = "/tmp/right"
    val right = Dataset.read[(Int, Int)](rightPath, Format.Parquet, false, "*")
    val ds = root.select(identity, _ == 1).join(right.where(_._2 < 1), (l, r) => l._1 == r._1)
    explain(ds)
  }

  goldenTest("expr boolean") {
    val ds = root.select(v => !(v == 1) && lit(false))
    explain(ds).linesIterator.next()
  }

  goldenTest("expr functions") {
    val ds = root.select(v => some(seq(v, lit(1))))
    explain(ds).linesIterator.next()
  }

  goldenTest("expr string functions") {
    val ds = Dataset.read[String](path, Format.Parquet, false, "*").select(_.trim())
    explain(ds).linesIterator.next()
  }

  goldenTest("aggregate") {
    val ds = root.aggregate(sum, max)
    explain(ds).linesIterator.next()
  }

  goldenTest("ternary") {
    val ds = root.select(v => Expr.ternary(v < 10, v, Expr.lit(0)))
    explain(ds).linesIterator.next()
  }

  goldenTest("map option outer reference") {
    val ds = root.select(v => some(v).map(_ < v))
    explain(ds).linesIterator.next()
  }

  goldenTest("map seq outer reference") {
    val ds = root.select(v => seq(v, v + 1).map(_ < v))
    explain(ds).linesIterator.next()
  }
}
