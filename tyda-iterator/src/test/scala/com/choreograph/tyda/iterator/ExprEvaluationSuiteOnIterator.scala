package com.choreograph.tyda.iterator

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Expr
import com.choreograph.tyda.testsuites.ExprEvaluationSuite

class ExprEvaluationSuiteOnIterator extends ExprEvaluationSuite {
  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] = {
    val eval = ExprEvaluation.lambda(expr)
    values.iterator.map(eval).toSeq
  }

  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String =
    com.choreograph.tyda.iterator.explain(CompiledExpr(expr))

  test("Support creating lambda from Expr lambda with 1 arg") {
    val extract = ExprEvaluation.lambda[(Int, Int), Int](_._1)
    assert(extract((1, 2)) == 1)
  }
  test("Support creating lambda from Expr lambda with 2 args") {
    val gt = ExprEvaluation.lambda2[Int, Int, Boolean](_ > _)
    assert(!gt(1, 2))
    assert(gt(2, 1))
  }
}
