package com.choreograph.tyda.sql

import scala.reflect.ClassTag
import scala.util.matching.Regex

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.testsuites.ExprEvaluationSuiteBase
import com.choreograph.tyda.unreachable

trait ExprEvaluationSuiteAsGoldenSuite extends SqlGoldenTestSuite, ExprEvaluationSuiteBase {
  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] =
    unreachable("Should only generate SQL, not evaluate expressions")
  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String =
    unreachable("Should only generate SQL, not evaluate expressions")
  override def testFailure[From: {Arbitrary, Codec}, To](
      name: String,
      expr: Expr[From] => Expr[To],
      messages: (String | Regex)*
  ) = testSqlOrSkip(name) { Dataset.readTable[From, EmptyTuple]("t1").select(_._1).select(expr) }

  override def testHasSameBehavior[From: ClassTag: Codec: Arbitrary, To](
      name: String,
      expr: Expr[From] => Expr[To],
      expected: From => To
  ): Unit = testSqlOrSkip(name) { Dataset.readTable[From, EmptyTuple]("t1").select(_._1).select(expr) }
}
