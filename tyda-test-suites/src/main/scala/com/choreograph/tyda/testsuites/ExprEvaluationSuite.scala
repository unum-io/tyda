package com.choreograph.tyda.testsuites

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.Random
import scala.util.Try
import scala.util.matching.Regex

import org.scalactic.Equality
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.functions.rand
import com.choreograph.tyda.functions.tuple

abstract class ExprEvaluationSuite extends AnyFunSuite, ExprEvaluationSuiteBase {
  private def exceptionFailureMessage[From: Codec, To](
      expr: Expr[From] => Expr[To],
      values: Seq[From],
      exception: Throwable
  ): String =
    s"""Evaluator threw an exception during execution:
       |${exception.getMessage}
       |
       |Explain:\n${explain(expr, values)}""".stripMargin
  private def comparisonFailureMessage[From: Codec, To](
      expr: Expr[From] => Expr[To],
      input: From,
      result: To,
      expected: To
  ): String = {
    val explainStr = explain(expr, Seq(input))
    s"Failed for input $input: got $result, expected $expected\nExplain:\n$explainStr"
  }

  private def evaluator[From: Codec, To](expr: Expr[From] => Expr[To]): From => To =
    from => evaluate(expr, Seq(from)).head

  def testHasSameBehavior[From: ClassTag: Codec: Arbitrary, To: Equality](
      name: String,
      expr: Expr[From] => Expr[To],
      expected: From => To
  ) =
    test(s"Same behavior test: $name") {
      val shrinkableValues = ArraySeq.fill(100)(Arbitrary[From].shrinkable(Random))
      val values = shrinkableValues.map(_.value)
      val exprWithIndex: Expr[(From, Int)] => Expr[(To, Int)] = { case Expr(value, index) =>
        tuple(expr(value), index)
      }
      def evalInOrder(input: Seq[From]): Seq[To] =
        evaluate(exprWithIndex, input.zipWithIndex).sortBy(_._2).map(_._1)
      val results =
        try evalInOrder(values)
        catch
          // TODO: Use ScalaTestControlException alias
          case e: (TestPendingException | TestCanceledException | TestFailedException) => throw e
          case e: Throwable =>
            alert(exceptionFailureMessage(expr, values, e) + "\n\nStarting to shrink failure...")
            val minimized = shrinkableValues.minimize(input => Try(evalInOrder(input)).isFailure)
            fail(exceptionFailureMessage(expr, minimized, e), e)
      val expectedResults = values.map(expected)
      assert(results.size == values.size, "Evaluator returned different number of values than expected")
      assert(results.size == expectedResults.size)
      shrinkableValues
        .zip(results.zip(expectedResults))
        .foreach { case (input, (result, expectedValue)) =>
          if result !== expectedValue then {
            alert(
              comparisonFailureMessage(expr, input.value, result, expectedValue) +
                "\n\nStarting to shrink failure..."
            )
            val eval = evaluator(expr)
            val minimized = input.minimize(v => eval(v) !== expected(v))
            fail(comparisonFailureMessage(expr, minimized, eval(minimized), expected(minimized)))
          }
        }
    }

  def testFailure[From: {Arbitrary, Codec}, To](
      name: String,
      expr: Expr[From] => Expr[To],
      messages: (String | Regex)*
  ) =
    test(s"Failure test: $name") {
      val value = Arbitrary[From]()
      val exception = intercept[Exception](evaluator(expr)(value))
      exception match {
        // TODO: Use ScalaTestControlException alias
        case e: (TestPendingException | TestCanceledException | TestFailedException) => throw e
        case _ =>
      }
      val exceptionMessage = Option(exception.getMessage) match {
        case Some(msg) => msg
        case None => fail(s"Exception did not have a message: $exception", exception)
      }
      messages.foreach {
        case msg: String => assert(
            exceptionMessage.contains(msg),
            s"Expected message '$msg' not found in '${exceptionMessage}'"
          )
        case regex: Regex => assert(
            regex.findFirstMatchIn(exceptionMessage).isDefined,
            s"Exception message '${exceptionMessage}' does not match regex '$regex'"
          )
      }
    }

  {
    val refNode = ExprNode.Reference[Int]()
    val ref: Expr[Int] = Expr.lift(refNode)
    val badExprFun: Expr[Int] => Expr[Int] = _ => ref
    testFailure[Int, Int](
      "throw on unexpected expr reference in failure test",
      badExprFun,
      refNode.toString,
      "This is likely caused by capturing"
    )
  }

  val constDelimiter = Arbitrary[String]()
  testHasSameBehavior[String, Seq[String]](
    "split string by const delimiter",
    v => v.split(constDelimiter),
    expectedSplit(_, constDelimiter)
  )

  test("split string by presented symbol") {
    val delimiter = Arbitrary[String]()
    val expr: Expr[String] => Expr[Seq[String]] = _.split(delimiter)
    val evalExpr = evaluator(expr)
    val str = (1 to 5).map(_ => Arbitrary[String]()).mkString(delimiter)
    assert(evalExpr(str) == expectedSplit(str, delimiter))
  }

  test("trim string ignores unicode whitespace") {
    val value = "\u2003x\u2003"
    assert(evaluator[String, String](_.trim())(value) == value)
  }

  test("trim string does not strip control chars") {
    val value = "a\u0005"
    assert(evaluator[String, String](_.trim())(value) == value)
  }

  test("rand returns a Double in [0.0, 1.0)") {
    val n = 100
    val expr: Expr[Int] => Expr[Double] = _ => rand()
    val results = evaluate(expr, Seq.fill(n)(0))
    assert(results.size == n, "Evaluator returned unexpected number of values")
    results.foreach { v =>
      assert(v >= 0.0, s"Expected rand() >= 0.0, but got $v")
      assert(v < 1.0, s"Expected rand() < 1.0, but got $v")
    }
  }

  test("rand is independently sampled per row") {
    val n = 100
    val expr: Expr[Int] => Expr[Double] = _ => rand()
    val results = evaluate(expr, Seq.fill(n)(0))
    // With 100 samples the probability that all values are identical is astronomically small
    assert(results.toSet.size > 1, "rand() should return distinct values across rows")
  }
}
