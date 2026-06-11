package com.choreograph.tyda.fuzz

import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Arbitrary.Shrinkable
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.iterator.ExprEvaluation
import com.choreograph.tyda.iterator.ExprEvaluation.LimitException
import com.choreograph.tyda.iterator.ExprEvaluation.Limits
import com.choreograph.tyda.iterator.explain
import com.choreograph.tyda.unreachable

object FuzzExprSuite {}

trait FuzzExprSuite extends AnyFunSuite {
  def reference[From, To](expr: CompiledExpr[From, To]): From => To =
    ExprEvaluation.lambda(expr, Limits(maxRange = 1000))
  def referenceExplain[From, To](expr: CompiledExpr[From, To]): String = explain(expr)

  def implementation[From, To](expr: CompiledExpr[From, To]): From => To
  def implementationExplain[From, To](expr: CompiledExpr[From, To]): String

  def knownBug[From, To](expr: CompiledExpr[From, To]): Boolean = false

  private def singleFuzz[From: Arbitrary: Codec, To: Codec](using r: Random): Unit = {
    type Trial = (input: Seq[From], compiled: CompiledExpr[From, To])

    val candidate: Shrinkable[Trial] =
      for
        values <- Arbitrary[Seq[From]].shrinkable(r)
        compiled <- GenExprNode.genCompiledExpr[From, To](Seq.empty, 0)
      yield (values, compiled)

    if knownBug(candidate.value.compiled) then pending: Unit

    def explains(trial: Trial): String =
      s"""Reference explain:
     |${referenceExplain(trial.compiled)}
     |Implementation explain:
     |${implementationExplain(trial.compiled)}""".stripMargin

    def evaluate(trial: Trial): Option[String] = {
      def runAll(f: => From => To): Either[Throwable, Seq[To]] =
        try Right(trial.input.map(f))
        catch {
          // If we hit evalation limit we skip the expression
          case LimitException(_) =>
            pending
            unreachable("pending skipped test")
          case e: Throwable => Left(e)
        }

      val referenceEval = runAll(reference(trial.compiled))
      val implementationEval = runAll(implementation(trial.compiled))

      (referenceEval, implementationEval) match {
        case (Right(expected), Right(actual)) =>
          Option.when(expected != actual)(s"""Backends produced different results.
           |Input:                  ${trial.input.mkString(", ")}
           |
           |Reference results:      ${expected.mkString(", ")}
           |Implementation results: ${actual.mkString(", ")}
           |
           |${explains(trial)}""".stripMargin)
        case (Left(_), Left(_)) => None
        case (Right(expected), Left(e)) =>
          Some(s"""Reference succeeded but implementation threw ${e.getClass.getSimpleName}: ${e.getMessage}
               |
               |Input:             ${trial.input.mkString(", ")}
               |Reference results: ${expected.mkString(", ")}
               |
               |${explains(trial)}""".stripMargin)
        case (Left(e), Right(actual)) =>
          Some(s"""Implementation succeeded but reference threw ${e.getClass.getSimpleName}: ${e.getMessage}
               |
               |Input:                  ${trial.input.mkString(", ")}
               |Implementation results: ${actual.mkString(", ")}
               |
               |${explains(trial)}""".stripMargin)
      }
    }

    evaluate(candidate.value) match {
      case None => () // passed
      case Some(firstMessage) =>
        alert(s"Found failing case:\n$firstMessage\n\nShrinking...")
        val minimal = candidate.minimize(evaluate(_).isDefined)
        evaluate(minimal) match {
          case Some(message) => fail(s"Minimal failing case:\n$message")
          case None =>
            fail(s"Failure did not reproduce after shrinking (non-deterministic backend?).\n$firstMessage")
        }
    }
  }

  def fuzz[From: Arbitrary: Codec, To: Codec](trails: Int = 1000) = (0 until trails).foreach(i =>
    test(s"fuzz ${Codec[From]} to ${Codec[To]} [seed=$i]") {
      given Random = Random(i + 1000)
      singleFuzz[From, To]
    }
  )

  fuzz[Boolean, Boolean]()
  fuzz[Int, Int]()
  fuzz[Long, Long]()
  fuzz[Decimal[38, 9], Decimal[38, 9]]()
  fuzz[String, String]()
  fuzz[Int, Option[Int]]()
  fuzz[String, Option[String]]()
  fuzz[Int, Seq[Int]]()
  fuzz[Seq[Int], Seq[Int]]()
  {
    given [T: Codec]: Codec[Iterable[T]] = Codec.Iterable(summon, summon)
    fuzz[Seq[Int], Iterable[Int]]()
    fuzz[Int, Iterable[(Int, Int)]]()
  }
  fuzz[Seq[Long], Seq[Int]]()
  fuzz[Seq[String], Seq[Option[Int]]]()
  fuzz[(a: Int, b: Int), Seq[Int]]()
  fuzz[(a: Int, b: Int), (Int, Int)]()
}
