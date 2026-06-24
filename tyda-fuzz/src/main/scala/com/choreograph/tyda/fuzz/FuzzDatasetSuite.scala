package com.choreograph.tyda.fuzz

import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary.Shrinkable
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.iterator.DatasetOnIterator
import com.choreograph.tyda.iterator.LimitException
import com.choreograph.tyda.iterator.Limits
import com.choreograph.tyda.iterator.explain
import com.choreograph.tyda.unreachable

trait FuzzDatasetSuite extends AnyFunSuite {
  def reference[T](ds: Dataset[T]): Seq[T] = DatasetOnIterator(ds, Limits(maxRange = 1000)).toSeq

  def referenceExplain[T](ds: Dataset[T]): String = explain(ds)

  def implementation[T](ds: Dataset[T]): Seq[T]
  def implementationExplain[T](ds: Dataset[T]): String

  def knownBug[T](ds: Dataset[T]): Boolean = false

  /** Compare as multisets since row ordering is not guaranteed. */
  private def asMultiset[T](xs: Seq[T]): Map[T, Int] = xs.groupBy(identity).view.mapValues(_.size).toMap

  private def singleFuzz[To: Codec](using r: Random): Unit = {
    type Trial = Dataset[To]

    val candidate: Shrinkable[Trial] = Iterator
      .continually(GenDataset.unfailable[To]())
      // Only keep candidate with serveral operations
      .filter(s => Dataset.api.fold(s.value)(0L)([t] => (acc, _) => Continue(acc + 1)) > 3)
      .filter(s => !knownBug(s.value))
      .next()

    if knownBug(candidate.value) then pending: Unit

    def explains(trial: Trial): String =
      s"""Reference explain:
         |${referenceExplain(trial)}
         |Implementation explain:
         |${implementationExplain(trial)}""".stripMargin

    def evaluate(trial: Trial): Option[String] = {
      def runEval(f: => Seq[To]): Either[Throwable, Seq[To]] =
        try Right(f)
        catch {
          case LimitException(_) =>
            pending
            unreachable("pending skipped test")
          case e: Throwable => Left(e)
        }

      val referenceEval = runEval(reference(trial))
      val implementationEval = runEval(implementation(trial))

      (referenceEval, implementationEval) match {
        case (Right(expected), Right(actual)) =>
          val expectedMultiset = asMultiset(expected)
          val actualMultiset = asMultiset(actual)
          Option.when(expectedMultiset != actualMultiset)(s"""Backends produced different results.
               |
               |Reference results (${expected.size} rows):      ${expected.mkString(", ")}
               |Implementation results (${actual.size} rows): ${actual.mkString(", ")}
               |
               |${explains(trial)}""".stripMargin)
        case (Left(_), Left(_)) => None
        case (Right(expected), Left(e)) =>
          Some(s"""Reference succeeded but implementation threw ${e.getClass.getSimpleName}: ${e.getMessage}
               |
               |Reference results (${expected.size} rows): ${expected.mkString(", ")}
               |
               |${explains(trial)}""".stripMargin)
        case (Left(e), Right(actual)) =>
          Some(s"""Implementation succeeded but reference threw ${e.getClass.getSimpleName}: ${e.getMessage}
               |
               |Implementation results (${actual.size} rows): ${actual.mkString(", ")}
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

  (0 to 5000).foreach { i =>
    test(s"fuzz Dataset [seed=$i]") {
      given Random = Random(i + 7001)
      GenExprNode.arbitraryCodec() match {
        case given Codec[t] =>
          singleFuzz[t]
      }
    }
  }

  def fuzz[To: Codec](trials: Int = 1000) = (0 until trials).foreach(i =>
    test(s"fuzz Dataset[${Codec[To]}] [seed=$i]") {
      given Random = Random(i + 7001)
      singleFuzz[To]
    }
  )

  fuzz[Int]()
//  fuzz[String]()
//  fuzz[Option[Int]]()
//  fuzz[Option[String]]()
//  fuzz[Seq[Int]]()
//  fuzz[((Byte, String), (Byte, String))]()
//  fuzz[((Byte, String), Option[(Byte, String)])]()
//  fuzz[((Byte, String), (Option[Byte], String))]()
//  fuzz[(Option[Int], Long)]()
//  fuzz[(a: Int, b: String)]()
//  fuzz[Option[(Int, Int)]]()
  /* def fuzz[To: Codec](trials: Int = 1000) = (0 until trials).foreach(i => test(s"fuzz Dataset[${Codec[To]}]
   * [seed=$i]") { given Random = Random(i + 7001) singleFuzz[To] } )
   *
   * fuzz[Int]() fuzz[String]() fuzz[Option[Int]]() fuzz[Option[String]]() fuzz[Seq[Int]]() fuzz[((Byte,
   * String), (Byte, String))]() fuzz[((Byte, String), Option[(Byte, String)])]() fuzz[((Byte, String),
   * (Option[Byte], String))]() fuzz[(Option[Int], Long)]() fuzz[(a: Int, b: String)]() fuzz[Option[(Int,
   * Int)]]() */
}
