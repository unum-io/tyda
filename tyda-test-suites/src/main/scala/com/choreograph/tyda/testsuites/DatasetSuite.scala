package com.choreograph.tyda.testsuites

import scala.reflect.ClassTag
import scala.util.Random

import org.scalactic.Equality
import org.scalatest.Assertions.fail
import org.scalatest.enablers.Aggregating
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.TestPendingException
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.CanCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Comparable
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.Groupable
import com.choreograph.tyda.Runner
import com.choreograph.tyda.SumMagnet

// This alias has been inlined because of this bug https://github.com/scala/scala3/issues/24357
// Without inlining, the catch clause will catch too broadly.
@deprecated(
  "Do not use until using Scala >3.8.1 where https://github.com/scala/scala3/issues/24357 is fixed."
)
type ScalaTestControlException = TestPendingException | TestCanceledException | TestFailedException

object DatasetSuite {
  opaque type TinyByte = Byte
  object TinyByte {
    def apply(b: Byte): TinyByte = b

    given Arbitrary[TinyByte] = Arbitrary.int.map(_ % 10).map(_.toByte)
    given Codec[TinyByte] = Codec[Byte]
    given Ordering[TinyByte] = Ordering[Byte]
    given Groupable[TinyByte] = Groupable[Byte]
    given Comparable[TinyByte] = Comparable[Byte]
    given SumMagnet.Aux[TinyByte, Long] = SumMagnet.byte
    given [T] => (canCast: CanCast[Byte, T]) => CanCast[TinyByte, T] = canCast
  }
  type Pair = (TinyByte, TinyByte)
  enum MyEnum extends EnumStableHashCode derives Codec {
    case A(value: Int)
    case B(value: String)
    case C
  }

  enum Result {
    case Success
    case Exception(message: String, e: Throwable)
    case Failure(message: String)

    def failureMessage: Option[String] =
      this match {
        case Success => None
        case Exception(message, _) => Some(message)
        case Failure(message) => Some(message)
      }

    def isFailure: Boolean =
      this match {
        case Success => false
        case _ => true
      }

    def check: Unit =
      this match {
        case Success => ()
        case Exception(message, e) => fail(message, e)
        case Failure(message) => fail(message)
      }
  }
}

// Testsuite that will compare a Dataset backend to a reference implementation.
trait DatasetSuite extends AnyFunSuite {
  import DatasetSuite.Result

  def reference: Runner

  def implementation: Runner

  private def checkSameResult[T](ds: Dataset[T])(using aggregating: Aggregating[Seq[T]]): Result = {
    val expected = reference.collect(ds)
    val actual =
      try implementation.collect(ds)
      catch {
        // TODO: Use ScalaTestControlException alias
        case e: (TestPendingException | TestCanceledException | TestFailedException) => throw e
        case e: Throwable => return Result.Exception(
            s"""Implementation threw an exception during execution:
               |${e.getMessage}
               |
               |Reference explain:\n${reference.explain(ds)}\n
               |Implementation explain:\n${implementation.explain(ds)}""".stripMargin,
            e
          )
      }
    val containsSameElements = aggregating.containsTheSameElementsAs(actual, expected)
    if containsSameElements then Result.Success
    else Result.Failure(s"""Implementations did not produced the same results\n
      |Reference results: ${expected.mkString(", ")}
      |Implementation results: ${actual.mkString(", ")}
      |
      |Reference explain:\n${reference.explain(ds)}\n
      |Implementation explain:\n${implementation.explain(ds)}
      """.stripMargin)
  }

  private def propTest[Input: Arbitrary](name: String, testInput: Input => Result) =
    test(name) {
      val shrinkableInput = Arbitrary[Input].shrinkable(Random)
      testInput(shrinkableInput.value).failureMessage match {
        case None => ()
        case Some(failureMessage) =>
          alert(s"Found failing input\n$failureMessage\n\nStarting to shrink failure...")
          val minimized = shrinkableInput.minimize(input => testInput(input).isFailure)
          testInput(minimized).check
      }
    }

  def test[T: Arbitrary: Codec, R: Equality](
      name: String,
      computation: Dataset[T] => Dataset[R],
      inputs: Seq[T]*
  ): Unit = {
    def testInput(values: Seq[T]): Result = {
      val ds = computation(Dataset.FromSeq(values))
      checkSameResult(ds)
    }
    propTest(name, testInput)
    inputs.foreach(inputSeq =>
      test(s"$name with input ${inputSeq.mkString(", ")}")(testInput(inputSeq).check)
    )
  }

  def test[T1: Arbitrary: Codec, T2: Arbitrary: Codec, R](
      name: String,
      computation: (Dataset[T1], Dataset[T2]) => Dataset[R]
  ): Unit = {
    def testInput(input: (Seq[T1], Seq[T2])): Result = {
      val (values1, values2) = input
      val ds = computation(Dataset.FromSeq(values1), Dataset.FromSeq(values2))
      checkSameResult(ds)
    }
    propTest(name, testInput)
  }

  def test[T1: Arbitrary: Codec, T2: Arbitrary: Codec, T3: Arbitrary: Codec, R](
      name: String,
      computation: (Dataset[T1], Dataset[T2], Dataset[T3]) => Dataset[R]
  ): Unit = {
    def testInput(input: (Seq[T1], Seq[T2], Seq[T3])): Result = {
      val (values1, values2, values3) = input
      val ds = computation(Dataset.FromSeq(values1), Dataset.FromSeq(values2), Dataset.FromSeq(values3))
      checkSameResult(ds)
    }
    propTest(name, testInput)
  }

  def testFailure[T: Codec, R](
      name: String,
      input: Seq[T],
      computation: Dataset[T] => Dataset[R],
      expectedError: String
  ): Unit =
    test(name) {
      val ds = computation(Dataset.FromSeq(input))
      val e = intercept[Exception](implementation.collect(ds))
      e match {
        // TODO: Use ScalaTestControlException alias
        case e: (TestPendingException | TestCanceledException | TestFailedException) => throw e
        case _ => Option(e.getMessage) match {
            case None => fail(s"Expected exception with $expectedError but got exception with no message.", e)
            case Some(message) => assert(
                message.contains(expectedError),
                s"Error message did not contain expected text.\nExpected to contain: $expectedError\nActual message: ${message}"
              )
          }
      }
    }
}
