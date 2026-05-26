package com.choreograph.tyda.table

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.NumericsReadMode
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.iterator.ExprEvaluation

object WideBigQuerySpec {
  // Bias Arbitrary[Long] to generate smaller values more often
  private given Arbitrary[Long] =
    Arbitrary.combine(
      Arbitrary[Byte].map(_.toLong),
      Arbitrary[Short].map(_.toLong),
      Arbitrary[Int].map(_.toLong),
      Arbitrary[Long]
    )
  private given Arbitrary[Double] = Arbitrary[Double].filter(!_.isNaN)
}

/* This test suite is defined in tyda-iterator to leverage the iterator implmementation as part of testing */
class WideBigQuerySpec extends AnyFunSuite {
  import WideBigQuerySpec.given
  private def repr[T](codec: Codec[T]): Codec[?] =
    codec match {
      case Codec.FromInjection(_, to) => to
      case _ => codec
    }

  private def check[T: {Codec, TypeName}, Widen: {Arbitrary, Codec}](expected: Widen => Option[T]): Unit = {
    val codecAndCast = NumericsReadMode.widenBigQuery(Codec[T])
    test(s"NumericsReadMode.widenBigQuery for ${TypeName.name[T]}") {
      assert(codecAndCast.codec == repr(Codec[Widen]))
      /* TYPE SAFETY: The assert above enures that the unknown type from widenBigQuery matches the expected
       * Widen type. */
      val evaluator = ExprEvaluation
        .lambda(codecAndCast.cast)(using codecAndCast.codec)
        .asInstanceOf[Widen => T]
      (0 to 100).foreach(_ =>
        val widenValue = Arbitrary[Widen]()
        expected(widenValue) match {
          case None => assertThrows[Exception](evaluator(widenValue))
          case Some(expectedValue) => assert(evaluator(widenValue) == expectedValue)
        }
      )
    }
  }
  private def checkUnsupported[T: {Codec, TypeName}]: Unit = {
    test(s"NumericsReadMode.widenBigQuery unsupported for ${TypeName.name[T]}") {
      val e = intercept[Exception](NumericsReadMode.widenBigQuery(Codec[T]))
      assert(e.getMessage.contains("unsupported"))
    }
  }

  check[Int, Long](v => Option.when(v >= Int.MinValue && v <= Int.MaxValue)(v.toInt))
  check[Short, Long](v => Option.when(v >= Short.MinValue && v <= Short.MaxValue)(v.toShort))
  check[Byte, Long](v => Option.when(v >= Byte.MinValue && v <= Byte.MaxValue)(v.toByte))
  check[Float, Double](v => Some(v.toFloat))
  check[Decimal[29, 9], Decimal[38, 9]](v => Decimal[29, 9](v.toBigDecimal))
  check[Seq[Float], Seq[Double]](v => Some(v.map(_.toFloat)))
  check[Option[Float], Option[Double]](v => Some(v.map(_.toFloat)))
  check[Long, Long](Some(_))
  check[Double, Double](Some(_))
  check[Map[Long, Long], Map[Long, Long]](Some(_))

  checkUnsupported[Decimal[38, 18]]
  checkUnsupported[Map[Int, Long]]
  checkUnsupported[Map[Long, Int]]
}
