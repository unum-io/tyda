package com.choreograph.tyda.json

import scala.util.Random

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.testsuites.FloatingPointEquality.given

object CodecToJsoniterSpec {
  final case class SimpleProduct(a: Int, b: Option[String]) derives Arbitrary, Codec
  final case class AllOptionalProduct(a: Option[Int], b: Option[String]) derives Arbitrary, Codec

  given [A] => Codec[A] => JsonValueCodec[A] = CodecToJsoniter.create[A]
}

class CodecToJsoniterSpec extends AnyFunSuite {
  import CodecToJsoniterSpec.{*, given}

  def roundTrip[T: Arbitrary: Codec as codec: TypeName: Equality] =
    test(s"round trip ${TypeName.name[T]}") {

      def failureMessage(expected: T, actual: T): String =
        s"""Round trip failed for ${TypeName.name[T]}:
         |Expected: $expected
         |Actual: $actual
         |""".stripMargin

      def roundTrip(value: T): T = readFromString(writeToString(value))

      for (_ <- 0 until 100) {
        val shrinkable = Arbitrary[T].shrinkable(Random)
        val read = roundTrip(shrinkable.value)
        if read !== shrinkable.value then {
          alert(failureMessage(shrinkable.value, read))
          val minimized = shrinkable.minimize(input => roundTrip(input) !== input)
          fail(failureMessage(minimized, roundTrip(minimized)))
        }
      }
    }

  def testRead[A: Codec as codec: Equality](testName: String, expected: A, json: String): Unit =
    test(testName) {
      val read = readFromString(json)
      assert(read === expected)
    }

  def testFailure[A: Codec as codec](testName: String, json: String, error: String): Unit =
    test(testName) {
      val e = intercept[Exception] { readFromString(json) }
      assert(e.getMessage.contains(error))
    }

  roundTrip[Boolean]
  roundTrip[Byte]
  roundTrip[Short]
  roundTrip[Int]
  roundTrip[Long]
  roundTrip[Float]
  roundTrip[Double]
  roundTrip[Decimal[3, 0]]
  roundTrip[Decimal[15, 5]]
  roundTrip[Decimal[38, 9]]
  roundTrip[Timestamp]
  roundTrip[Date]
  roundTrip[Duration]
  roundTrip[String]
  roundTrip[Option[Int]]
  roundTrip[Option[Option[Int]]]
  roundTrip[Seq[Int]]
  roundTrip[Seq[SimpleProduct]]
  roundTrip[Map[String, Int]]
  roundTrip[Map[SimpleProduct, SimpleProduct]]
  roundTrip[EmptyTuple]
  roundTrip[AllOptionalProduct]
  roundTrip[SimpleProduct]

  testRead[AllOptionalProduct](
    "allow all optional fields to be missing",
    AllOptionalProduct(None, None),
    "{}"
  )
  testRead[SimpleProduct]("allow optional fields to be missing", SimpleProduct(1, None), """{"a":1}""")
  testRead[SimpleProduct]("allow optional fields to be null", SimpleProduct(1, None), """{"a":1,"b":null}""")

  testFailure[SimpleProduct](
    "fail when required field is missing",
    """{"b":"foo"}""",
    "missing required field a"
  )

  // These checks that we can read data as written by BigQuery
  testRead[Timestamp](
    "Read offset timestamps",
    Timestamp.fromMicros(1772447400123456),
    """{"value":"2026-03-02T10:30:00.123456+00:00"}"""
  )
  testRead[Date]("Read local date", Date("2026-03-02"), """{"value":"2026-03-02"}""")
  testRead[Long]("Read Long as string", 123L, """{"value":"123"}""")
  testRead[Decimal[15, 5]]("Read decimal as int", Decimal[15, 5](123), """{"value":123}""")
  testRead[Decimal[15, 5]]("Read decimal as string", Decimal[15, 5](123), """{"value":"123"}""")
  testRead[Float]("read Infinity from string", Float.PositiveInfinity, """{"value":"Infinity"}""")
  testRead[Double]("read -Infinity from string", Double.NegativeInfinity, """{"value":"-Infinity"}""")
  testRead[Float]("read NaN from string", Float.NaN, """{"value":"NaN"}""")
}
