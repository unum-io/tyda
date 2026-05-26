package com.choreograph.tyda

import java.time.LocalDate

import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

class TimestampSpec extends AnyFunSuite {

  test("round trip with instant") {
    (0 to 100).foreach { _ =>
      val expected = Arbitrary[Timestamp]()
      val instant = expected.toInstant
      val actual = Timestamp.fromInstant(instant)
      assert(actual.contains(expected))
    }
  }

  test("round trip toMicros") {
    (0 to 100).foreach { _ =>
      val ts = Arbitrary[Timestamp]()
      assert(ts == Timestamp.fromMicros(ts.toMicros))
    }
  }

  test("toDate") {
    val instant = LocalDate
      .of(1990, 1, 2)
      .atStartOfDay()
      .toInstant(java.time.ZoneOffset.UTC)
      .plusSeconds(Random.nextInt(24 * 3600))
      .plusNanos(Random.nextLong(1000_000_000L))
    val ts = Timestamp.fromInstant(instant)
    val date = ts.map(_.toDate)
    assert(date.map(_.toIsoString).contains("1990-01-02"))
  }

  test("fromMicros throws on out of range") {
    assertThrows[ArithmeticException](Timestamp.fromMicros(Timestamp.MinValue.toMicros - 1))
    assertThrows[ArithmeticException](Timestamp.fromMicros(Timestamp.MaxValue.toMicros + 1))
  }

  test("fromMicros accepts boundary values") {
    val _ = Timestamp.fromMicros(Timestamp.MinValue.toMicros)
    val _ = Timestamp.fromMicros(Timestamp.MaxValue.toMicros)
  }

  test("fromInstant rejects out of range") {
    val tooEarly = java.time.Instant.parse("0000-01-01T00:00:00Z")
    val tooLate = java.time.Instant.parse("+10000-01-01T00:00:00Z")
    assert(Timestamp.fromInstant(tooEarly).isEmpty)
    assert(Timestamp.fromInstant(tooLate).isEmpty)
  }

  test("fromInstant accepts boundary values") {
    assert(Timestamp.fromInstant(java.time.Instant.parse("0001-01-01T00:00:00Z")).isDefined)
    assert(Timestamp.fromInstant(java.time.Instant.parse("9999-12-31T23:59:59.999999Z")).isDefined)
  }
}
