package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

class DateSpec extends AnyFunSuite {
  test("Be constructable from literal strings") { Date("1970-01-01") == Date.fromDays(0) }

  test("reject bad values at compiletime") {
    assertCompileTimeError("""Date("2025-02-29")""", "Invalid date", "does not match uuuu-MM-dd")
  }

  test("reject out of range values at compiletime") {
    assertCompileTimeError("""Date("0000-01-01")""", "out of range")
  }

  test("Date toIsoString") {
    val date = Date.fromDays(1950)
    assert(date.toIsoString == "1975-05-05")
  }

  Seq(
    ("1970-01-01", Date.fromDays(0)),
    ("2024-01-01", Date.fromDays(19723)),
    ("2024-02-29", Date.fromDays(19782))
  ).foreach({ case (date, expected) =>
    test(s"parse date $date") { assert(Date.fromIsoString(date).contains(expected)) }
  })

  test("fromIsoString rejects out of range dates") {
    assert(Date.fromIsoString("0000-01-01").isEmpty)
    assert(Date.fromIsoString("10000-01-01").isEmpty)
  }

  test("fromIsoString accepts boundary dates") {
    assert(Date.fromIsoString("0001-01-01").isDefined)
    assert(Date.fromIsoString("9999-12-31").isDefined)
  }

  test("fromDays throws on out of range") {
    assertThrows[ArithmeticException](Date.fromDays(Date.MinValue.daysSinceEpoch - 1))
    assertThrows[ArithmeticException](Date.fromDays(Date.MaxValue.daysSinceEpoch + 1))
  }

  test("fromDays accepts boundary values") {
    assert(Date.MinValue.toIsoString == "0001-01-01")
    assert(Date.MaxValue.toIsoString == "9999-12-31")
  }

  test("convert to Timestamp") {
    val localDate = java.time.LocalDate.of(2024, 2, 27)
    val expected = Timestamp
      .fromInstant(localDate.atStartOfDay.toInstant(java.time.ZoneOffset.UTC))
      .getOrElse(fail("Failed to compute expected timestamp"))
    assert(Date("2024-02-27").atStartOfDay == expected)
  }

  test("add and subtract days") {
    val date = Date("2024-02-27")
    assert(date.addDays(2) == Date("2024-02-29"))
    assert(date.subtractDays(27) == Date("2024-01-31"))
    assert(Date("2025-02-27").addDays(2) == Date("2025-03-01"))
  }

  test("addDays throws on overflow") { assertThrows[ArithmeticException](Date.MaxValue.addDays(1)) }

  test("subtractDays throws on overflow") { assertThrows[ArithmeticException](Date.MinValue.subtractDays(1)) }

  test("daysSinceEpoch") { assert(Date("2024-02-27").daysSinceEpoch == 19780) }
}
