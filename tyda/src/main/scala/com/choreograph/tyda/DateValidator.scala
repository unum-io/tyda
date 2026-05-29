package com.choreograph.tyda

import java.time.LocalDate
import java.time.format.DateTimeParseException

import scala.quoted.*

private object DateValidator {
  val MinDate: Int = LocalDate.of(1, 1, 1).toEpochDay.toInt
  val MaxDate: Int = LocalDate.of(9999, 12, 31).toEpochDay.toInt

  inline def validate(inline ts: String): Int = ${ validateImpl('ts) }

  private def validateImpl(ts: Expr[String])(using Quotes): Expr[Int] =
    val value = ts.valueOrAbort
    try {
      val epochDayLong = LocalDate.parse(value).toEpochDay()
      if epochDayLong < MinDate || epochDayLong > MaxDate then
        quotes
          .reflect
          .report
          .errorAndAbort(s"Date ${value} is out of range, must be between 0001-01-01 and 9999-12-31")
      val epochDay = epochDayLong.toInt
      Expr(epochDay)
    } catch {
      case e: DateTimeParseException => quotes
          .reflect
          .report
          .errorAndAbort(s"Invalid date ${value} does not match uuuu-MM-dd: ${e.getMessage}")
    }
}
