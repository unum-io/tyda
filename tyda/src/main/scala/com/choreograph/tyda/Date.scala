package com.choreograph.tyda

import java.time.LocalDate
import java.time.format.DateTimeParseException
import java.util.concurrent.TimeUnit

import com.choreograph.tyda.Arbitrary

/** Date represents a calendar date.
  *
  * Valid range is [0001-01-01, 9999-12-31]. Values outside this range will
  * cause an [[ArithmeticException]] when constructed via [[Date.fromDays]] or
  * arithmetic operations. Note that this limit is not always strictly enforced
  * when reading data from external sources such as Parquet files.
  */
opaque type Date = Int

object Date {

  /** The earliest representable date (0001-01-01). */
  val MinValue: Date = DateValidator.MinDate

  /** The latest representable date (9999-12-31). */
  val MaxValue: Date = DateValidator.MaxDate

  private inline def valid(days: Int): Boolean = days >= MinValue && days <= MaxValue

  private def checkRange(days: Int): Date =
    if (!valid(days))
      throw new ArithmeticException("Date out of range, must be between 0001-01-01 and 9999-12-31")
    else days

  /** Create a Date from a compile-time constant ISO-8601 formatted date string
    * (yyyy-MM-dd).
    */
  inline def apply(inline days: String): Date = DateValidator.validate(days)

  /** Create a Date from the number of days since the unix epoch (1970-01-01).
    *
    * @throws ArithmeticException
    *   if the value is outside the valid range [0001-01-01, 9999-12-31]
    */
  def fromDays(days: Int): Date = checkRange(days)

  /** Create a Date from a ISO-8601 formatted date string (yyyy-MM-dd).
    */
  def fromIsoString(date: String): Option[Date] =
    try {
      val days = LocalDate.parse(date).toEpochDay.toInt
      Option.when(valid(days))(days)
    } catch case _: DateTimeParseException => None

  extension (value: Date) {

    /** Convert to a timestamp at the start of the day 00:00.
      */
    def atStartOfDay: Timestamp = Timestamp.fromMicros(TimeUnit.DAYS.toMicros(value.toLong))

    /** Convert to String in ISO-8601 format (yyyy-MM-dd).
      */
    def toIsoString: String = toLocalDate.toString

    /** Convert to a LocalDate.
      */
    def toLocalDate: LocalDate = LocalDate.ofEpochDay(value.toLong)

    /** Get the number of days since the unix epoch (1970-01-01).
      */
    def daysSinceEpoch: Int = value

    /** Add the specified number of days to this date.
      *
      * @throws ArithmeticException
      *   if the result is outside the valid range [0001-01-01, 9999-12-31].
      */
    def addDays(days: Int): Date = checkRange(Math.addExact(value, days))

    /** Subtract the specified number of days from this date.
      *
      * @throws ArithmeticException
      *   if the result is outside the valid range [0001-01-01, 9999-12-31].
      */
    def subtractDays(days: Int): Date = checkRange(Math.subtractExact(value, days))
  }

  /** The unix epoch date (1970-01-01).
    */
  val Epoch = fromDays(0)

  given Arbitrary[Date] = Arbitrary.between(MinValue, MaxValue + 1)
  given Ordering[Date] = Ordering[Int]
  given Groupable[Date] = Groupable[Int]
}
