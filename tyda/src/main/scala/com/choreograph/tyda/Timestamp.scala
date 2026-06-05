package com.choreograph.tyda

import java.time.Instant
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util.concurrent.TimeUnit

/** A timestamp represents a point in time with microsecond precision.
  *
  * Valid range is [0001-01-01T00:00:00.000000Z, 9999-12-31T23:59:59.999999Z].
  * Values outside this range will cause an [[java.lang.ArithmeticException]]
  * when constructed via [[Timestamp.fromMicros]] . Note that this limit is not
  * always strictly enforced when reading data from external sources such as
  * Parquet files.
  *
  * NOTE: In practice it's implemented as a `Long` representing the number of
  * microseconds since the unix epoch. But this should not be exposed at api
  * level `Expr[Timestamp]` might have a different implmentation in some
  * backends.
  */
opaque type Timestamp = Long

object Timestamp {

  private val MicrosPerDay: Long = TimeUnit.DAYS.toMicros(1)

  /** The earliest representable timestamp (0001-01-01T00:00:00.000000Z). */
  val MinValue: Timestamp = LocalDate.of(1, 1, 1).toEpochDay * MicrosPerDay

  /** The latest representable timestamp (9999-12-31T23:59:59.999999Z). */
  val MaxValue: Timestamp = (LocalDate.of(9999, 12, 31).toEpochDay + 1) * MicrosPerDay - 1

  private val MinInstant = Instant.EPOCH.plus(MinValue, ChronoUnit.MICROS)
  private val MaxInstant = Instant.EPOCH.plus(MaxValue, ChronoUnit.MICROS)

  /** Create from an [[java.time.Instant]].
    *
    * NOTE: This is a destructive operation and will truncate precision to
    * microseconds.
    */
  def fromInstant(i: Instant): Option[Timestamp] = {
    if (i.isBefore(MinInstant) || i.isAfter(MaxInstant)) return None
    val microsFromSeconds = i.getEpochSecond * 1_000_000L
    val microsFromNanos = i.getNano / 1_000L
    Some(microsFromSeconds + microsFromNanos)
  }

  /** Create a timestamp from the number of microseconds since the unix epoch.
    *
    * @throws java.lang.ArithmeticException
    *   if the value is outside the valid range [0001-01-01T00:00:00.000000Z,
    *   9999-12-31T23:59:59.999999Z].
    */
  def fromMicros(micros: Long): Timestamp =
    if (micros < MinValue || micros > MaxValue) throw new ArithmeticException(
      "Timestamp out of range, must be between 0001-01-01T00:00:00Z and 9999-12-31T23:59:59.999999Z"
    )
    else micros

  extension (t: Timestamp) {

    /** Convert to an [[java.time.Instant]]. Note that this will have precision
      * up to microseconds only.
      */
    def toInstant: Instant = Instant.EPOCH.plus(t, ChronoUnit.MICROS)

    /** Get the number of microseconds since the unix epoch */
    def toMicros: Long = t

    /** Truncate the timestamp to a date (drops the time part) */
    def toDate: Date = Date.fromDays(TimeUnit.MICROSECONDS.toDays(t).toInt)
  }

  given Arbitrary[Timestamp] = Arbitrary.between(MinValue, MaxValue + 1)
  given Ordering[Timestamp] = Ordering.Long
  given Groupable[Timestamp] = Groupable[Long]
}
