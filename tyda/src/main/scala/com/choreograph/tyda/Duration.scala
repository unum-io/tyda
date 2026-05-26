package com.choreograph.tyda

import java.util.concurrent.TimeUnit

/** Represents a duration of time with microsecond precision.
  *
  * In SQL the corresponding type is `INTERVAL DAY TO SECOND`.
  */
opaque type Duration = Long

object Duration {

  /** Creates a `Duration` from the given number of microseconds. */
  def fromMicros(micros: Long): Duration = micros

  /** Creates a `Duration` from the given number of days. */
  def fromDays(days: Int): Duration = TimeUnit.DAYS.toMicros(days.toLong)

  extension (d: Duration) {

    /** Returns the duration in microseconds. */
    def toMicros: Long = d

    /** Returns the duration truncated to whole days. */
    def toDays: Long = TimeUnit.MICROSECONDS.toDays(d)

    /** Adds two durations together. */
    infix def +(other: Duration): Duration = d + other
  }

  given Ordering[Duration] = Ordering.Long
  given Arbitrary[Duration] = Arbitrary[Long]
  given Groupable[Duration] = Groupable[Long]
}
