package com.choreograph.tyda

import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

class DurationSpec extends AnyFunSuite {
  test("Duration fromMicros and toMicros") {
    val micros = 123456789L
    val duration = Duration.fromMicros(micros)
    assert(duration.toMicros == micros)
  }

  test("Duration toDays") {
    val days = 5
    val duration = Duration.fromDays(days) +
      Duration.fromMicros(Random.nextInt(24 * 60 * 60 * 1_000_000).toLong)
    assert(duration.toDays == days)
  }
}
