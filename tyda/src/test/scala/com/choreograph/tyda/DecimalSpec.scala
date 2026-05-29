package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

class DecimalSpec extends AnyFunSuite {
  test("be constructable from Int") {
    val _ = Decimal[10, 0](Int.MaxValue)
    val _ = Decimal[20, 10](Int.MaxValue)
    val _ = Decimal[38, 0](Int.MaxValue)
    val _ = Decimal[38, 24](Int.MaxValue)
  }

  test("be constructable from Long") {
    val _ = Decimal[20, 0](Long.MaxValue)
    val _ = Decimal[30, 10](Long.MaxValue)
    val _ = Decimal[38, 0](Long.MaxValue)
    val _ = Decimal[38, 19](Long.MaxValue)
  }

  test("precision of 0 should not be allowed") {
    assertCompileTimeError("Decimal.zero[0, 0]", "Cannot prove that Decimal")
  }

  test("not be constructable from Int when the precision is not enough") {
    assertCompileTimeError("Decimal[2, 0](10)", "Cannot prove that")
    assertCompileTimeError("Decimal[20, 15](10)", "Cannot prove that")
    assertCompileTimeError("Decimal[33, 24](10)", "Cannot prove that")
  }

  test("not be constructable from Long when the precision is not enough") {
    assertCompileTimeError("Decimal[2, 0](10L)", "Cannot prove that")
    assertCompileTimeError("Decimal[38, 20](10L)", "Cannot prove that")
  }
  test("be constructable from BigDecimal when it fits") {
    assert(Decimal[38, 24](BigDecimal(1.0)).isDefined)
    assert(Decimal[2, 0](BigDecimal(99)).isDefined)
  }

  test("return none when it does not fit") {
    assert(Decimal[2, 0](BigDecimal(100)).isEmpty)
    assert(Decimal[2, 1](BigDecimal(100, 1)).isEmpty)
  }

  test("roundToLong returns Some when value is within long range") {
    assert(Decimal[38, 24](BigDecimal(-1.2)).flatMap(_.roundToLong).contains(-1L))
    assert(Decimal[38, 24](BigDecimal(1.2)).flatMap(_.roundToLong).contains(1L))
    assert(Decimal[38, 24](BigDecimal(-98.6)).flatMap(_.roundToLong).contains(-99L))
    assert(Decimal[38, 24](BigDecimal(98.6)).flatMap(_.roundToLong).contains(99L))
    assert(Decimal[38, 0](Int.MaxValue.toLong + 1).roundToLong.contains(Int.MaxValue.toLong + 1))
  }

  test("roundToLong returns None when value is too large") {
    val maxLongBD = BigDecimal(Long.MaxValue) + BigDecimal(1)
    val maxLongDecimal = Decimal[38, 0](maxLongBD)
    assert(maxLongDecimal.isDefined)
    assert(maxLongDecimal.flatMap(_.roundToLong).isEmpty)

    val minLongBD = BigDecimal(Long.MinValue) - BigDecimal(1)
    val minLongDecimal = Decimal[38, 0](minLongBD)
    assert(minLongDecimal.isDefined)
    assert(minLongDecimal.flatMap(_.roundToLong).isEmpty)
  }

  test("roundToInt returns Some when value is within long range") {
    assert(Decimal[38, 24](BigDecimal(-1.2)).flatMap(_.roundToInt).contains(-1))
    assert(Decimal[38, 24](BigDecimal(1.2)).flatMap(_.roundToInt).contains(1))
    assert(Decimal[38, 24](BigDecimal(-98.6)).flatMap(_.roundToInt).contains(-99))
    assert(Decimal[38, 24](BigDecimal(98.6)).flatMap(_.roundToInt).contains(99))
  }

  test("roundToInt returns None when value is too large") {
    val maxIntBD = BigDecimal(Int.MaxValue) + BigDecimal(1)
    val maxIntDecimal = Decimal[38, 0](maxIntBD)
    assert(maxIntDecimal.isDefined)
    assert(maxIntDecimal.flatMap(_.roundToInt).isEmpty)

    val minIntBD = BigDecimal(Int.MinValue) - BigDecimal(1)
    val minIntDecimal = Decimal[38, 0](minIntBD)
    assert(minIntDecimal.isDefined)
    assert(minIntDecimal.flatMap(_.roundToInt).isEmpty)
  }

  test("toDouble should return the value as Double") {
    assert(Decimal[38, 24](1.23).map(_.toDouble).contains(1.23))
    assert(Decimal[38, 0](1234567890).toDouble == 1234567890.0)
    assert(Decimal[38, 0](BigDecimal("12345678901234567890")).map(_.toDouble).contains(1.2345678901234568e19))
  }

  test("toIntOption should not exists for scale > 0") {
    assertCompileTimeError("Decimal[38, 24](10).toIntOption", "toIntOption is not a member")
  }

  test("truncateToByte") {
    assert(Decimal[38, 3](BigDecimal("-1.1")).flatMap(_.truncateToByte).contains(-1))
    assert(Decimal[38, 3](BigDecimal("-1.9")).flatMap(_.truncateToByte).contains(-1))
    assert(Decimal[38, 3](BigDecimal("1.1")).flatMap(_.truncateToByte).contains(1))
    assert(Decimal[38, 3](BigDecimal("1.9")).flatMap(_.truncateToByte).contains(1))
    assert(Decimal[38, 0](Byte.MaxValue).truncateToByte.contains(Byte.MaxValue))
    assert(Decimal[38, 0](Short.MaxValue).truncateToByte.isEmpty)
  }

  test("truncateToShort") {
    assert(Decimal[38, 3](BigDecimal("-1.1")).flatMap(_.truncateToShort).contains(-1))
    assert(Decimal[38, 3](BigDecimal("-1.9")).flatMap(_.truncateToShort).contains(-1))
    assert(Decimal[38, 3](BigDecimal("1.1")).flatMap(_.truncateToShort).contains(1))
    assert(Decimal[38, 3](BigDecimal("1.9")).flatMap(_.truncateToShort).contains(1))
    assert(Decimal[38, 0](Short.MaxValue).truncateToShort.contains(Short.MaxValue))
    assert(Decimal[38, 0](Int.MaxValue).truncateToShort.isEmpty)
  }

  test("truncateToInt") {
    assert(Decimal[38, 3](BigDecimal("-1.1")).flatMap(_.truncateToInt).contains(-1))
    assert(Decimal[38, 3](BigDecimal("-1.9")).flatMap(_.truncateToInt).contains(-1))
    assert(Decimal[38, 3](BigDecimal("1.1")).flatMap(_.truncateToInt).contains(1))
    assert(Decimal[38, 3](BigDecimal("1.9")).flatMap(_.truncateToInt).contains(1))
    assert(Decimal[38, 0](Int.MaxValue).truncateToInt.contains(Int.MaxValue))
    assert(Decimal[38, 0](Long.MaxValue).truncateToInt.isEmpty)
  }

  test("truncateToLong") {
    assert(Decimal[38, 3](BigDecimal("-1.1")).flatMap(_.truncateToLong).contains(-1))
    assert(Decimal[38, 3](BigDecimal("-1.9")).flatMap(_.truncateToLong).contains(-1))
    assert(Decimal[38, 3](BigDecimal("1.1")).flatMap(_.truncateToLong).contains(1))
    assert(Decimal[38, 3](BigDecimal("1.9")).flatMap(_.truncateToLong).contains(1))
    assert(Decimal[38, 0](Long.MaxValue).truncateToLong.contains(Long.MaxValue))
    assert(Decimal[38, 0](BigDecimal(Long.MaxValue) + BigDecimal(1)).flatMap(_.truncateToLong).isEmpty)
  }

  test("toByteOption should return Some when value is within Byte range") {
    assert(Decimal[38, 0](Byte.MaxValue).toByteOption.contains(Byte.MaxValue))
    assert(Decimal[38, 0](Byte.MinValue).toByteOption.contains(Byte.MinValue))
    assert(Decimal[38, 0](Long.MaxValue).toByteOption.isEmpty)
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Byte]()
      assert(Decimal[10, 0](value).toByteOption.contains(value))
    }
  }

  test("toShortOption should return Some when value is within Short range") {
    assert(Decimal[38, 0](Short.MaxValue).toShortOption.contains(Short.MaxValue))
    assert(Decimal[38, 0](Short.MinValue).toShortOption.contains(Short.MinValue))
    assert(Decimal[38, 0](Long.MaxValue).toShortOption.isEmpty)
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Short]()
      assert(Decimal[10, 0](value).toShortOption.contains(value))
    }
  }

  test("toIntOption should return Some when value is within Int range") {
    assert(Decimal[38, 0](Int.MaxValue).toIntOption.contains(Int.MaxValue))
    assert(Decimal[38, 0](Int.MinValue).toIntOption.contains(Int.MinValue))
    assert(Decimal[38, 0](Long.MaxValue).toIntOption.isEmpty)
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Int]()
      assert(Decimal[10, 0](value).toIntOption.contains(value))
    }
  }

  test("toLongOption should return Some when value is within Long range") {
    assert(Decimal[38, 0](Long.MaxValue).toLongOption.contains(Long.MaxValue))
    assert(Decimal[38, 0](Long.MinValue).toLongOption.contains(Long.MinValue))
    assert(Decimal[38, 0](BigDecimal(Long.MaxValue) + BigDecimal(1)).flatMap(_.toLongOption).isEmpty)
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Long]()
      assert(Decimal[19, 0](value).toLongOption.contains(value))
    }
  }

  test("toInt should not exists for Precision > 10") {
    assertCompileTimeError("Decimal[11, 0](10).toInt", "toInt is not a member")
    assertCompileTimeError("Decimal[20, 0](10).toInt", "toInt is not a member")
  }

  test("toInt should return the value as Int") {
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Int]()
      assert(Decimal[5, 0](BigDecimal(value % 10000)).map(_.toInt).contains(value % 10000))
      assert(Decimal[10, 0](value).toInt == value)
    }
  }

  test("toLong should not exists for Precision > 19") {
    assertCompileTimeError("Decimal[20, 0](10).toLong", "toLong is not a member")
    assertCompileTimeError("Decimal[38, 0](10).toLong", "toLong is not a member")
  }

  test("toLong should return the value as Long") {
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Long]()
      assert(Decimal[5, 0](BigDecimal(value % 10000)).map(_.toLong).contains(value % 10000))
      assert(Decimal[19, 0](value).toLong == value)
    }
  }

  test("toBigDecimal should return the value as BigDecimal") {
    val decimal = Arbitrary[Decimal[38, 24]]()
    assert(Decimal[38, 24](decimal.toBigDecimal).contains(decimal))
  }

  test("toByteOption should return the value as Byte") {
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Byte]()
      assert(Decimal[2, 0](BigDecimal(value % 100)).flatMap(_.toByteOption).contains(value % 100))
      assert(Decimal[5, 0](value).toByteOption.contains(value))
      assert(Decimal[38, 0](Int.MaxValue).toByteOption.isEmpty)
    }
  }

  test("toShortOption should return the value as Short") {
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Short]()
      assert(Decimal[4, 0](BigDecimal(value % 10000)).flatMap(_.toShortOption).contains(value % 10000))
      assert(Decimal[5, 0](value).toShortOption.contains(value))
      assert(Decimal[38, 0](Int.MaxValue).toShortOption.isEmpty)
    }
  }

  test("toIntOption should return the value as Int") {
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Int]()
      assert(Decimal[5, 0](BigDecimal(value % 10000)).flatMap(_.toIntOption).contains(value % 10000))
      assert(Decimal[10, 0](value).toIntOption.contains(value))
      assert(Decimal[38, 0](Long.MaxValue).toIntOption.isEmpty)
    }
  }

  test("toLongOption should return the value as Long") {
    (0 to 10).foreach { _ =>
      val value = Arbitrary[Long]()
      assert(Decimal[10, 0](BigDecimal(value % 10000)).flatMap(_.toLongOption).contains(value % 10000))
      assert(Decimal[19, 0](value).toLongOption.contains(value))
      assert(Decimal[38, 0](BigDecimal(Long.MaxValue) + BigDecimal(1)).flatMap(_.toLongOption).isEmpty)
    }
  }

  test("Float round trip") {
    val decimal = Arbitrary[Decimal[38, 24]]()
    val floatValue = decimal.toFloat
    assert(Decimal[38, 24](BigDecimal(floatValue)).map(_.toFloat).contains(floatValue))
  }

  test("Double round trip") {
    val decimal = Arbitrary[Decimal[38, 24]]()
    val doubleValue = decimal.toDouble
    assert(Decimal[38, 24](BigDecimal(doubleValue)).map(_.toDouble).contains(doubleValue))
  }

  test("widening Decimal") {
    val decimal = Arbitrary[Decimal[10, 4]]()
    val widened = decimal.widen[15, 6]
    assert(widened.toBigDecimal.scale == 6)
    assert(Decimal[10, 4](widened.toBigDecimal).contains(decimal))
  }
}
