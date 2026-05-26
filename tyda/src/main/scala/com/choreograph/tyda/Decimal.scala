package com.choreograph.tyda

import java.math.BigDecimal as JBigDecimal

import scala.annotation.implicitNotFound
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.-
import scala.compiletime.ops.int.<=
import scala.compiletime.ops.int.>=

/** Decimal type with compile-time precision and scale.
  *
  * This type is constrained to be encodable as a 128 bit value. This means that
  * the maximum precision is 38 and the scale is less or equal to the precision.
  * This mostly matches how Spark handles decimal types.
  */
opaque type Decimal[Precision <: Int, Scale <: Int] = BigDecimal

object Decimal {
  // The maximum allowed precision to be encodable as 128 bit value
  type MaxPrecision = 38
  val MaxPrecision: MaxPrecision = 38

  // Access to the actual values for Scale and Precision, only available when the values are valid.
  @implicitNotFound(
    "Cannot prove that Decimal[${Precision}, ${Scale}] is an valid decimal type, " +
      "it requires that 1 <= ${Precision} <= 38 and 0 <= ${Scale} <= ${Precision}"
  )
  sealed trait Valid[Precision <: Int, Scale <: Int] extends Serializable {
    def precision: Precision
    def scale: Scale
  }

  given [Scale <: Int: ValueOf, Precision <: Int: ValueOf](using
      (Scale <= Precision && Scale >= 0 && Precision >= 1 && Precision <= MaxPrecision) =:= true
  ): Valid[Precision, Scale] =
    new Valid[Precision, Scale] {
      def scale: Scale = valueOf[Scale]
      def precision: Precision = valueOf[Precision]
    }

  /** Create a Decimal from an Int value.
    *
    * The function takes evidence that requires there to be enough precision to
    * represent all Int values.
    */
  def apply[Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale],
      ev: (Precision - Scale) >= 10 =:= true
  )(value: Int): Decimal[Precision, Scale] = {
    val decimal = BigDecimal(value).setScale(valid.scale)
    assert(decimal.precision <= valid.precision, s"Value $value exceeds precision ${valid.precision}")
    decimal
  }

  /** Create a Decimal from a Long value.
    *
    * The function takes evidence that requires there to be enough precision to
    * represent all Long values.
    */
  def apply[Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale],
      ev: (Precision - Scale) >= 19 =:= true
  )(value: Long): Decimal[Precision, Scale] = {
    val decimal = BigDecimal(value).setScale(valid.scale)
    assert(decimal.precision <= valid.precision, s"Value $value exceeds precision ${valid.precision}")
    decimal
  }

  /** Construct a Decimal from a Java BigDecimal.
    *
    * Returns None if the required precision is not met and performs rounding if
    * the BigDecimal has higher scale.
    */
  def apply[Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale]
  )(value: JBigDecimal): Option[Decimal[Precision, Scale]] = apply[Precision, Scale](BigDecimal(value))

  /** Construct a Decimal from a BigDecimal.
    *
    * Returns None if the required precision is not met and performs rounding if
    * the BigDecimal has higher scale.
    */
  def apply[Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale]
  )(value: BigDecimal): Option[Decimal[Precision, Scale]] =
    Some(value.setScale(valid.scale, BigDecimal.RoundingMode.HALF_UP)).filter(_.precision <= valid.precision)

  /** Create a Decimal from a Byte value.
    *
    * The function takes evidence that requires there to be enough precision to
    * represent all Byte values (which require at most 3 digits plus the scale).
    */
  def apply[Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale],
      ev: (Precision - Scale) >= 3 =:= true
  )(value: Byte): Decimal[Precision, Scale] = {
    val decimal = BigDecimal(value.toInt).setScale(valid.scale)
    assert(decimal.precision <= valid.precision, s"Value $value exceeds precision ${valid.precision}")
    decimal
  }

  /** Create a Decimal from a Short value.
    *
    * The function takes evidence that requires there to be enough precision to
    * represent all Short values (which require at most 5 digits plus the
    * scale).
    */
  def apply[Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale],
      ev: (Precision - Scale) >= 5 =:= true
  )(value: Short): Decimal[Precision, Scale] = {
    val decimal = BigDecimal(value.toInt).setScale(valid.scale)
    assert(decimal.precision <= valid.precision, s"Value $value exceeds precision ${valid.precision}")
    decimal
  }

  /** Construct a Decimal from a Float value.
    *
    * Returns None if the value is NaN or infinite, otherwise returns the
    * closest Decimal representation.
    */
  def apply[Precision <: Int, Scale <: Int](
      value: Float
  )(using valid: Valid[Precision, Scale]): Option[Decimal[Precision, Scale]] =
    Option.when(value.isFinite)(Decimal[Precision, Scale](JBigDecimal(value))).flatten

  /** Construct a Decimal from a Double value.
    *
    * Returns None if the value is NaN or infinite, otherwise returns the
    * closest Decimal representation.
    */
  def apply[Precision <: Int, Scale <: Int](value: Double)(using
      valid: Valid[Precision, Scale]
  ): Option[Decimal[Precision, Scale]] =
    /* Scala BigDecimal does toString as part of conversion from Double, so we use Java BigDecimal directly
     * instead. Should we maybe only use Java BigDecimal if they have such weird choices in scala BigDecimal? */
    Option.when(value.isFinite)(Decimal[Precision, Scale](JBigDecimal(value))).flatten

  given [Precision <: Int, Scale <: Int](using
      valid: Valid[Precision, Scale]
  ): Arbitrary[Decimal[Precision, Scale]] = {
    val maxUnscaledValue = BigInt(10).pow(valid.precision) - 1
    val maxBytes = maxUnscaledValue.bitLength / 8 + 1
    for {
      sign <- Arbitrary.oneOf(-1, 1)
      bytes <- Arbitrary.bytes(maxBytes)
    } yield BigDecimal(BigInt(sign, bytes) % maxUnscaledValue, valid.scale)
  }

  /** Returns a Decimal containing 0. */
  def zero[Precision <: Int, Scale <: Int](using valid: Valid[Precision, Scale]): Decimal[Precision, Scale] =
    BigDecimal(0).setScale(valid.scale)

  extension [Precision <: Int, Scale <: Int](d: Decimal[Precision, Scale]) {

    /** Rounds the decimal to an Byte by truncating the value.
      *
      * Returns None if the value is out of bounds for a Int.
      */
    def truncateToByte: Option[Byte] = {
      val rounded = d.setScale(0, BigDecimal.RoundingMode.DOWN)
      Option.when(rounded.isValidByte)(rounded.toByte)
    }

    /** Rounds the decimal to an Short by truncating the value.
      *
      * Returns None if the value is out of bounds for a Short.
      */
    def truncateToShort: Option[Short] = {
      val rounded = d.setScale(0, BigDecimal.RoundingMode.DOWN)
      Option.when(rounded.isValidShort)(rounded.toShort)
    }

    /** Rounds the decimal to an Int by truncating the value.
      *
      * Returns None if the value is out of bounds for a Int.
      */
    def truncateToInt: Option[Int] = {
      val rounded = d.setScale(0, BigDecimal.RoundingMode.DOWN)
      Option.when(rounded.isValidInt)(rounded.toInt)
    }

    /** Rounds the decimal to an Long by truncating the value.
      *
      * Returns None if the value is out of bounds for a long.
      */
    def truncateToLong: Option[Long] = {
      val rounded = d.setScale(0, BigDecimal.RoundingMode.DOWN)
      Option.when(rounded.isValidLong)(rounded.toLong)
    }

    /** Rounds the decimal to the nearest Int.
      *
      * Returns None if the value is out of bounds for a Int.
      */
    def roundToInt: Option[Int] = {
      val rounded = d.setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      Option.when(rounded.isValidInt)(rounded.toInt)
    }

    /** Rounds the decimal to the nearest long. Returns None if the value is out
      * of bounds for a long.
      */
    def roundToLong: Option[Long] =
      val rounded = d.setScale(0, BigDecimal.RoundingMode.HALF_UP).toBigInt
      Option.when(rounded.isValidLong)(rounded.toLong)

    /** Converts the decimal into a Float.
      *
      * Note: In some cases this conversion will lose precision.
      */
    def toFloat: Float = d.floatValue()

    /** Converts the decimal into a Double.
      *
      * Note: In some cases this conversion will lose precision.
      */
    def toDouble: Double = d.doubleValue()

    /** Converts the decimal into a BigDecimal.
      */
    def toBigDecimal: BigDecimal = d

    /** Widens the Decimal to a new precision and scale.
      */
    def widen[P2 <: Int, S2 <: Int](using
        valid: Valid[P2, S2],
        maxIncrease: (P2 - S2 >= Precision - Scale) =:= true,
        scaleIncrease: (S2 >= Scale) =:= true
    ): Decimal[P2, S2] = {
      val res = d.setScale(valid.scale)
      assert(res.precision <= valid.precision)
      res
    }
  }

  extension [Precision <: Int](d: Decimal[Precision, 0]) {
    private def checkScale: Unit = assert(d.scale == 0, "Expected Decimal with scale 0")

    /** Converts the decimal to an Bytes if it is within the bounds of an Byte.
      */
    def toByteOption: Option[Byte] = {
      checkScale
      Option.when(d.isValidByte)(d.toByte)
    }

    /** Converts the decimal to an Short if it is within the bounds of an Short.
      */
    def toShortOption: Option[Short] = {
      checkScale
      Option.when(d.isValidShort)(d.toShort)
    }

    /** Converts the decimal to an Int if it is within the bounds of an Int.
      */
    def toIntOption: Option[Int] = {
      checkScale
      Option.when(d.isValidInt)(d.toInt)
    }

    /** Converts the decimal to a Long if it is within the bounds of a Long.
      */
    def toLongOption: Option[Long] = {
      checkScale
      Option.when(d.isValidLong)(d.toLong)
    }
  }

  extension [Precision <: Int](d: Decimal[Precision, 0])(using Precision <= 10 =:= true) {

    /** Converts the decimal to a Long that is statically know to be
      * representable as a Long.
      */
    def toInt: Int = {
      assert(d.isValidInt, s"Decimal $d is not valid for Int, but should have been statically checked")
      d.toInt
    }
  }

  extension [Precision <: Int](d: Decimal[Precision, 0])(using Precision <= 19 =:= true) {

    /** Converts the decimal to a Long that is statically know to be
      * representable as a Long.
      */
    def toLong: Long = {
      assert(d.isValidLong, s"Decimal $d is not valid for Long, but should have been statically checked")
      d.toLong
    }
  }

  given [P <: Int, S <: Int]: Ordering[Decimal[P, S]] = Ordering[BigDecimal]
}
