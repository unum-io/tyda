package com.choreograph.tyda

/* NumericLimits is a type class that provides the minimum and maximum values of a numeric type. */
private[tyda] trait NumericLimits[T] {
  def min: T
  def max: T
}

/* Specialized type class with values that only exists for floating point types. */
private[tyda] trait FloatingLimits[T] extends NumericLimits[T] {
  def nan: T
  def infinity: T
  def minPositiveValue: T
}

private[tyda] object FloatingLimits {
  def apply[T: FloatingLimits]: FloatingLimits[T] = summon[FloatingLimits[T]]
}

private[tyda] object NumericLimits {
  def apply[T: NumericLimits]: NumericLimits[T] = summon[NumericLimits[T]]

  given byte: NumericLimits[Byte] with {
    def min: Byte = Byte.MinValue
    def max: Byte = Byte.MaxValue
  }

  given short: NumericLimits[Short] with {
    def min: Short = Short.MinValue
    def max: Short = Short.MaxValue
  }

  given int: NumericLimits[Int] with {
    def min: Int = Int.MinValue
    def max: Int = Int.MaxValue
  }

  given long: NumericLimits[Long] with {
    def min: Long = Long.MinValue
    def max: Long = Long.MaxValue
  }

  given float: FloatingLimits[Float] with {
    def min: Float = -Float.MaxValue
    def max: Float = Float.MaxValue
    def nan: Float = Float.NaN
    def infinity: Float = Float.PositiveInfinity
    def minPositiveValue: Float = Float.MinPositiveValue
  }

  given double: FloatingLimits[Double] with {
    def min: Double = -Double.MaxValue
    def max: Double = Double.MaxValue
    def nan: Double = Double.NaN
    def infinity: Double = Double.PositiveInfinity
    def minPositiveValue: Double = Double.MinPositiveValue
  }

  given decimal[P <: Int, S <: Int](using valid: Decimal.Valid[P, S]): NumericLimits[Decimal[P, S]] with {
    def min: Decimal[P, S] = {
      val maxUnscaledValue = BigInt(10).pow(valid.precision) - 1
      Decimal[P, S](BigDecimal(-maxUnscaledValue, valid.scale)).getOrElse(unreachable(
        s"Failed to create min Decimal[${valid.precision}, ${valid.scale}]"
      ))
    }

    def max: Decimal[P, S] = {
      val maxUnscaledValue = BigInt(10).pow(valid.precision) - 1
      Decimal[P, S](BigDecimal(maxUnscaledValue, valid.scale)).getOrElse(unreachable(
        s"Failed to create max Decimal[${valid.precision}, ${valid.scale}]"
      ))
    }
  }
}
