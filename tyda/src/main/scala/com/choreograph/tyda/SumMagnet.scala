package com.choreograph.tyda

import scala.annotation.implicitNotFound

import com.choreograph.tyda.Decimal.MaxPrecision

@implicitNotFound("""Sum is not supported for type ${T}.
Summation is currently only supported for numeric types: Byte, Short, Int, Long, Float, Double.
Opaque types wrapping these needs to provide a SumManget instances in their companion object.""")
sealed trait SumMagnet[T] extends Serializable {
  type Result
  def toResult(t: T): Result
  def codec: Codec[Result]
  def add(x: Result, y: Result): Result
}

object SumMagnet {
  type Aux[T, R] = SumMagnet[T] { type Result = R }

  def apply[T](using magnet: SumMagnet[T]): SumMagnet.Aux[T, magnet.Result] = magnet

  private[tyda] final case class AsLong[T](integral: Integral[T]) extends SumMagnet[T] {
    type Result = Long
    def codec: Codec[Long] = summon
    def toResult(t: T): Long = integral.toLong(t)
    def add(x: Long, y: Long): Long = Math.addExact(x, y)
  }
  private[tyda] object AsLong {
    def apply[T: Integral](): SumMagnet.Aux[T, Long] = AsLong[T](summon)
  }

  private[tyda] final case class AsDouble[T](fractional: Fractional[T]) extends SumMagnet[T] {
    type Result = Double
    def codec: Codec[Double] = summon
    def toResult(t: T): Double = fractional.toDouble(t)
    def add(x: Double, y: Double): Double = x + y
  }
  private[tyda] object AsDouble {
    def apply[T: Fractional](): SumMagnet.Aux[T, Double] = AsDouble[T](summon)
  }

  private[tyda] final case class AsDecimal[S <: Int]()(using valid: Decimal.Valid[MaxPrecision, S])
      extends SumMagnet[Decimal[MaxPrecision, S]] {
    type Result = Decimal[MaxPrecision, S]
    def codec: Codec[Decimal[MaxPrecision, S]] = summon
    def toResult(t: Decimal[MaxPrecision, S]): Decimal[MaxPrecision, S] = t
    def add(x: Decimal[MaxPrecision, S], y: Decimal[MaxPrecision, S]): Decimal[MaxPrecision, S] =
      Decimal(x.toBigDecimal + y.toBigDecimal).getOrElse {
        throw new ArithmeticException(s"Sum of $x and $y exceeds the maximum precision of Decimal(${valid
            .precision}, ${valid.scale})")
      }
  }

  private[tyda] final case class Nullable[T, R](inner: SumMagnet.Aux[T, R]) extends SumMagnet[Option[T]] {
    type Result = Option[R]
    def codec: Codec[Result] = Codec.option(using inner.codec)
    def toResult(t: Option[T]): Result = t.map(inner.toResult)
    def add(x: Result, y: Result): Result = x.zip(y).map(inner.add).orElse(x).orElse(y)
  }

  given byte: SumMagnet.Aux[Byte, Long] = AsLong[Byte]()
  given short: SumMagnet.Aux[Short, Long] = AsLong[Short]()
  given int: SumMagnet.Aux[Int, Long] = AsLong[Int]()
  given long: SumMagnet.Aux[Long, Long] = AsLong[Long]()
  given float: SumMagnet.Aux[Float, Double] = AsDouble[Float]()
  given double: SumMagnet.Aux[Double, Double] = AsDouble[Double]()
  given decimal[S <: Int](using
      Decimal.Valid[MaxPrecision, S]
  ): SumMagnet.Aux[Decimal[MaxPrecision, S], Decimal[MaxPrecision, S]] = AsDecimal[S]()
  given option[T, R](using inner: SumMagnet.Aux[T, R]): SumMagnet.Aux[Option[T], Option[R]] = Nullable(inner)
}
