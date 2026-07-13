package com.choreograph.tyda

import scala.annotation.implicitNotFound

/** Type class that serves as evidence that a type supports numeric operations
  * (+, -, *, /) in the Expr API.
  *
  * For opaque types that should support numeric operations one just needs to
  * provide a given instance delegating to the underlying type:
  * ```scala
  * opaque type MyLong = Long
  * object MyLong {
  *   given Num[MyLong] = Num[Long]
  * }
  * ```
  *
  * Note: for custom types where only a subset of operations makes sense (e.g.
  * only + and - for a Duration-like type) prefer defining dedicated extension
  * methods rather than providing a Num instance.
  */
@implicitNotFound(
  "Type ${T} does not support numeric operations.\n" +
    "If ${T} is a type parameter consider adding a context bound: [${T}: Num]\n" +
    "If ${T} is an opaque type consider adding a given Num[${T}] = Num.<evidence> to the companion object\n"
)
sealed trait Num[T] {
  def plus(lhs: T, rhs: T): T
  def minus(lhs: T, rhs: T): T
  def times(lhs: T, rhs: T): T
  def quot(lhs: T, rhs: T): T
  def negate(t: T): T
}

object Num {
  def apply[T: Num]: Num[T] = summon

  sealed trait Primitive[T] extends Num[T]
  sealed trait Integral[T] extends Primitive[T]

  private def exactByte(value: Int): scala.Byte =
    if value < scala.Byte.MinValue || value > scala.Byte.MaxValue then
      throw new ArithmeticException("Byte overflow")
    else value.toByte

  private def exactShort(value: Int): scala.Short =
    if value < scala.Short.MinValue || value > scala.Short.MaxValue then
      throw new ArithmeticException("Short overflow")
    else value.toShort

  private def checkedFloat(operation: => Float): Float =
    if operation.isInfinite() then throw new ArithmeticException(s"Float overflow: $operation")
    operation

  private def checkedDouble(operation: => Double): Double =
    if operation.isInfinite() then throw new ArithmeticException(s"Double overflow: $operation")
    operation

  private[tyda] case object Byte extends Integral[scala.Byte] {
    def plus(lhs: scala.Byte, rhs: scala.Byte): scala.Byte = exactByte(lhs.toInt + rhs.toInt)
    def minus(lhs: scala.Byte, rhs: scala.Byte): scala.Byte = exactByte(lhs.toInt - rhs.toInt)
    def times(lhs: scala.Byte, rhs: scala.Byte): scala.Byte = exactByte(lhs.toInt * rhs.toInt)
    def quot(lhs: scala.Byte, rhs: scala.Byte): scala.Byte = exactByte(lhs.toInt / rhs.toInt)
    def negate(t: scala.Byte): scala.Byte = exactByte(-t.toInt)
  }

  private[tyda] case object Short extends Integral[scala.Short] {
    def plus(lhs: scala.Short, rhs: scala.Short): scala.Short = exactShort(lhs.toInt + rhs.toInt)
    def minus(lhs: scala.Short, rhs: scala.Short): scala.Short = exactShort(lhs.toInt - rhs.toInt)
    def times(lhs: scala.Short, rhs: scala.Short): scala.Short = exactShort(lhs.toInt * rhs.toInt)
    def quot(lhs: scala.Short, rhs: scala.Short): scala.Short = exactShort(lhs.toInt / rhs.toInt)
    def negate(t: scala.Short): scala.Short = exactShort(-t.toInt)
  }

  private[tyda] case object Int extends Integral[scala.Int] {
    def plus(lhs: scala.Int, rhs: scala.Int): scala.Int = Math.addExact(lhs, rhs)
    def minus(lhs: scala.Int, rhs: scala.Int): scala.Int = Math.subtractExact(lhs, rhs)
    def times(lhs: scala.Int, rhs: scala.Int): scala.Int = Math.multiplyExact(lhs, rhs)
    def quot(lhs: scala.Int, rhs: scala.Int): scala.Int = lhs / rhs
    def negate(t: scala.Int): scala.Int = Math.negateExact(t)
  }

  private[tyda] case object Long extends Integral[scala.Long] {
    def plus(lhs: scala.Long, rhs: scala.Long): scala.Long = Math.addExact(lhs, rhs)
    def minus(lhs: scala.Long, rhs: scala.Long): scala.Long = Math.subtractExact(lhs, rhs)
    def times(lhs: scala.Long, rhs: scala.Long): scala.Long = Math.multiplyExact(lhs, rhs)
    def quot(lhs: scala.Long, rhs: scala.Long): scala.Long = lhs / rhs
    def negate(t: scala.Long): scala.Long = Math.negateExact(t)
  }

  private[tyda] case object Float extends Primitive[scala.Float] {
    def plus(lhs: scala.Float, rhs: scala.Float): scala.Float = checkedFloat(lhs + rhs)
    def minus(lhs: scala.Float, rhs: scala.Float): scala.Float = checkedFloat(lhs - rhs)
    def times(lhs: scala.Float, rhs: scala.Float): scala.Float = checkedFloat(lhs * rhs)
    def quot(lhs: scala.Float, rhs: scala.Float): scala.Float =
      if rhs == 0 then throw new ArithmeticException("/ by zero") else checkedFloat(lhs / rhs)
    def negate(t: scala.Float): scala.Float = -t
  }

  private[tyda] case object Double extends Primitive[scala.Double] {
    def plus(lhs: scala.Double, rhs: scala.Double): scala.Double = checkedDouble(lhs + rhs)
    def minus(lhs: scala.Double, rhs: scala.Double): scala.Double = checkedDouble(lhs - rhs)
    def times(lhs: scala.Double, rhs: scala.Double): scala.Double = checkedDouble(lhs * rhs)
    def quot(lhs: scala.Double, rhs: scala.Double): scala.Double =
      if rhs == 0 then throw new ArithmeticException("/ by zero") else checkedDouble(lhs / rhs)
    def negate(t: scala.Double): scala.Double = -t
  }

  given byte: Num[scala.Byte] = Byte
  given short: Num[scala.Short] = Short
  given int: Num[scala.Int] = Int
  given long: Num[scala.Long] = Long
  given float: Num[scala.Float] = Float
  given double: Num[scala.Double] = Double
}
