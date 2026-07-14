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

  private def exactByte(value: Int): Byte =
    if value < Byte.MinValue || value > Byte.MaxValue then throw new ArithmeticException("Byte overflow")
    else value.toByte

  private def exactShort(value: Int): Short =
    if value < Short.MinValue || value > Short.MaxValue then throw new ArithmeticException("Short overflow")
    else value.toShort

  private def checkedFloat(lhs: Float, rhs: Float)(operation: => Float): Float =
    if operation.isInfinite && lhs.isFinite && rhs.isFinite then
      throw new ArithmeticException(s"Float overflow: $operation")
    operation

  private def checkedDouble(lhs: Double, rhs: Double)(operation: => Double): Double =
    if operation.isInfinite && lhs.isFinite && rhs.isFinite then
      throw new ArithmeticException(s"Double overflow: $operation")
    operation

  given Num[Byte] =
    new Integral[Byte] {
      def plus(lhs: Byte, rhs: Byte): Byte = exactByte(lhs.toInt + rhs.toInt)
      def minus(lhs: Byte, rhs: Byte): Byte = exactByte(lhs.toInt - rhs.toInt)
      def times(lhs: Byte, rhs: Byte): Byte = exactByte(lhs.toInt * rhs.toInt)
      def quot(lhs: Byte, rhs: Byte): Byte = exactByte(lhs.toInt / rhs.toInt)
      def negate(t: Byte): Byte = exactByte(-t.toInt)
    }

  given Num[Short] =
    new Integral[Short] {
      def plus(lhs: Short, rhs: Short): Short = exactShort(lhs.toInt + rhs.toInt)
      def minus(lhs: Short, rhs: Short): Short = exactShort(lhs.toInt - rhs.toInt)
      def times(lhs: Short, rhs: Short): Short = exactShort(lhs.toInt * rhs.toInt)
      def quot(lhs: Short, rhs: Short): Short = exactShort(lhs.toInt / rhs.toInt)
      def negate(t: Short): Short = exactShort(-t.toInt)
    }

  given Num[Int] =
    new Integral[Int] {
      def plus(lhs: Int, rhs: Int): Int = Math.addExact(lhs, rhs)
      def minus(lhs: Int, rhs: Int): Int = Math.subtractExact(lhs, rhs)
      def times(lhs: Int, rhs: Int): Int = Math.multiplyExact(lhs, rhs)
      def quot(lhs: Int, rhs: Int): Int = lhs / rhs
      def negate(t: Int): Int = Math.negateExact(t)
    }

  given Num[Long] =
    new Integral[Long] {
      def plus(lhs: Long, rhs: Long): Long = Math.addExact(lhs, rhs)
      def minus(lhs: Long, rhs: Long): Long = Math.subtractExact(lhs, rhs)
      def times(lhs: Long, rhs: Long): Long = Math.multiplyExact(lhs, rhs)
      def quot(lhs: Long, rhs: Long): Long = lhs / rhs
      def negate(t: Long): Long = Math.negateExact(t)
    }

  given Num[Float] =
    new Primitive[Float] {
      def plus(lhs: Float, rhs: Float): Float = checkedFloat(lhs, rhs)(lhs + rhs)
      def minus(lhs: Float, rhs: Float): Float = checkedFloat(lhs, rhs)(lhs - rhs)
      def times(lhs: Float, rhs: Float): Float = checkedFloat(lhs, rhs)(lhs * rhs)
      def quot(lhs: Float, rhs: Float): Float =
        if rhs == 0 then throw new ArithmeticException("/ by zero") else checkedFloat(lhs, rhs)(lhs / rhs)
      def negate(t: Float): Float = -t
    }

  given Num[Double] =
    new Primitive[Double] {
      def plus(lhs: Double, rhs: Double): Double = checkedDouble(lhs, rhs)(lhs + rhs)
      def minus(lhs: Double, rhs: Double): Double = checkedDouble(lhs, rhs)(lhs - rhs)
      def times(lhs: Double, rhs: Double): Double = checkedDouble(lhs, rhs)(lhs * rhs)
      def quot(lhs: Double, rhs: Double): Double =
        if rhs == 0 then throw new ArithmeticException("/ by zero") else checkedDouble(lhs, rhs)(lhs / rhs)
      def negate(t: Double): Double = -t
    }
}
