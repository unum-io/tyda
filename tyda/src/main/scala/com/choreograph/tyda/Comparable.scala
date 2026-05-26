package com.choreograph.tyda

import scala.annotation.implicitNotFound

/** Type class that serves as evidence that a type can be compared in the Expr
  * API.
  *
  * For opaque types backed that should be comparable one just needs to provide
  * the a given instance for the Comparable type.
  * ```scala
  * opaque type MyLong = Long
  * object MyLong {
  *   given Comparable[MyLong] = Comparable[Long]
  * }
  * ```
  */
@implicitNotFound(
  "Type ${T} is not comparable.\n" +
    "If ${T} is a type parameter consider adding a context bound: [${T}: Comparable]\n" +
    "If ${T} is an opaque type consider adding a given Comparable[${T}] = Comparable.<evidence> to the companion object\n"
)
sealed trait Comparable[T] extends Serializable

object Comparable {
  def apply[T: Comparable as c]: Comparable[T] = c

  sealed trait Primitive[T] extends Comparable[T]

  private[tyda] case object Boolean extends Primitive[scala.Boolean]
  private[tyda] case object Byte extends Primitive[scala.Byte]
  private[tyda] case object Short extends Primitive[scala.Short]
  private[tyda] case object Int extends Primitive[scala.Int]
  private[tyda] case object Long extends Primitive[scala.Long]
  private[tyda] case object Float extends Primitive[scala.Float]
  private[tyda] case object Double extends Primitive[scala.Double]
  private[tyda] final case class Decimal[S <: Int, P <: Int]()
      extends Primitive[com.choreograph.tyda.Decimal[S, P]]
  private[tyda] case object String extends Primitive[Predef.String]
  private[tyda] case object Duration extends Primitive[com.choreograph.tyda.Duration]
  private[tyda] case object Date extends Primitive[com.choreograph.tyda.Date]
  private[tyda] case object Timestamp extends Primitive[com.choreograph.tyda.Timestamp]

  given boolean: Comparable[scala.Boolean] = Boolean
  given byte: Comparable[scala.Byte] = Byte
  given short: Comparable[scala.Short] = Short
  given int: Comparable[scala.Int] = Int
  given long: Comparable[scala.Long] = Long
  given float: Comparable[scala.Float] = Float
  given double: Comparable[scala.Double] = Double
  given decimal[S <: Int, P <: Int]: Comparable[com.choreograph.tyda.Decimal[S, P]] = Decimal()
  given string: Comparable[Predef.String] = String
  given date: Comparable[com.choreograph.tyda.Date] = Date
  given timestamp: Comparable[com.choreograph.tyda.Timestamp] = Timestamp
  given duration: Comparable[com.choreograph.tyda.Duration] = Duration
}
