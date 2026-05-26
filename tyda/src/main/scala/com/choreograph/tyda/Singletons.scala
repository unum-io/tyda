package com.choreograph.tyda

import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.deriving.Mirror

/** A type class that provides access to the singletons values of an enum.
  *
  * If you want to ensure that all values of an enum are singletons, use
  * [[AllSingletons]] instead.
  */
opaque type Singletons[T] <: Seq[T] = Seq[T]

object Singletons {
  def apply[T: Singletons]: Singletons[T] = summon

  inline given [T](using m: Mirror.SumOf[T]): Singletons[T] = getSingletons[T, m.MirroredElemTypes]

  private inline def getSingletons[T, Elem <: Tuple]: Seq[T] =
    inline erasedValue[Elem] match {
      case _: EmptyTuple => Seq.empty
      case _: (h *: t) => summonFrom {
          case _: ValueOf[`h`] => staticCast[T](valueOf[`h`]) +: getSingletons[T, t]
          case _ => getSingletons[T, t]
        }
    }
}
