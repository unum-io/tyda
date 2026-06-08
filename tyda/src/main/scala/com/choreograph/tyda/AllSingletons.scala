package com.choreograph.tyda
import scala.deriving.Mirror

/** A type class that provides access and serves as evidence that all values of
  * a enum are singletons.
  */
opaque type AllSingletons[T] <: Seq[T] = Seq[T]

object AllSingletons {
  def apply[T: AllSingletons]: AllSingletons[T] = summon

  given [T](using m: Mirror.SumOf[T], tuple: AllSingletonsTuple[T, m.MirroredElemTypes]): AllSingletons[T] =
    tuple

  opaque type AllSingletonsTuple[T, Elem <: Tuple] = List[T]

  object AllSingletonsTuple {
    given empty[T]: AllSingletonsTuple[T, EmptyTuple] = Nil
    given [T, H <: T: ValueOf, Tail <: Tuple](using
        tail: AllSingletonsTuple[T, Tail]
    ): AllSingletonsTuple[T, H *: Tail] = valueOf[H] :: tail
  }
}
