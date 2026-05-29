package com.choreograph.tyda
import scala.deriving.Mirror

/** A type class that provides access and serves as evidence that all values of
  * a enum are singletons.
  */
trait AllSingletons[T] {
  def values: Seq[T]
}

object AllSingletons {
  def apply[T: AllSingletons]: AllSingletons[T] = summon

  given [T](using m: Mirror.SumOf[T], tuple: AllSingletonsTuple[T, m.MirroredElemTypes]): AllSingletons[T] =
    new AllSingletons[T] {
      def values: Seq[T] = tuple.values
    }

  trait AllSingletonsTuple[T, Elem <: Tuple] {
    def values: List[T]
  }

  object AllSingletonsTuple {
    given empty[T]: AllSingletonsTuple[T, EmptyTuple] with {
      def values: List[T] = Nil
    }

    given [T, H <: T: ValueOf, Tail <: Tuple](using tail: AllSingletonsTuple[T, Tail]): AllSingletonsTuple[
      T,
      H *: Tail
    ] with {
      def values: List[T] = valueOf[H] :: tail.values
    }
  }
}
