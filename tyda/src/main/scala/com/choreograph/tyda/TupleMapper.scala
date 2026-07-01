package com.choreograph.tyda

import com.choreograph.tyda.Expr

/** Helper class for applying [[ExprDepFn1]] to each element of tuple. */
trait TupleMapper[HK[X] <: ExprDepFn1[X], T <: Tuple] extends ExprDepFn1[T] {
  type Out <: Tuple
}

object TupleMapper {
  type Aux[HK[X] <: ExprDepFn1[X], T <: Tuple, Out1 <: Tuple] = TupleMapper[HK, T] { type Out = Out1 }

  given empty[HK[X] <: ExprDepFn1[X]]: Aux[HK, EmptyTuple, EmptyTuple] =
    new TupleMapper[HK, EmptyTuple] {
      type Out = EmptyTuple
      def apply(e: Expr[EmptyTuple]): Expr[Out] = e
    }

  given cons[HK[X] <: ExprDepFn1[X], T <: Tuple, H](using
      mapHead: HK[H],
      mapTail: TupleMapper[HK, T]
  ): Aux[HK, H *: T, mapHead.Out *: mapTail.Out] =
    new TupleMapper[HK, H *: T] {
      type Out = mapHead.Out *: mapTail.Out
      def apply(e: Expr[H *: T]): Expr[mapHead.Out *: mapTail.Out] = mapHead(e.head) *: mapTail(e.tail)
    }
}
