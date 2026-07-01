package com.choreograph.tyda

import scala.NamedTuple.NamedTuple
import scala.deriving.Mirror

import com.choreograph.tyda.TupleOperations.EqualSize

/** Type class for applying an [[ExprDepFn1]] to each field of a product type.
  *
  * Unlike [[TupleMapper]] which operates on positional tuples, `Mapper` works
  * with any type that has a `Mirror.ProductOf` (case classes, named tuples,
  * etc.) and produces a named tuple preserving the original field names.
  *
  * Example:
  * {{{
  * trait ToStr[T] extends ExprDepFn1[T] { type Out = String }
  * given ToStr[Int] with { def apply(e: Expr[Int]): Expr[String] = e.cast[String] }
  *
  * val mapper = summon[Mapper[ToStr, (name: String, age: Int)]]
  * // mapper.Out =:= (name: String, age: String)
  * }}}
  */
trait Mapper[HK[X] <: ExprDepFn1[X], T] extends ExprDepFn1[T] {
  type Out
}

object Mapper {
  type Aux[HK[X] <: ExprDepFn1[X], T, Out0] = Mapper[HK, T] { type Out = Out0 }

  given product[HK[X] <: ExprDepFn1[X], T: Mirror.ProductOf as m](using
      tupleMapper: TupleMapper[HK, m.MirroredElemTypes],
      names: StringLiterals[m.MirroredElemLabels],
      sizeEv: EqualSize[tupleMapper.Out, m.MirroredElemLabels]
  ): Aux[HK, T, NamedTuple[m.MirroredElemLabels, tupleMapper.Out]] =
    new Mapper[HK, T] {
      type Out = NamedTuple[m.MirroredElemLabels, tupleMapper.Out]
      def apply(e: Expr[T]): Expr[Out] = tupleMapper(e.toTuple).withNames[m.MirroredElemLabels]
    }
}
