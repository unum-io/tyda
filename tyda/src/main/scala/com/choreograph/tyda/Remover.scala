package com.choreograph.tyda

import scala.NamedTuple.NamedTuple
import scala.compiletime.constValue
import scala.deriving.Mirror

/** Type class for removing unique field of type `E` from a product type `P`.
  *
  * This is inspired by the Remove[1] in shapeless2 but defined for any type P
  * which has a Mirror.ProductOf instance and a unique field of type E.
  *
  * [1]
  * https://github.com/milessabin/shapeless/blob/de11db9ff9e26ea60dced5c6a745ad2a56f9f724/core/shared/src/main/scala/shapeless/ops/records.scala#L433-L439
  */
sealed trait Remover[T, E] {
  type Out
  def apply(p: T): Out
}

object Remover {
  type Of[E] = [T] =>> Remover[T, E]
  type From[T] = [E] =>> Remover[T, E]
  type Aux[T, E, Out0] = Remover[T, E] { type Out = Out0 }

  def apply[T, E](using s: Remover[T, E]): Remover[T, E] = s
  def remove[E](using DummyImplicit)[T](t: T)(using r: Remover[T, E]): r.Out = r(t)

  type RemoveElem[T <: Tuple, N <: Int] = Tuple.Concat[Tuple.Take[T, N], Tuple.Tail[Tuple.Drop[T, N]]]
  type Remove[NS <: Tuple, VS <: Tuple, N <: Int] = NamedTuple[RemoveElem[NS, N], RemoveElem[VS, N]]

  private[tyda] final case class Impl[T, E, NS <: Tuple, VS <: Tuple, N <: Int](n: N)(using
      m: Mirror.ProductOf[T] {
        type MirroredElemLabels = NS
        type MirroredElemTypes = VS
      }
  ) extends Remover[T, E] {
    type Out = Remove[NS, VS, n.type]

    private def removeValues(p: VS): RemoveElem[VS, n.type] = p.take(n) ++ p.drop(n).tail
    def apply(p: T): Out = NamedTuple.build()((removeValues(p.toMirroredElemTypes)))
  }

  inline given product[T: Mirror.ProductOf as m, E](using
      n: UniqueIndexOf[m.MirroredElemTypes, E]
  ): Aux[T, E, Remove[m.MirroredElemLabels, m.MirroredElemTypes, n.type]] =
    Impl[T, E, m.MirroredElemLabels, m.MirroredElemTypes, n.type](constValue[n.type])
}
