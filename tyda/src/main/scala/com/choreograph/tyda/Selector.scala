package com.choreograph.tyda

import scala.deriving.Mirror

import com.choreograph.tyda.asProduct

/** Type class for extracting unique field of type `E` from the product type
  * `P`.
  *
  * This inspired by the Selector in shapeless2 but defined for any type P which
  * has a Mirror.ProductOf instance and a unique field of type E.
  */
sealed trait Selector[P, E] {
  def apply(p: P): E
}

object Selector {
  type From[P] = [E] =>> Selector[P, E]
  type To[E] = [P] =>> Selector[P, E]

  def apply[P, E](using s: Selector[P, E]): Selector[P, E] = s
  def select[E](using DummyImplicit)[P](p: P)(using s: Selector[P, E]): E = s(p)

  private[tyda] final case class UnsafeImpl[P: Mirror.ProductOf, E](index: Int) extends Selector[P, E] {
    def apply(p: P): E =
      // TYPE SAFETY: UniqueIndexOf ensures that the index is valid and the element is of type E.
      p.asProduct.productElement(index).asInstanceOf[E]
  }

  given [P: Mirror.ProductOf as m, E](using index: UniqueIndexOf[m.MirroredElemTypes, E]): Selector[P, E] =
    UnsafeImpl[P, E](index)
}
