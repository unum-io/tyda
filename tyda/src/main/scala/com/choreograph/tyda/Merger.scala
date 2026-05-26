package com.choreograph.tyda

import scala.NamedTuple.NamedTuple
import scala.deriving.Mirror
import scala.quoted.*

import com.choreograph.tyda.QuotesUtils.labelsToTupleType
import com.choreograph.tyda.QuotesUtils.tupleElems
import com.choreograph.tyda.QuotesUtils.tupleLabels
import com.choreograph.tyda.QuotesUtils.typesToTupleType

/** Type class for merging two product types `Left` and `Right` into a new
  * product type `Out`.
  *
  * When merging, if there are fields with the same name in both `Left` and
  * `Right`, the field from `Right` have precedence and will be used in the
  * resulting type `Out`.
  *
  * For example: TODO: Remove nocompile when
  * https://github.com/scala/scala3/issues/24946 is fixed
  * ```scala sc:nocompile
  * case class A(x: Int, y: String)
  * case class B(y: Int, z: Boolean)
  * val merger = Merger[A, B]
  * val m: (x: Int, y: Int, z: Boolean) = merger(A(1, "hello"), B(2, true))
  * // (x = 1, y = 2, z = true)
  * ```
  *
  * This is similar to record merge in shapeless2.
  *
  * Note: This could be implemented using match types instead, but we did not
  * find a way to make that without significant compiletime cost for large
  * products.
  */
sealed trait Merger[Left, Right] {
  type Out
  def apply(left: Left, right: Right): Out
}

object Merger {
  type Aux[Left, Right, Out0] = Merger[Left, Right] { type Out = Out0 }

  def apply[Left, Right](using merger: Merger[Left, Right]): merger.type = merger

  transparent inline given derived[
      Left: Mirror.ProductOf as leftMirror,
      Right: Mirror.ProductOf as rightMirror
  ]: Merger[Left, Right] =
    ${
      derivedImpl[
        Left,
        Right,
        leftMirror.MirroredElemLabels,
        leftMirror.MirroredElemTypes,
        rightMirror.MirroredElemLabels,
        rightMirror.MirroredElemTypes
      ]
    }

  private def derivedImpl[
      Left: Type,
      Right: Type,
      LeftLabels: Type,
      LeftTypes: Type,
      RightLabels: Type,
      RightTypes: Type
  ](using Quotes): Expr[Merger[Left, Right]] =
    import quotes.reflect.*

    val leftLabels = tupleLabels[LeftLabels]
    val leftTypes = tupleElems[LeftTypes]
    val rightLabels = tupleLabels[RightLabels]
    val rightTypes = tupleElems[RightTypes]

    val rightLabelSet = rightLabels.toSet
    val leftIndicesToDrop = leftLabels
      .zipWithIndex
      .collect { case (label, idx) if rightLabelSet.contains(label) => idx }
      .toSet

    def filterLeftSide[A](items: Seq[A]): Seq[A] =
      items.zipWithIndex.collect { case (item, i) if !leftIndicesToDrop(i) => item }
    val filteredLeftLabels = filterLeftSide(leftLabels)
    val filteredLeftTypes = filterLeftSide(leftTypes)

    val outLabels = filteredLeftLabels ++ rightLabels
    val outTypes = filteredLeftTypes ++ rightTypes

    val outType = TypeRepr
      .of[NamedTuple]
      .appliedTo(List(labelsToTupleType(outLabels), typesToTupleType(outTypes)))
    val leftIndicesToDropExpr = Expr(IArray.from(leftIndicesToDrop))
    val leftArityExpr = Expr(leftLabels.length)

    outType.asType match {
      case '[out] =>
        '{ new UnsafeMerger[Left, Right, out]($leftIndicesToDropExpr, $leftArityExpr): Aux[Left, Right, out] }
    }

  private class MergedProduct(left: Product, right: Product, leftIndicesToKeep: IArray[Int]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productArity: Int = leftIndicesToKeep.length + right.productArity
    def productElement(n: Int): Any =
      if n < leftIndicesToKeep.length then left.productElement(leftIndicesToKeep(n))
      else right.productElement(n - leftIndicesToKeep.length)
  }

  // We generate indices to drop since we expect that to be smaller than indices to keep in the common case.
  private[tyda] class UnsafeMerger[Left, Right, Out0](leftIndicesToDrop: IArray[Int], leftArity: Int)
      extends Merger[Left, Right] {
    type Out = Out0

    private val leftIndicesToKeep: IArray[Int] =
      val dropSet = leftIndicesToDrop.toSet
      IArray.from((0 until leftArity).filterNot(dropSet))

    def apply(left: Left, right: Right): Out =
      val merged = Tuple.fromProduct(MergedProduct(
        // TYPE SAFETY: Mirror.ProductOf must exist when deriving Merger
        left.asInstanceOf[Product],
        // TYPE SAFETY: Mirror.ProductOf must exist when deriving Merger
        right.asInstanceOf[Product],
        leftIndicesToKeep
      ))
      // TYPE SAFETY: Correctness of the macro implementation ensures that the resulting type is correct
      merged.asInstanceOf[Out]
  }
}
