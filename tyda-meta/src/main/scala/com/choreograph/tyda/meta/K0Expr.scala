package com.choreograph.tyda.meta

import scala.compiletime.error
import scala.deriving.Mirror

import shapeless3.deriving.CompleteOr
import shapeless3.deriving.Id
import shapeless3.deriving.Kind
import shapeless3.deriving.Labelling
import shapeless3.deriving.internals.ErasedProductInstances
import shapeless3.deriving.internals.Kinds

import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Field
import com.choreograph.tyda.shapeless3extras.mapConst

object K0Expr extends Kind[Any, Tuple, Id, Kinds.Head, Kinds.Tail] {
  type Labelled[F[_]] = [T] =>> (String, F[T])

  private class ExprProduct(expr: Expr[?], fields: Seq[Field[?]]) extends Product {
    override def canEqual(that: Any): Boolean = that.isInstanceOf[ExprProduct]
    override def productArity: Int = fields.size
    override def productElement(n: Int): Any = Expr.lift(ExprNode.Select(Expr.unlift(expr), fields(n).name))
    override def productPrefix: String = "ExprProduct"
  }
  private object ExprProduct {
    def apply(expr: Expr[?]): ExprProduct =
      expr.codec match {
        case Codec.Product(_, fields, _) =>
          new ExprProduct(expr, fields.mapConst[Field[?]]([t] => identity(_)))
        case _ => throw new IllegalArgumentException("Hopefully does not happen")
      }
  }

  private class ExprMirror[T: Codec]() extends Mirror.Product {
    type MirroredMonoType = ExprNode[T]
    def fromProduct(p: scala.Product): MirroredMonoType = {
      val elements = Tuple.fromArray(
        p.productIterator
          .map {
            case e: Expr[?] => e.node
            case e: AggregateExpr[?] => e.node
          }
          .toArray
      )
      // TYPE SAFETY: We only create ExprMirror when we have a Mirror.ProductOf[T]
      val productCodec = Codec[T].asInstanceOf[Codec.Product[T]]
      // TYPE SAFETY: The shapeless-3 api guarantees that the elements are of the correct type
      ExprNode.MakeProduct(elements.asInstanceOf, productCodec)
    }
  }

  /** Accessor for ExprMirror in inline macros */
  private def mkExprMirror[T: Codec]: ExprMirror[T] = ExprMirror[T]()

  /** Shadow to avoid deriving incorrect Instances */
  inline given mkInstances[F[_], T: Codec](using gen: Mirror of T): Instances[F, T] =
    inline gen match
      case given (Mirror.Product of T) => mkProductInstances[F, T]
      case given (Mirror.Sum of T) => mkCoproductInstancesError[F, T]

  /** Shadow the existing ProductInstances to inject ExprMirror */
  inline given mkProductInstances[F[_], T: Codec](using gen: Mirror.Product of T): ProductInstances[F, T] =
    ErasedProductInstances[this.type, F[T], LiftP[F, gen.MirroredElemTypes]](mkExprMirror[T])

  /** Shadow the existing CoproductInstances to give helpful error message */
  inline given mkCoproductInstancesError[F[_], T](using gen: Mirror.Sum of T): CoproductInstances[F, T] =
    error("Coproduct are currently not supported for K0Expr")

  // scalafix:off TypeSafetyComment
  // All the asInstanceOf are needed because of how shapeless3 works internally
  extension [F[_], T](inst: ProductInstances[F, T])

    /** Use the labelling to add the field name to each F[_] instance */
    def labelled(using labels: Labelling[T]): ProductInstances[Labelled[F], T] = {
      val labelIt = labels.elemLabels.iterator
      inst.mapK[Labelled[F]]([t] => (labelIt.next(), _))
    }

    /** Construct an Expr[T] by constructing each element with the given
      * function
      */
    inline def construct(f: [t] => F[t] => Expr[t]): Expr[T] =
      Expr.lift(inst.erasedConstruct(f.asInstanceOf).asInstanceOf)

    /** Map a product into an AggregateExp by aggregating each element with the
      * given function.
      */
    inline def mapAggregate(x: Expr[T])(f: [t] => (F[t], Expr[t]) => AggregateExpr[t]): AggregateExpr[T] =
      AggregateExpr.lift(inst.erasedMap(ExprProduct(x))(f.asInstanceOf).asInstanceOf)

    /** Fold over each type class of the product. */
    inline def foldLeft0[Acc](i: Acc)(f: [t] => (Acc, F[t]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft0(i)(f.asInstanceOf).asInstanceOf

    /** Fold over each element of the product using the given function. */
    inline def foldLeft[Acc](x: Expr[T])(i: Acc)(f: [t] => (Acc, F[t], Expr[t]) => CompleteOr[Acc]): Acc =
      inst.erasedFoldLeft(ExprProduct(x))(i)(f.asInstanceOf).asInstanceOf

    /** Fold over each instances and element accessors.
      *
      * This is the similar to foldLeft taking a `Expr[T]` but has an accessor
      * instead of the element itself. Since function expressions are composable
      * this gives more flexibility in how the fold can be used. For example it
      * can be used to implement foldLeft2.
      */
    def foldLeft[Acc](i: Acc)(f: [t] => (Acc, F[t], Expr[T] => Expr[t]) => Acc): Acc = {
      val (acc, _) = inst.foldLeft0((i, 0)) { [t] => (accAndIdx: (Acc, Int), field: F[t]) =>
        val (acc, idx) = accAndIdx
        val select = (expr: Expr[T]) => ExprProduct(expr).productElement(idx)
        (f(acc, field, select.asInstanceOf), idx + 1)
      }
      acc
    }

    /** Do a pairwise fold over two products using the given function. */
    inline def foldLeft2[Acc](x: Expr[T], y: Expr[T])(i: Acc)(
        f: [t] => (Acc, F[t], Expr[t], Expr[t]) => CompleteOr[Acc]
    ): Acc = inst.erasedFoldLeft2(ExprProduct(x), ExprProduct(y))(i)(f.asInstanceOf).asInstanceOf
  // scalafix:on
}
