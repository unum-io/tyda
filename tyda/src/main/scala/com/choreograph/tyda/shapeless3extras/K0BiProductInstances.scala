package com.choreograph.tyda.shapeless3extras

import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.summonInline
import scala.deriving.Mirror

import shapeless3.deriving.CompleteOr
import shapeless3.deriving.internals.ErasedProductInstances
import shapeless3.deriving.internals.ErasedProductInstancesN

/** Type-safe wrapper over [[ErasedProductInstances]] for type classes with two
  * type parameters.
  *
  * Where shapeless3's `ProductInstances[F[_], T]` stores one instance of
  * `F[elem_i]` per field of product `T`, this stores one instance of
  * `F[elem_i_A, elem_i_B]` for each pair of corresponding fields in products
  * `A` and `B`.
  *
  * Construction summons `F[a_i, b_i]` for each field pair inline, and the
  * resulting instances are stored in a flat array — identical layout to
  * shapeless3's erased representation. The opaque type ensures that the typed
  * operations (`map`, `mapK`) are the only way to interact with the underlying
  * array.
  *
  * @tparam F
  *   the two-parameter type class
  * @tparam A
  *   the source product type
  * @tparam B
  *   the target product type
  *
  * @example
  *   {{{
  * trait Convert[A, B]:
  *   def apply(a: A): B
  *
  * case class Source(x: Int, y: Short)
  * case class Target(x: Long, y: Int)
  *
  * // Given Convert[Int, Long] and Convert[Short, Int] are in scope,
  * // summons one instance per field pair:
  * val instances = summon[K0BiProductInstances[Convert, Source, Target]]
  *
  * // Apply each instance field-wise to transform Source into Target:
  * val target: Target = instances.map(Source(42, 1))(
  *   [a, b] => (conv: Convert[a, b], value: a) => conv(value)
  * )
  *   }}}
  */
private[tyda] opaque type K0BiProductInstances[F[_, _], A, B] = ErasedProductInstances[Any, B]

object K0BiProductInstances {
  extension [F[_, _], A, B](instances: K0BiProductInstances[F, A, B]) {

    /** Transforms the stored instances from `F` to `G` without changing the
      * product types.
      */
    def mapK[G[_, _]](f: [a, b] => F[a, b] => G[a, b]): K0BiProductInstances[G, A, B] = {
      // TYPE SAFETY: The is the runtime representation of f.
      val erasedF = f.asInstanceOf[Any => Any]
      // TYPE SAFETY: This is safe if we are using the shapeless3 internals correctly.
      instances.erasedMapK(erasedF).asInstanceOf[K0BiProductInstances[G, A, B]]
    }

    /** Applies each stored `F[a_i, b_i]` to the corresponding field of `x` to
      * produce a `B`.
      *
      * Each field of the source product `x` is paired with its corresponding
      * `F` instance and passed to `f`, which produces the matching field of the
      * target product `B`. The target is constructed via `B`'s
      * [[Mirror.ProductOf]].
      *
      * @param x
      *   the source product value
      * @param f
      *   a polymorphic function that uses each `F[a_i, b_i]` to convert field
      *   `a_i` to `b_i`
      * @return
      *   the target product with each field transformed
      */
    def map(x: A)(f: [a, b] => (F[a, b], a) => b): B = {
      // TYPE SAFETY: The is the runtime representation of f.
      val erasedF = f.asInstanceOf[(Any, Any) => Any]
      // TYPE SAFETY: This is safe if we are using the shapeless3 internals correctly.
      instances.erasedMap(x)(erasedF).asInstanceOf[B]
    }

    /** Folds over the stored instances with an accumulator of type `Acc`. */
    def foldLeft0[Acc](z: Acc)(f: [a, b] => (Acc, F[a, b]) => CompleteOr[Acc]): Acc = {
      // TYPE SAFETY: The is the runtime representation of f.
      val erasedF = f.asInstanceOf[(Any, Any) => Any]
      // TYPE SAFETY: This is safe if we are using the shapeless3 internals correctly.
      instances.erasedFoldLeft0(z)(erasedF).asInstanceOf[Acc]
    }

    /** Map each instance to a constant type and collect it to a Seq. */
    def mapConst[C](f: [a, b] => F[a, b] => C): Seq[C] =
      foldLeft0[Vector[C]](Vector.empty)([a, b] => (acc, inst) => acc :+ f(inst))
  }

  /** Summons `F[a_i, b_i]` for each pair of corresponding fields in `A` and
    * `B`.
    *
    * Both `A` and `B` must be product types with the same number of fields, and
    * an instance of `F[a_i, b_i]` must be available for every field pair. A
    * compile-time error is raised if the products have different arities or if
    * any field pair lacks an `F` instance.
    */
  inline given [F[_, _], A: Mirror.ProductOf as mA, B: Mirror.ProductOf as mB]
      : K0BiProductInstances[F, A, B] =
    ErasedProductInstancesN(mB, collectErased[F, mA.MirroredElemTypes, mB.MirroredElemTypes])

  private inline def collectErased[F[_, _], From <: Tuple, To <: Tuple]: Array[Any] =
    inline (erasedValue[From], erasedValue[To]) match
      case _: (EmptyTuple, EmptyTuple) => Array.empty[Any]
      case _: ((fh *: ft), (th *: tt)) => summonInline[F[fh, th]] +: collectErased[F, ft, tt]
      case _: (EmptyTuple, _) | _: (_, EmptyTuple) =>
        error("Cannot derive K0BiProductInstances for products of different arities")
}
