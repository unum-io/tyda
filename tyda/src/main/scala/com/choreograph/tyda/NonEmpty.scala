package com.choreograph.tyda

import scala.annotation.targetName
import scala.collection.Factory
import scala.collection.IterableOps
import scala.collection.immutable.MapOps
import scala.collection.immutable.SeqOps

/** A type that represents a non-empty collection of type C.
  *
  * ```scala
  * val neList: NonEmpty[List[Int]] = NonEmpty[List](1, 2, 3)
  * val neSet: NonEmpty[Set[Int]] = neList.to(Set)
  * val mapped: NonEmpty[List[Int]] = neList.map(_ * 2)
  * val prepended: NonEmpty[List[Int]] = 0 :: neList
  * val asSeq: Seq[Int] = neList
  * ```
  *
  * The opaque type does not have any bounds to allow us to "override" methods
  * with specific versions that maintain the NonEmpty invariant. But that
  * `NonEmpty[C] <:< C` is provided as a generialized type constraint, this
  * means that `NonEmpty[C]` can be treated as `C` in many places and member
  * functions of `C` are accessible on `NonEmpty[C]`.
  */
private[tyda] opaque type NonEmpty[+C <: Iterable[?]] = C

private[tyda] object NonEmpty {

  /** Creates a NonEmpty collection from the given head and elements.
    *
    * ```scala
    * val neList: NonEmpty[List[Int]] = NonEmpty[List](1, 2, 3)
    * val neSet: NonEmpty[Set[Int]] = NonEmpty[Set](1, 2, 3)
    * ```
    *
    * Note: The `DummyImplicit` here is used to curry the type parameters so
    * users only specifys the collection type and the element type is inferred.
    * This is the suggested workaround from
    * https://docs.scala-lang.org/sips/47.html#type-currying
    */
  inline def apply[C[X] <: Iterable[X]](using
      DummyImplicit
  )[T](head: T, elements: T*)(using factory: Factory[T, C[T]]): NonEmpty[C[T]] =
    factory.fromSpecific(head +: elements)

  /** Creates a NonEmpty collection from the given collection if it is
    * non-empty.
    *
    * ```scala
    * val neListOpt: Option[NonEmpty[List[Int]]] = NonEmpty.from(List(1, 2, 3)) // Some(List(1, 2, 3))
    * val emptyOpt: Option[NonEmpty[List[Int]]] = NonEmpty.from(List()) // None
    * ```
    */
  inline def from[C <: Iterable[?]](coll: C): Option[NonEmpty[C]] = Option.when(coll.nonEmpty)(coll)

  extension [C <: Iterable[?]](coll: NonEmpty[C]) {

    /** Access to the underlying collection.
      */
    inline def underlying: C = coll
  }

  /** This extension provides the methods normally provided by Iterable.
    * Refinement on the return type is provided where possible.
    *
    * For documentation on the methods see the corresponding methods on the `C`.
    *
    * Note: By extending IterableOps explicitly, we get more refined return
    * types from the underlying methods
    */
  extension [A, CC[X] <: Iterable[X], C <: CC[A] & IterableOps[A, CC, CC[A]]](coll: NonEmpty[C]) {

    /** Same as [[scala.collection.IterableOps.map]] but refined to NonEmpty.
      */
    inline def map[U](f: A => U): NonEmpty[CC[U]] = coll.map(f)

    /** Same as [[scala.collection.IterableOps.zipWithIndex]] but refined to
      * NonEmpty.
      */
    inline def zipWithIndex: NonEmpty[CC[(A, Int)]] = coll.zipWithIndex

    /** Same as [[scala.collection.IterableOps.zip]] but refined to NonEmpty.
      *
      * Zips this NonEmpty collection with another NonEmpty collection.
      */
    inline def zip[B](that: NonEmpty[Iterable[B]]): NonEmpty[CC[(A, B)]] = coll.zip(that)

    /** Same as [[scala.collection.IterableOps.zip]].
      *
      * Needs to be defined so that both versions participate in overload
      * resolution at the same time.
      */
    @targetName("zipIterable")
    inline def zip[B](that: Iterable[B]): CC[(A, B)] = coll.zip(that)

    /** Same as [[scala.collection.IterableOps.scanLeft]] but refined to
      * NonEmpty.
      */
    inline def scanLeft[B](z: B)(op: (B, A) => B): NonEmpty[CC[B]] = coll.scanLeft(z)(op)

    /** Same as [[scala.collection.IterableOps.++]] but refined to NonEmpty.
      */
    inline infix def ++[B >: A](suffix: Iterable[B]): NonEmpty[CC[B]] = coll ++ suffix

    /** Same as [[scala.collection.IterableOps.concat]] but refined to NonEmpty.
      */
    inline def concat[B >: A](suffix: Iterable[B]): NonEmpty[CC[B]] = coll.concat(suffix)

    /** Same as [[scala.collection.IterableOps.groupBy]] but returns a Map with
      * NonEmpty values.
      */
    inline def groupBy[K](f: A => K): NonEmpty[Map[K, NonEmpty[CC[A]]]] = coll.groupBy(f)

    /** Same as [[scala.collection.IterableOps.groupMap]] but returns a Map with
      * NonEmpty values.
      */
    inline def groupMap[K, B](key: A => K)(f: A => B): NonEmpty[Map[K, NonEmpty[CC[B]]]] =
      coll.groupMap(key)(f)

    /** Same as [[scala.collection.IterableOps.groupMapReduce]].
      */
    inline def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): NonEmpty[Map[K, B]] =
      coll.groupMapReduce(key)(f)(reduce)

    /** Same as [[scala.collection.IterableOnceOps.to]] but refined to NonEmpty.
      */
    inline def to[C1 <: Iterable[?]](factory: Factory[A, C1]): NonEmpty[C1] = coll.to(factory)
  }

  extension [A, CC[X] <: Seq[X], C <: CC[A] & SeqOps[A, CC, CC[A]]](coll: NonEmpty[C]) {

    /** Same as [[scala.collection.SeqOps.prepended]] but refined to NonEmpty.
      */
    inline def prepended[B >: A](elem: B): NonEmpty[CC[B]] = coll.prepended(elem)

    /** Same as [[scala.collection.SeqOps.appended]] but refined to NonEmpty.
      */
    inline def appended[B >: A](elem: B): NonEmpty[CC[B]] = coll.appended(elem)

    /** Same as [[scala.collection.SeqOps.+:]] but refined to NonEmpty.
      */
    inline infix def :+[B >: A](elem: B): NonEmpty[CC[B]] = coll :+ elem

    /** Same as [[scala.collection.SeqOps.reverse]] but refined to NonEmpty.
      */
    inline def reverse: NonEmpty[CC[A]] = coll.reverse

    /** Same as [[scala.collection.SeqOps.distinct]] but refined to NonEmpty.
      */
    inline def distinct: NonEmpty[CC[A]] = coll.distinct

    /** Same as [[scala.collection.SeqOps.distinctBy]] but refined to NonEmpty.
      */
    inline def distinctBy[B](f: A => B): NonEmpty[CC[A]] = coll.distinctBy(f)

    /** Same as [[scala.collection.SeqOps.sorted]] but refined to NonEmpty.
      */
    inline def sorted[B >: A: Ordering]: NonEmpty[CC[A]] = coll.sorted

    /** Same as [[scala.collection.SeqOps.sortBy]] but refined to NonEmpty.
      */
    inline def sortBy[B: Ordering](f: A => B): NonEmpty[CC[A]] = coll.sortBy(f)
  }

  extension [K, V, CC[X, +Y] <: Iterable[(X, Y)] & MapOps[X, Y, CC, ?], C <: CC[K, V] & MapOps[K, V, CC, C]](
      coll: NonEmpty[C]
  ) {

    /** Same as [[scala.collection.MapOps.updated]] but refined to NonEmpty.
      */
    inline def updated[V1 >: V](key: K, value: V1): NonEmpty[CC[K, V1]] = coll.updated(key, value)

    /** Same as [[scala.collection.MapOps.+]] but refined to NonEmpty.
      */
    inline infix def +[V1 >: V](kv: (K, V1)): NonEmpty[CC[K, V1]] = coll + kv

    /** Same as [[scala.collection.MapOps.++]] but refined to NonEmpty.
      */
    infix def ++[V1 >: V](kvs: Iterable[(K, V1)]): NonEmpty[CC[K, V1]] = coll ++ kvs

    /** Same as [[scala.collection.MapOps.transform]] but refined to NonEmpty.
      */
    inline def transform[W](f: (K, V) => W): NonEmpty[CC[K, W]] = coll.transform(f)

    /** Same as [[scala.collection.MapOps.keySet]] but refined to NonEmpty.
      */
    inline def keySet: NonEmpty[Set[K]] = coll.keySet
  }

  /** Methods for prepending an element to a NonEmpty collection.
    *
    * Because of how right associative methods work, we need to define the
    * extension method on the element instead of the collection. For more
    * details see:
    * https://docs.scala-lang.org/scala3/reference/contextual/right-associative-extension-methods.html
    */
  extension [A](elem: A) {

    /** Same as [[List.::]] but refined to NonEmpty.
      */
    inline infix def ::[B <: A](coll: NonEmpty[List[B]]): NonEmpty[List[A]] = elem :: coll

    /** Same as [[scala.collection.SeqOps.+:]] but refined to NonEmpty.
      */
    inline infix def +:[B >: A, CC[X] <: Iterable[X], C <: CC[B] & SeqOps[B, CC, CC[B]]](
        coll: NonEmpty[C]
    ): NonEmpty[CC[B]] = elem +: coll
  }

  /** We can provide this as a bound on the opaque type since member functions
    * are always chosen over extension methods. But this evidence allows
    * `NonEmpty[C]` to be used where `C` is expected.
    *
    * Note: DummyImplicit is used to give this lower priority than the other
    * extension methods.
    */
  given subtype[T, C <: Iterable[T]](using DummyImplicit): (NonEmpty[C] <:< C) = summon[C =:= C]
}
