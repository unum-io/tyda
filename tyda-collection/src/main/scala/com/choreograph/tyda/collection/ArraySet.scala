package com.choreograph.tyda.collection

import scala.annotation.nowarn
import scala.annotation.targetName
import scala.collection.Factory
import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import scala.collection.View
import scala.collection.immutable.AbstractSeq
import scala.collection.immutable.AbstractSet
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.IndexedSeqOps
import scala.collection.immutable.StrictOptimizedSeqOps
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.Builder
import scala.language.implicitConversions
import scala.reflect.ClassTag

import com.choreograph.tyda.collection.Itertools.dedup

/** Immutable, order-stable set backed by an ArraySeq.
  *
  * The ordering is part of the type so that two ArraySets can be compared for
  * equality using normal [[Seq]] equality. This property is useful when used
  * with a query engine like Spark that does not natively support Sets. But with
  * the order being part of the type, it is safe to let Spark treat them as
  * regular arrays.
  *
  * Since it is not possible to implement both [[Set]] and [[Seq]] in Scala due
  * to both extending `Function1` with different types, this class only
  * implements the [[Seq]] interface to signal that the elements are ordered.
  * The collection provides a toSet method that will convert to a `Set[A]`
  * without copying the underlying collection.
  *
  * @tparam A
  *   the type of the elements in the set
  * @tparam O
  *   the ordering of the elements in the set, must be a singleton type
  */
final class ArraySet[A: ClassTag, O <: Ordering[A] & Singleton] private (private val elems: ArraySeq[A])(using
    O
) extends AbstractSeq[A],
      IndexedSeq[A],
      /* We can not use ArraySet as type argument here. For the second argument since we don't have an
       * ordering and for the third argument that would violate the Liskov substitution principle with the
       * provided reverse implementation.
       * Since we only use IndexedSeq over a more specific type we can override the methods that do not break
       * the set property. The filter has been overridden to show this, but there are many more that would be
       * safe to override */
      IndexedSeqOps[A, IndexedSeq, IndexedSeq[A]],
      StrictOptimizedSeqOps[A, IndexedSeq, IndexedSeq[A]],
      Serializable {

  /** Check if an element is contained in the set.
    *
    * Overloaded to allow using binary search to check for membership. Becase
    * [[Seq]] is covariant the contains defined there would not allow for using
    * the ordering to check for membership.
    */
  @targetName("ArraySet.contains")
  def contains(elem: A): Boolean = ArraySet.contains(elems, elem)

  /** @inheritdoc
    */
  override def apply(i: Int): A = elems(i)

  /** @inheritdoc
    */
  override def length: Int = elems.length

  /** Merge this ArraySet with another ArraySet.
    */
  def union(set: ArraySet[A, O]): ArraySet[A, O] =
    if isEmpty then set else if set.isEmpty then this else new ArraySet(ArraySet.merge(elems, set.elems))

  /** Returns a Set[A] view of this ArraySet.
    *
    * This is useful for interoperability with APIs that expect a Set without
    * copying the values.
    *
    * Overloaded to allow reuse of the underlying ArraySeq without copying.
    * Would not be possible with the normal toSeq due to [[Iterable]] being
    * covariant in its element type.
    */
  @targetName("ArraySet.toSet")
  def toSet: Set[A] = ArraySet.SetView(elems)

  /** @inheritdoc
    */
  override def filter(p: A => Boolean): ArraySet[A, O] =
    // Since we use ArraySeq in fromSpecific this will not copy the elements an extra time.
    new ArraySet(super.filter(p).to(ArraySeq))

  /** @inheritdoc
    */
  override def newSpecificBuilder: Builder[A, IndexedSeq[A]] = ArraySeq.newBuilder[A]

  /** @inheritdoc
    */
  override def fromSpecific(coll: IterableOnce[A]): IndexedSeq[A] = ArraySeq.from(coll)

  /** @inheritdoc
    */
  override def empty: ArraySet[A, O] = ArraySet.empty[A, O]
}

object ArraySet {
  // Convenience type alias to allow ArraySet specifing only the ordering.
  type By[O <: Ordering[?] & Singleton] = O match { case Ordering[t] => ArraySet[t, O] }

  /** Construct an ArraySet from an IterableOnce.
    */
  def from[A: ClassTag, O <: Ordering[A] & Singleton](elements: IterableOnce[A])(using O): ArraySet[A, O] =
    elements match {
      case it: Seq[A] => sortState(it) match {
          case SortState.SortedSet => new ArraySet(it.to(ArraySeq))
          case SortState.SortedWithDuplicates(duplicates) =>
            val builder = ArrayBuilder.make[A]
            builder.sizeHint(it.size - duplicates)
            builder.addAll(it.iterator.dedup)
            new ArraySet(ArraySeq.unsafeWrapArray(builder.result()))
          case SortState.Unsorted => fromUnsorted(it)
        }
      case _ => fromUnsorted(elements)
    }

  private def fromUnsorted[A: ClassTag, O <: Ordering[A] & Singleton](
      elements: IterableOnce[A]
  )(using O): ArraySet[A, O] = {
    val array = elements.iterator.toArray
    new ArraySet(array.sortInPlace().iterator.dedup.to(ArraySeq))
  }

  /** Create an empty ArraySet.
    */
  def empty[A: ClassTag, O <: Ordering[A] & Singleton](implicit ordering: O): ArraySet[A, O] =
    new ArraySet(ArraySeq.empty[A])

  /** Factory method to create an ArraySet from a variable number of elements.
    */
  def apply[A: ClassTag, O <: Ordering[A] & Singleton](elements: A*)(using O): ArraySet[A, O] = from(elements)

  /** Create a new builder for an ArraySet.
    */
  def newBuilder[A: ClassTag, O <: Ordering[A] & Singleton](using O): Builder[A, ArraySet[A, O]] =
    ArraySeq.newBuilder[A].mapResult(ArraySet.from[A, O](_))

  /** Similar to [[scala.collection.IterableFactory.fill]]
    */
  def fill[A: ClassTag, O <: Ordering[A] & Singleton](n: Int)(elem: => A)(using O): ArraySet[A, O] =
    from(new View.Fill[A](n)(elem))

  /** Similar to [[scala.collection.IterableFactory.tabulate]]
    */
  def tabulate[A: ClassTag, O <: Ordering[A] & Singleton](n: Int)(f: Int => A)(using O): ArraySet[A, O] =
    from(new View.Tabulate[A](n)(f))

  /** Similar to [[scala.collection.IterableFactory.iterate]]
    */
  def iterate[A: ClassTag, O <: Ordering[A] & Singleton](start: A, len: Int)(f: A => A)(using
      O
  ): ArraySet[A, O] = from(new View.Iterate[A](start, len)(f))

  /** Similar to [[scala.collection.IterableFactory.unfold]]
    */
  def unfold[A: ClassTag, O <: Ordering[A] & Singleton, S](init: S)(f: S => Option[(A, S)])(using
      O
  ): ArraySet[A, O] = from(new View.Unfold[A, S](init)(f))

  /* Defined as a scala2 implicit conversion for it to be used implicitly. Used to support syntax like
   * `iterator.to(ArraySet)`. */
  implicit def toFactory[A: ClassTag, O <: Ordering[A] & Singleton](t: this.type)(using
      O
  ): Factory[A, ArraySet[A, O]] = factory

  given factory[A: ClassTag, O <: Ordering[A] & Singleton](using O): Factory[A, ArraySet[A, O]] =
    ArraySetFactory[A, O]

  private final class ArraySetFactory[A: ClassTag, O <: Ordering[A] & Singleton](using O)
      extends Factory[A, ArraySet[A, O]], Serializable {
    def fromSpecific(coll: IterableOnce[A]): ArraySet[A, O] = ArraySet.from(coll)
    def newBuilder: Builder[A, ArraySet[A, O]] = ArraySet.newBuilder
  }

  @nowarn("id=E198") // Bug reported here https://github.com/scala/scala3/issues/23694
  private final class SetView[A: ClassTag, O <: Ordering[A] & Singleton](elems: ArraySeq[A])(using O)
      extends AbstractSet[A] {
    override def iterator: Iterator[A] = elems.iterator
    override def excl(elem: A): SetView[A, O] =
      elems.search(elem) match {
        case Found(idx) => new SetView(elems.patch(idx, Nil, 1))
        case InsertionPoint(_) => this
      }
    override def incl(elem: A): SetView[A, O] =
      elems.search(elem) match {
        case Found(_) => this
        case InsertionPoint(idx) => new SetView(elems.patch(idx, ArraySeq(elem), 0))
      }
    override def contains(elem: A): Boolean = ArraySet.contains(elems, elem)

    override def toSeq: Seq[A] = elems
    override def toIndexedSeq: IndexedSeq[A] = elems
  }

  private def contains[A: Ordering](elems: ArraySeq[A], elem: A): Boolean =
    elems.search(elem) match {
      case Found(_) => true
      case InsertionPoint(_) => false
    }

  private enum SortState {
    case SortedSet
    case SortedWithDuplicates(duplicates: Int)
    case Unsorted
  }

  private def sortState[A: Ordering as ord](elems: Seq[A]): SortState = {
    var duplicates = 0
    val it = elems.iterator
    if !it.hasNext then return SortState.SortedSet
    var last = it.next()
    while it.hasNext do {
      val next = it.next()
      ord.compare(last, next) match {
        case cmp if cmp > 0 => return SortState.Unsorted
        case 0 => duplicates += 1
        case _ =>
      }
      last = next
    }
    if duplicates != 0 then SortState.SortedWithDuplicates(duplicates) else SortState.SortedSet
  }

  private def merge[A: {ClassTag, Ordering as ord}](left: ArraySeq[A], right: ArraySeq[A]): ArraySeq[A] = {
    val merged = ArrayBuilder.make[A]
    merged.sizeHint(left.length + right.length)

    var leftIdx = 0
    var rightIdx = 0
    while (leftIdx < left.length && rightIdx < right.length) {
      val leftElem = left(leftIdx)
      val rightElem = right(rightIdx)
      ord.compare(leftElem, rightElem) match {
        case 0 =>
          // Elements are equal, add only one of them
          merged += leftElem
          leftIdx += 1
          rightIdx += 1
        case cmp if cmp < 0 =>
          merged += leftElem
          leftIdx += 1
        case _ =>
          merged += rightElem
          rightIdx += 1
      }
    }
    while (leftIdx < left.length) {
      merged += left(leftIdx)
      leftIdx += 1
    }
    while (rightIdx < right.length) {
      merged += right(rightIdx)
      rightIdx += 1
    }
    ArraySeq.unsafeWrapArray(merged.result())
  }
}
