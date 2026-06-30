package com.choreograph.tyda

import scala.annotation.tailrec
import scala.collection.Factory
import scala.collection.immutable.Queue
import scala.compiletime.ops.int.>=
import scala.reflect.ClassTag
import scala.util.Random

import shapeless3.deriving.Ap
import shapeless3.deriving.K0
import shapeless3.deriving.MapF
import shapeless3.deriving.Pure

sealed trait Arbitrary[T] {
  final def apply(r: Random): T = shrinkable(r).value
  final def apply(): T = apply(Random)
  def shrinkable(r: Random): Arbitrary.Shrinkable[T]

  /** Create a new `Arbitrary[U]` by applying `f` to the result of this
    * `Arbitrary`.
    *
    * Note: For shriking to work, shriking the input `T` should also produce
    * smaller `U` values.
    */
  def map[U](f: T => U): Arbitrary[U] = Arbitrary.Map(this, f)

  /** Create a new `Arbitrary[U]` from a generated value of `T`. */
  def flatMap[U](f: T => Arbitrary[U]): Arbitrary[U] = Arbitrary.FlatMap(this, f)

  /** Same as `filter` but used by for-comprehensions. */
  def withFilter(p: T => Boolean): Arbitrary[T] = filter(p)

  /** Create a new `Arbitrary[T]` that only generates values that satisfy the
    * predicate `p`.
    *
    * Note: That this method will only try a finite number of times to generate
    * a value that satisfies the predicate and throws an exception if it fails.
    * So one should make sure that the predicate is not too restrictive.
    */
  def filter(p: T => Boolean): Arbitrary[T] = Arbitrary.Filter(this, p)
}

object Arbitrary {
  def apply[T](using i: Arbitrary[T]): Arbitrary[T] = i

  private val negativeZeroFloatBits = java.lang.Float.floatToRawIntBits(-0.0f)
  private val negativeZeroDoubleBits = java.lang.Double.doubleToRawLongBits(-0.0d)

  // TODO: Should be removed once https://github.com/scala/scala/pull/11083 is released
  def hackFloat: Arbitrary[Float] =
    float.filter(f => java.lang.Float.floatToRawIntBits(f) != negativeZeroFloatBits)
  def hackDouble: Arbitrary[Double] =
    double.filter(f => java.lang.Double.doubleToRawLongBits(f) != negativeZeroDoubleBits)

  // Create an new `Arbitrary[T]` that will generate values between `min` (inclusive) and `max` (exclusive).
  def between[T: Between](min: T, max: T): Arbitrary[T] = Between[T](min, max)

  // Create an Arbitrary instance for `T` that will generate one of the provided values.
  def oneOf[T](v0: T, vn: T*): Arbitrary[T] = {
    val values = (v0 +: vn).toIndexedSeq
    between(0, values.size).map(i => values(i))
  }

  /** Create an `Arbitrary[Array[Byte]]` that generates arrays of length `n`. */
  def bytes(n: Int): Arbitrary[Array[Byte]] =
    FixedSizeIterableArbitrary[Byte, Array[Byte]](Arbitrary[Byte], n, summon)

  // Create an `Arbitrary[Seq[T]]` that generates sequences of length `n`.
  def seqN[T: Arbitrary](n: Int): Arbitrary[Seq[T]] =
    FixedSizeIterableArbitrary[T, Seq[T]](Arbitrary[T], n, summon)

  // Create an arbitrary instance that generates tuples of length `N` with elements of type `T`.
  def tupleN[T: ClassTag: Arbitrary as arb, N <: Int: ValueOf](using
      N >= 0 =:= true
  ): Arbitrary[TupleOperations.TupleN[T, N]] =
    FixedSizeIterableArbitrary[T, Array[T]](arb, valueOf[N], summon).map(
      // TYPE SAFETY: We know that the array has length `n` because of FixedSizeIterableArbitrary
      Tuple.fromArray(_).asInstanceOf[TupleOperations.TupleN[T, N]]
    )

  /* Create an arbitrary instance that generates tuples of length `N` where all elements are distinct.
   * If elements cannot be generated after `maxTriesPerElement` attempts an exception is thrown. */
  def distinctN[T: ClassTag: Arbitrary as arb, N <: Int: ValueOf](using
      N >= 0 =:= true
  ): Arbitrary[TupleOperations.TupleN[T, N]] =
    @tailrec
    def make(current: Arbitrary[Set[T]], size: Int): Arbitrary[Set[T]] =
      if size == valueOf[N] then return current
      val next = current.flatMap(set => Arbitrary[T].filter(!set.contains(_)).map(set + _))
      make(next, size + 1)
    make(Arbitrary.oneOf(Set.empty[T]), 0).map { set =>
      // TYPE SAFETY: We know that the set has size `n` because of the construction
      Tuple.fromArray(set.toArray).asInstanceOf[TupleOperations.TupleN[T, N]]
    }

  // Create an new `Arbitrary[T]` that will use given the `Arbitrary[T]` with equal probability.
  def combine[T](arb0: Arbitrary[T], arb1: Arbitrary[T], arbs: Arbitrary[T]*): Arbitrary[T] = {
    val arbitraries = (arb0 +: arb1 +: arbs).toIndexedSeq
    between(0, arbitraries.size).flatMap(i => arbitraries(i))
  }

  given byte: Arbitrary[Byte] = integral(_.nextInt().toByte)
  given short: Arbitrary[Short] = integral(_.nextInt().toShort)
  given int: Arbitrary[Int] = integral(_.nextInt())
  given long: Arbitrary[Long] = integral(_.nextLong())
  given float: Arbitrary[Float] = floating(r => java.lang.Float.intBitsToFloat(r.nextInt()))
  given double: Arbitrary[Double] = floating(r => java.lang.Double.longBitsToDouble(r.nextLong()))
  given boolean: Arbitrary[Boolean] = oneOf(v0 = true, vn = false)
  given string: Arbitrary[String] = combine(mkString(ascii), StringArbitrary(5))

  given bigInt: Arbitrary[BigInt] =
    for {
      sign <- oneOf(-1, 1)
      size <- between(0, 20)
      bytes <- bytes(size)
    } yield BigInt(sign, bytes.toArray)

  given iterable[T: Arbitrary, C <: Iterable[T]](using factory: Factory[T, C]): Arbitrary[C] =
    IterableArbitrary[T, C](Arbitrary[T], 5, factory)

  given sum[T](using inst: K0.CoproductInstances[Arbitrary, T]): Arbitrary[T] = SumArbitrary(inst)

  given product[T](using inst: K0.ProductInstances[Arbitrary, T]): Arbitrary[T] = ProductArbitrary(inst)

  given valueClass[T: ValueClassMirror as m](using inner: Arbitrary[m.MirroredElemType]): Arbitrary[T] =
    inner.map(m.fromValue)

  inline def derived[T](using gen: K0.Generic[T]): Arbitrary[T] = gen.derive(product, sum)

  trait Between[T] extends Function2[T, T, Arbitrary[T]]
  object Between {
    def apply[T: Between as b]: Between[T] = b

    given int: Between[Int] = (min, max) => long(min.toLong, max.toLong).map(_.toInt)
    given long: Between[Long] with {
      def apply(min: Long, max: Long): Arbitrary[Long] = {
        assert(min < max, s"Invalid range: [$min, $max)")
        val difference = max - min
        // Difference is larger than Long.MaxValue so filter is efficient enough
        if difference < 0 then return Arbitrary.long.filter(v => v >= min && v < max)

        if min <= 0L && max > 0L then IntegralArbitrary(_.between(min, max), negateInShrink = false)
        else if min.abs < max.abs then
          IntegralArbitrary(r => r.nextLong(difference), negateInShrink = false).map(_ + min)
        else IntegralArbitrary(r => r.nextLong(difference), negateInShrink = false).map(d => max - d - 1)
      }
    }
    given float: Between[Float] = (min, max) => double(min.toDouble, max.toDouble).map(_.toFloat)
    given double: Between[Double] =
      (min, max) =>
        val origin = if min <= 0.0 && max >= 0.0 then 0.0 else if max.abs < min.abs then max else min
        FloatingArbitrary(_.between(min, max), origin)
  }

  // Arbitrary instances for primitive types that is biased towards edge cases.
  private def integral[T: NumericLimits: Integral](genUniform: Random => T): Arbitrary[T] = {
    val edgeCases = Vector(Numeric[T].zero, NumericLimits[T].min, NumericLimits[T].max)
    val gen =
      (r: Random) => if r.nextDouble() < 0.7 then genUniform(r) else edgeCases(r.nextInt(edgeCases.length))
    IntegralArbitrary(gen, negateInShrink = true)
  }

  // Arbitrary instances for floating point types that is biased towards edge cases.
  private def floating[T: FloatingLimits: Fractional](genUniform: Random => T): Arbitrary[T] = {
    val numeric = Numeric[T]
    import numeric.mkNumericOps
    val edgeCases = Vector(
      NumericLimits[T].min,
      NumericLimits[T].max,
      Numeric[T].zero,
      -Numeric[T].zero,
      FloatingLimits[T].infinity,
      -FloatingLimits[T].infinity,
      FloatingLimits[T].minPositiveValue,
      -FloatingLimits[T].minPositiveValue,
      FloatingLimits[T].nan
    )
    val gen =
      (r: Random) => if r.nextDouble() < 0.7 then genUniform(r) else edgeCases(r.nextInt(edgeCases.length))
    FloatingArbitrary(gen, Numeric[T].zero)
  }

  private val asciiChars = (32 to 127).map(_.toChar).toArray
  private val ascii: Arbitrary[Char] = between(0, asciiChars.length).map(asciiChars)
  private def mkString(arb: Arbitrary[Char]): Arbitrary[String] =
    IterableArbitrary[Char, String](arb, 5, summon)

  private final case class IntegralArbitrary[T: Integral as integral](
      gen: Random => T,
      negateInShrink: Boolean
  ) extends Arbitrary[T] {
    import integral.{equiv, fromInt, mkNumericOps, zero}

    private def shrink(t: T): LazyList[T] = {
      val half = t / fromInt(2)
      if equiv(t, zero) then LazyList.empty
      else if negateInShrink then half #:: -half #:: shrink(half).map(h => t - half + h)
      else half #:: shrink(half).map(h => t - half + h)
    }

    override def shrinkable(r: Random): Shrinkable[T] = Shrinkable[T](gen(r), shrink, zero)
  }

  // TODO: More complex shrinking for floating point numbers
  private final case class FloatingArbitrary[T](gen: Random => T, origin: T) extends Arbitrary[T] {
    override def shrinkable(r: Random): Shrinkable[T] = {
      val value = gen(r)
      Shrinkable[T](value, _ => LazyList.empty, origin)
    }
  }

  private type ChunkRange = (from: Int, size: Int)
  private def genRanges(size: Int): LazyList[ChunkRange] = {
    if size == 0 then return LazyList.empty
    if size == 1 then return LazyList((0, size))
    def subranges(r: ChunkRange): Seq[ChunkRange] =
      if r.size <= 1 then Seq.empty
      else {
        val halfSize = r.size / 2
        Seq((r.from, halfSize), (r.from + halfSize, r.size - halfSize))
      }
    LazyList.unfold(Queue.from[ChunkRange](subranges((0, size))))(queue =>
      queue.headOption match {
        case None => None
        case Some(r) => Some(r, queue.tail.enqueueAll(subranges(r)))
      }
    )
  }

  private def shrinkElements[T](value: Seq[Shrinkable[T]]): LazyList[Shrinkable[Seq[T]]] =
    if value.isEmpty then LazyList.empty
    else {
      val h = value.head
      val t = value.tail
      val shrinkHead = h
        .shrinks
        .map { shrunkHead =>
          val newValue = shrunkHead +: t
          Shrinkable(newValue.map(_.value), shrinkElements(newValue))
        }
      shrinkHead #::: shrinkElements(t).map(shrunkTail => shrunkTail.map(h.value +: _))
    }

  private def shrinkSeq[T](value: Seq[Shrinkable[T]]): LazyList[Shrinkable[Seq[T]]] =
    shrinkSize(value) ++ shrinkElements(value)
  private def shrinkSize[T](value: Seq[Shrinkable[T]]): LazyList[Shrinkable[Seq[T]]] =
    genRanges(value.size).map(range =>
      Shrinkable(
        value.patch(range.from, Seq.empty, range.size).map(_.value),
        shrinkSeq(value.patch(range.from, Seq.empty, range.size))
      )
    )

  private final case class IterableArbitrary[T, C](arb: Arbitrary[T], maxSize: Int, factory: Factory[T, C])
      extends Arbitrary[C] {
    override def shrinkable(r: Random): Shrinkable[C] = {
      val size = r.nextInt(maxSize + 1)
      val elements = Vector.fill(size)(arb.shrinkable(r))
      val valueSeq = elements.map(_.value)
      Shrinkable(valueSeq, Shrinkable.unshrinkable(Seq.empty) #:: shrinkSeq(elements)).map(
        factory.fromSpecific
      )
    }
  }

  private final case class FixedSizeIterableArbitrary[T, C](
      arb: Arbitrary[T],
      size: Int,
      factory: Factory[T, C]
  ) extends Arbitrary[C] {
    override def shrinkable(r: Random): Shrinkable[C] = {
      val elements = Vector.fill(size)(arb.shrinkable(r))
      val valueSeq = elements.map(_.value)
      Shrinkable(valueSeq, shrinkElements(elements)).map(factory.fromSpecific)
    }
  }

  private final case class StringArbitrary(maxSize: Int) extends Arbitrary[String] {
    private def shrink(value: String): LazyList[String] =
      if value.size == 0 then LazyList.empty
      else genRanges(value.length).map(range => value.patch(range.from, "", range.size))
    override def shrinkable(r: Random): Shrinkable[String] =
      Shrinkable(r.nextString(r.nextInt(maxSize)), shrink)
  }

  private final case class SumArbitrary[T](inst: K0.CoproductInstances[Arbitrary, T]) extends Arbitrary[T] {
    private val singletons = (0 until inst.arity)
      .map(i =>
        inst.inject(i)([t <: T] =>
          _ match {
            case singleton @ ProductArbitrary(inst) if inst.arity == 0 =>
              // Singletons do not depend on Random, so passing global Random is fine
              Some(singleton.shrinkable(Random))
            case _ => None
          }
        )
      )
      .flatten
      .to(LazyList)
    override def shrinkable(r: Random): Shrinkable[T] = {
      val index = r.nextInt(inst.arity)
      val shrink = inst.inject(index)([t <: T] => _.shrinkable(r))
      Shrinkable(shrink.value, singletons #::: shrink.shrinks)
    }
  }

  private final case class ProductArbitrary[T](inst: K0.ProductInstances[Arbitrary, T]) extends Arbitrary[T] {
    override def shrinkable(r: Random): Shrinkable[T] =
      inst.constructA[Shrinkable]([t] => _.shrinkable(r))(Shrinkable.pure, Shrinkable.map, Shrinkable.ap)
  }

  private final case class Filter[T](arb: Arbitrary[T], p: T => Boolean) extends Arbitrary[T] {
    override def shrinkable(r: Random): Shrinkable[T] = {
      val maxTries = 1000
      Iterator
        .continually(arb.shrinkable(r))
        .take(maxTries)
        .find(s => p(s.value))
        .map(s => s.filter(p))
        .getOrElse {
          throw new RuntimeException(
            s"Failed to generate a value that satisfies the predicate in ${maxTries} tries"
          )
        }
    }
  }

  private final case class Map[T, U](arb: Arbitrary[T], f: T => U) extends Arbitrary[U] {
    override def shrinkable(r: Random): Shrinkable[U] = arb.shrinkable(r).map(f)
  }

  private final case class FlatMap[T, U](arb: Arbitrary[T], f: T => Arbitrary[U]) extends Arbitrary[U] {
    override def shrinkable(r: Random): Shrinkable[U] = {
      /* Since we do not have a immutable Random, we need to use a fixed seed inside the flatMap otherwise
       * shrinking will not work. This is because the flatMap is lazy and if we pass a mutable instance the
       * generated values will depend on what was explored earlier during shrinking. */
      val seed = r.nextLong()
      arb.shrinkable(r).flatMap(t => f(t).shrinkable(new Random(seed)))
    }
  }

  /** A value of type `T` together with generator of shrink candidates.
    *
    * Note: `genShrinkCandidates` is a Function instead of a LazyList directly
    * to avoid keeping the potentially large LazyList in memory after it been
    * used to minimize.
    */
  final class Shrinkable[+T] private (val value: T, genShrinkCandidates: () => LazyList[Shrinkable[T]]) {
    def map[U](f: T => U): Shrinkable[U] = Shrinkable(f(value), shrinks.map(_.map(f)))

    def flatMap[U](f: T => Shrinkable[U]): Shrinkable[U] = {
      val first = f(value)
      Shrinkable(first.value, shrinks.map(s => s.flatMap(f)) ++ first.shrinks)
    }

    def filter(pred: T => Boolean): Shrinkable[T] =
      Shrinkable(value, shrinks.filter(s => pred(s.value)).map(s => s.filter(pred)))

    def minimize(pred: T => Boolean): T = {
      @tailrec
      def loop(current: Shrinkable[T]): Shrinkable[T] = {
        val smaller = current.shrinks.find(s => pred(s.value))
        smaller match {
          case Some(s) => loop(s)
          case None => current
        }
      }
      loop(this).value
    }

    def shrinks: LazyList[Shrinkable[T]] = genShrinkCandidates()
  }

  object Shrinkable {
    def apply[T](value: T, genShrink: => LazyList[Shrinkable[T]]): Shrinkable[T] =
      new Shrinkable(value, () => genShrink)

    def apply[T](value: T, shrinkSingle: T => LazyList[T], earlyCases: T*): Shrinkable[T] = {
      def shrinkRec(v: T): LazyList[Shrinkable[T]] = shrinkSingle(v).map(v => Shrinkable(v, shrinkRec(v)))
      Shrinkable(value, earlyCases.to(LazyList).map(v => Shrinkable(v, shrinkRec(v))) ++ shrinkRec(value))
    }

    def unshrinkable[T](value: T): Shrinkable[T] = Shrinkable(value, LazyList.empty)

    extension [T](seq: Seq[Shrinkable[T]]) {
      def minimize(pred: Seq[T] => Boolean): Seq[T] = {
        val shrinkable = Shrinkable(seq.map(_.value), shrinkSeq(seq))
        shrinkable.minimize(pred)
      }
    }

    val pure: Pure[Shrinkable] = [t] => unshrinkable(_)
    val map: MapF[Shrinkable] = [a, b] => _.map(_)
    val ap: Ap[Shrinkable] = [a, b] => (ff, fa) => ff.flatMap(fa.map)
  }
}
