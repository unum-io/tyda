package com.choreograph.tyda

import java.lang.Double.doubleToLongBits

import scala.collection.mutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.Random

import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary.Shrinkable
import com.choreograph.tyda.TupleOperations.TupleN
import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

object ArbitrarySpec {
  private final case class MyProduct(name: String, age: Int) derives Arbitrary
  private enum MyEnum derives Arbitrary {
    case Singleton
    case Prod(p: MyProduct)
    case Other(i: Int, l: Long)
  }

  private enum SimpleEnum derives Arbitrary {
    case A, B, C
  }

  private sealed trait MySealedTrait derives Arbitrary
  private object MySealedTrait {
    case object Singleton extends MySealedTrait
    final case class Prod(p: MyProduct) extends MySealedTrait
    final case class Other(i: Int, l: Long) extends MySealedTrait
  }
  private final case class Recursive(r: Option[Recursive]) derives Arbitrary

  opaque type OpaqueInt = Int
  object OpaqueInt {
    given Arbitrary[OpaqueInt] = Arbitrary.int
  }

  given float: Equality[Float] =
    new Equality[Float] {
      override def areEqual(a: Float, b: Any): Boolean =
        b match {
          case b: Float => Ord[Float].equiv(a, b)
          case _ => false
        }
    }
  given double: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        b match {
          case b: Double => Ord[Double].equiv(a, b)
          case _ => false
        }
    }
}

class ArbitrarySpec extends AnyFunSuite {
  import ArbitrarySpec.{MyEnum, SimpleEnum, MyProduct, MySealedTrait, Recursive, given}

  def checkStabililty[T: Arbitrary: Equality](name: String) = {
    val seed = Random.nextLong()
    assert(Arbitrary[T](Random(seed)) === Arbitrary[T](Random(seed)), s"$name produced different values")
  }

  def checkNotConst[T: Arbitrary: Equality](name: String) = {
    val maxTries = 40
    val first = Arbitrary[T]()
    val iter = (0 until maxTries).iterator.filter(_ => first !== Arbitrary[T]())

    assert(iter.hasNext, s"$name failed to produce different value in $maxTries")
  }

  def test[T: Arbitrary: TypeName: Equality]: Unit = {
    val name = s"Arbitrary[${TypeName.name[T]}]"
    test(s"$name stable when seeded") { checkStabililty[T](name) }

    test(s"$name produce different values") { checkNotConst[T](name) }
  }

  test[Byte]
  test[Int]
  test[Short]
  test[Long]
  test[Float]
  test[Double]
  test[Boolean]
  test[String]
  test[Option[Int]]
  test[(Int, Long)]
  test[ArraySeq[Int]]
  test[Iterable[Boolean]]
  test[Set[Long]]
  test[Seq[(Int, Long)]]
  test[Map[Int, Int]]
  test[Map[MyProduct, String]]
  test[MyProduct]
  test[MyEnum]
  test[SimpleEnum]
  test[MySealedTrait]
  test[Recursive]
  {
    given Arbitrary[(Int, Int, Int, Int)] = Arbitrary.tupleN[Int, 4]
    test[(Int, Int, Int, Int)]
  }

  test("Arbitrary collections can be empty") {
    val samples = (0 until 1000).iterator.map(_ => Arbitrary[List[Int]]())
    assert(samples.exists(_.isEmpty), "Should sometimes generate empty collections")
  }

  test("Long should be biased towards edge cases") {
    def samples = (0 until 1000).iterator.map(_ => Arbitrary[Long]())
    assert(samples.exists(_ == Long.MinValue), "Should generate Long.MinValue")
    assert(samples.exists(_ == Long.MaxValue), "Should generate Long.MaxValue")
    assert(samples.exists(_ == 0L), "Should generate 0L")
  }

  test("Double should be biased towards edge cases") {
    def samples = (0 until 1000).iterator.map(_ => Arbitrary[Double]())
    assert(samples.exists(_ == Double.MinValue), "Should generate Double.MinValue")
    assert(samples.exists(_ == Double.MaxValue), "Should generate Double.MaxValue")
    assert(samples.exists(_ == 0.0), "Should generate 0.0")
    val negativeZeroBits = doubleToLongBits(-0.0)
    assert(samples.exists(v => doubleToLongBits(v) == negativeZeroBits), "Should generate -0.0")
    assert(samples.exists(_ == Double.PositiveInfinity), "Should generate Infinity")
    assert(samples.exists(_ == Double.NegativeInfinity), "Should generate -Infinity")
  }

  test("Float should generate full range of values") {
    def genFiniteGreater(v: Float): Iterator[Float] = {
      val filtered = Arbitrary.float.filter(d => d.isFinite && d < Float.MaxValue && d >= v)
      Iterator.continually(filtered())
    }
    assert(genFiniteGreater(1.0).nextOption().isDefined, "Should be able to generate values >= 1.0")
    assert(
      genFiniteGreater(Long.MaxValue.toFloat).nextOption().isDefined,
      "Should be able to generate values >= Long.MaxValue"
    )
  }

  test("Double should generate full range of values") {
    def genFiniteGreater(v: Double): Iterator[Double] = {
      val filtered = Arbitrary.double.filter(d => d.isFinite && d < Double.MaxValue && d >= v)
      Iterator.continually(filtered())
    }
    assert(genFiniteGreater(1.0).nextOption().isDefined, "Should be able to generate values >= 1.0")
    assert(
      genFiniteGreater(Long.MaxValue.toDouble).nextOption().isDefined,
      "Should be able to generate values >= Long.MaxValue"
    )
  }

  test("support building new Arbitrary instances using filter") {
    val name: String = "Arbitrary[Int].filter(_.nonEmpty)"
    given arb: Arbitrary[Seq[Int]] = Arbitrary.iterable[Int, Seq[Int]].filter(_.nonEmpty)
    checkStabililty[Seq[Int]](name)
    checkNotConst[Seq[Int]](name)
    assert(arb().nonEmpty, "Should not generate non-empty sequences")
  }

  test("support building new Arbitrary instances using map") {
    val name: String = "Arbitrary[Int].map(_.toString)"
    given arb: Arbitrary[String] = Arbitrary[Int].map(_.toString)
    checkStabililty[String](name)
    checkNotConst[String](name)
    arb().toInt // Should not throw
  }

  test("filter unsatifisfied predicate should throw") {
    given arb: Arbitrary[Int] = Arbitrary[Int].filter(_ => false)
    assertThrows[RuntimeException](arb())
  }

  test("support for comprehension builders") {
    type Range = (min: Int, max: Int)
    val gen: Arbitrary[Range] = for {
      min <- Arbitrary.int
      max <- Arbitrary.int if max >= min
    } yield (min, max)

    (0 to 100).foreach { _ =>
      val r = gen()
      assert(r.max >= r.min)
    }
  }

  test("distinctN supports opaque types") {
    val arb = Arbitrary.distinctN[ArbitrarySpec.OpaqueInt, 3]
    val (a, b, c) = arb()
    assert(a != b, "Values should be distinct")
    assert(a != c, "Values should be distinct")
    assert(b != c, "Values should be distinct")
  }

  test("distinctN with negative n is not allowed") {
    assertCompileTimeError(
      """Arbitrary.distinctN[Int, -2]""",
      "Cannot prove that (-2 : Int) >= (0 : Int) =:= (true : Boolean)"
    )
  }

  test("distinctN throws if distinct elements cannot be generated") {
    // We only have 3 elements in SimpleEnum, so trying to generate 4 distinct should fail
    val arb = Arbitrary.distinctN[SimpleEnum, 4]
    assertThrows[RuntimeException](arb())
  }

  test("distinctN 0 produces empty tuple") {
    val arb = Arbitrary.distinctN[Int, 0]
    val sample = arb()
    assert(sample === EmptyTuple)
  }

  test("distinctN can be called multiple times without throwing") {
    val arb = Arbitrary.distinctN[SimpleEnum, 3]
    val a = arb()
    val b = arb()
    assert(a != b || a == b) // just to use the values
  }

  test("seqN supports opaque types") {
    val arb = Arbitrary.seqN[ArbitrarySpec.OpaqueInt](2)
    val sample = arb()
    assert(sample.size == 2)
  }

  test("seqN is supported for arbitrary n") {
    Iterator
      .continually(Arbitrary.between(-1000, 1000)())
      .take(10)
      .foreach { n =>
        val arb = Arbitrary.seqN[Int](n)
        val sample = arb()
        assert(sample.size == n.max(0))
      }
  }

  test("tupleN supports opaque types") {
    val arb = Arbitrary.tupleN[ArbitrarySpec.OpaqueInt, 2]
    val (s1, s2) = arb() // Check that values can be unpacked
    assert(s1 != s2 || s1 == s2) // just to use the values
  }

  test("tupleN with negative n is not allowed") {
    assertCompileTimeError(
      """Arbitrary.tupleN[Int, -1]""",
      "Cannot prove that (-1 : Int) >= (0 : Int) =:= (true : Boolean)"
    )
  }

  def testLongBetween(min: Long, max: Long): Unit = {
    test(s"Arbitrary.between($min, $max) for Long complete") {
      val arb = Arbitrary.between(min, max)
      val generated = Iterator.continually(arb()).take(1000).distinct.toSet
      assert(generated == (min until max).toSet, s"Did not generate full range between $min and $max")
    }
  }
  testLongBetween(-10, 10)
  testLongBetween(1, 10)
  testLongBetween(0, 10)
  testLongBetween(-5, -1)
  testLongBetween(0, 1)

  def testShrinking[T: Arbitrary: TypeName](
      predicate: T => Boolean,
      expectedMin: T,
      name: String = ""
  ): Unit = {
    def generateFailingExample: Shrinkable[T] =
      val rng = new Random()
      Iterator
        .continually(Arbitrary[T].shrinkable(rng))
        .take(10000)
        .find(predicate.compose(_.value))
        .getOrElse(
          throw new RuntimeException(
            "Failed to generate a value that satisfies the predicate for shrinking test"
          )
        )

    test(s"Arbitrary[${TypeName.name[T]}] shrinking $name") {
      val failing = generateFailingExample
      val shrunk = failing.minimize(predicate)
      assert(
        shrunk == expectedMin,
        s"${TypeName.name[T]} did not shrink to expected minimum. Got: $shrunk, expected: $expectedMin"
      )
    }
    test(s"Arbitrary[${TypeName.name[T]}] shrinking $name random path") {
      val value = generateFailingExample
      // The check here is that this should not throw
      value.minimize(_ => Random.nextBoolean())
    }
  }

  testShrinking[Int](_ >= 10, 10)
  testShrinking[Long](_ <= -100L, -100L)
  testShrinking[Float](_ => true, 0)
  testShrinking[Double](_ => true, 0)
  testShrinking[BigInt](_ => true, BigInt(0))
  {
    given Arbitrary[Int] = Arbitrary.int.map(_ * 2)
    testShrinking[Int](_ >= 5, 6, "even numbers")
  }
  {
    given Arbitrary[Int] = Arbitrary.between(-100, -9)
    testShrinking[Int](_ => true, -10, "in range")
    testShrinking[Int](_.abs >= 20, -20, "in range with abs > 20")
  }
  {
    given Arbitrary[Long] = Arbitrary.between(-100, Long.MaxValue)
    testShrinking[Long](_ => true, 0, "in range wide range")
  }
  {
    given Arbitrary[Long] = Arbitrary.between(10, 100)
    testShrinking[Long](_ => true, 10, "in range")
  }
  {
    given Arbitrary[Float] = Arbitrary.between(0.1, 1)
    testShrinking[Float](_ => true, 0.1, "in range")
  }
  {
    given Arbitrary[Double] = Arbitrary.between(-100.0, -10.0)
    testShrinking[Double](_ => true, -10.0, "in range")
  }
  {
    /* This tests that flatMap shrinking works correctly and that we correctly generate v2 independently of v1
     * when there is no dependency */
    type Pair = (v1: Long, v2: Long)
    given Arbitrary[Pair] =
      for {
        v1 <- Arbitrary.int
        v2 <- Arbitrary.between(0, 11)
      } yield (v1, v2)
    testShrinking[Pair](r => r.v1 >= 10 && r.v2 >= 10, (10, 10), "shrink flatMap of indpendent values")
  }
  testShrinking[String](_ => true, "", "to empty string")
  testShrinking[String](_.exists(_ == ' '), " ", "shrink string size")
  testShrinking[Seq[Int]](_ => true, Seq.empty[Int], "empty for true predicate")
  testShrinking[Seq[Int]](_.exists(_ >= 10), Seq(10), "shrink size and elements")
  testShrinking[Set[Int]](
    s => { s.exists(_ <= -10) && s.exists(_ >= 10) },
    Set(-10, 10),
    "shrink size and elements"
  )
  testShrinking[Map[Int, Int]](
    m => { m.exists { case (k, v) => k >= 10 && v >= 10 } },
    Map(10 -> 10),
    "shrink size and elements"
  )
  {
    given Arbitrary[Seq[Int]] = Arbitrary.seqN(5)
    testShrinking[Seq[Int]](_.forall(_ >= 10), Seq(10, 10, 10, 10, 10), "fixed size seq")
  }
  {
    given Arbitrary[Seq[Int]] = Arbitrary.iterable[Int, Seq[Int]].filter(_.size >= 3)
    testShrinking[Seq[Int]](_.forall(_ >= 10), Seq(10, 10, 10), "filtered seq with min size")
  }
  testShrinking[Seq[Seq[Int]]](_ => true, Seq.empty[Seq[Int]], "to empty seq of seq")
  testShrinking[Option[Int]](_.forall(_ >= 10), None, "to None")
  testShrinking[Option[Int]](_.exists(_ >= 10), Some(10), "shink value inside")
  testShrinking[MySealedTrait](
    {
      case MySealedTrait.Prod(p) if p.age >= 10 => true
      case _ => false
    },
    MySealedTrait.Prod(MyProduct("", 10)),
    "sealed trait containing product"
  )
  testShrinking[MySealedTrait](_ => true, MySealedTrait.Singleton, "sealed trait to singleton")
  testShrinking[MyProduct](p => p.age >= 10, MyProduct("", 10), "product shrinking fields")
  {
    given Arbitrary[Int] = Arbitrary.int.map(_.abs)
    given Arbitrary[TupleN[Int, 5]] =
      Arbitrary
        .distinctN[Int, 5]
        .map { case (x0, x1, x2, x3, x4) =>
          // TYPE SAFETY: The array has exactly 5 elements
          Tuple.fromArray(Array(x0, x1, x2, x3, x4).sorted).asInstanceOf[TupleN[Int, 5]]
        }
    testShrinking[TupleN[Int, 5]](_ => true, (0, 1, 2, 3, 4), "distinctN tuple")
  }
}
