package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object MergerSpec {
  private final case class Left(a: Int, b: String) derives Arbitrary
  private final case class Right(b: Long, c: Short) derives Arbitrary
  private final case class NoOverlap(x: Int, y: String) derives Arbitrary
}

class MergerSpec extends AnyFunSuite {
  import MergerSpec.*

  test("merge two case classes with overlapping field names uses right value") {
    val merger = Merger[Left, Right]
    val left = Arbitrary[Left]()
    val right = Arbitrary[Right]()

    val result = merger(left, right)
    val expected = (a = left.a, b = right.b, c = right.c)

    assert(result == expected)
  }

  test("result type uses right type for overlapping fields") {
    val merger = Merger[Left, Right]
    summon[merger.Out =:= (a: Int, b: Long, c: Short)]
  }

  test("merge two case classes with no overlapping fields") {
    val merger = Merger[Left, NoOverlap]
    val left = Arbitrary[Left]()
    val noOverlap = Arbitrary[NoOverlap]()

    val result = merger(left, noOverlap)
    val expected = (a = left.a, b = left.b, x = noOverlap.x, y = noOverlap.y)

    assert(result == expected)
  }

  test("merge named tuples uses right value for overlapping fields") {
    val merger = Merger[(a: Int, b: String), (b: Long, c: Short)]
    val left = Arbitrary[(a: Int, b: String)]()
    val right = Arbitrary[(b: Long, c: Short)]()

    val result = merger(left, right)
    val expected = (a = left.a, b = right.b, c = right.c)

    assert(result == expected)
  }

  test("merge with empty right product preserves left") {
    val merger = Merger[Left, EmptyTuple]
    val left = Arbitrary[Left]()
    val result = merger(left, EmptyTuple)
    assert(result == (a = left.a, b = left.b))
  }

  test("merge with empty left product uses right") {
    val merger = Merger[EmptyTuple, Right]
    val right = Arbitrary[Right]()
    val result = merger(EmptyTuple, right)
    assert(result == (a = right.b, b = right.c))
  }

  test("and be fast to compile for big products") {
    val merger = Merger[BigNamedTuple, BigNamedTuple]
    val big = Arbitrary[BigNamedTuple]()
    val big2 = Arbitrary[BigNamedTuple]()
    val result = merger(big, big2)
    assert(result == big2)
  }

  test("support and be fast to compile for big products with partial overlap") {
    val merger = Merger[BigNamedTuple, (i150: Int, extra: String)]
    val big = Arbitrary[BigNamedTuple]()
    val right = Arbitrary[(i150: Int, extra: String)]()
    val result = merger(big, right)
    assert(result.i0 == big.i0)
    assert(result.i150 == right.i150)
    assert(result.extra == right.extra)
  }
}
