package com.choreograph.tyda.collection

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.collection.Itertools.dedup

class ItertoolsSpec extends AnyFunSuite {
  test("dedup should deduplicate elements") {
    val input = Iterator(1, 1, 2, 2, 3, 3, 4, 4)
    assert(input.dedup.toSeq == Seq(1, 2, 3, 4))
  }

  test("dedup should handle empty iterator") {
    val input = Iterator.empty[Int]
    val deduped = input.dedup
    assert(!deduped.hasNext)
    assert(deduped.toSeq == Seq())
  }

  test("dedup should handle single element") {
    val input = Iterator(42)
    val deduped = input.dedup
    assert(deduped.toSeq == Seq(42))
  }

  test("dedup should be lazy in construction") {
    val it = new Iterator[Int] {
      def hasNext: Boolean = true
      def next(): Int = fail("This iterator should not be accessed eagerly")
    }
    val _ = it.dedup // Should not eagerly read from the iterator
  }

  test("dedup should should not eagerly deduplicate") {
    val it = Iterator(1) ++ Iterator.continually(2)
    val deduped = it.dedup
    assert(deduped.hasNext)
    assert(deduped.next() == 1)
    assert(deduped.hasNext)
    assert(deduped.next() == 2)
  }

  test("blindly calling next should work") {
    val it = Iterator(1, 2, 3, 4, 4, 5)
    val deduped = it.dedup
    assert(deduped.next() == 1)
    assert(deduped.next() == 2)
    assert(deduped.next() == 3)
    assert(deduped.next() == 4)
    assert(deduped.next() == 5)
    assert(!deduped.hasNext)
  }

  test("Should use Equiv for deduplication") {
    val input = Iterator(Float.NaN, Float.NaN, 1.0f, 1.0f, 2.0f)
    val deduped = input.dedup
    assert(deduped.next().isNaN)
    assert(deduped.next() == 1.0f)
    assert(deduped.next() == 2.0f)
    assert(!deduped.hasNext)
  }
}
