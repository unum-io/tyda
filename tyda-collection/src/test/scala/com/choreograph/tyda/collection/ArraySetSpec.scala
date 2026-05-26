package com.choreograph.tyda.collection

import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

import scala.collection.Factory
import scala.collection.SortedSet
import scala.collection.immutable.ArraySeq
import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

class ArraySetSpec extends AnyFunSuite {
  test("be constructable from values with inferred type") {
    val set = ArraySet(1, 2, 3)
    summon[set.type <:< ArraySet[Int, Ordering.Int.type]]
  }

  test("SortedSet.By can be used as a shorthand") {
    val _: ArraySet.By[Ordering.Int.type] = ArraySet(1, 2, 3)
  }

  private def checkEqual[C](s1: C, s2: C) = {
    assert(s1 == s2)
    assert(s2 == s1)
  }
  private def checkNotEqual[C](s1: C, s2: C) = {
    assert(s1 != s2)
    assert(s2 != s1)
  }

  test("== with ArraySet") {
    val set1 = ArraySet(1, 2, 3)
    val set2 = ArraySet(3, 2, 1)
    checkEqual(set1, set2)
    checkEqual(set1, set1)
    checkNotEqual(set1, ArraySet.empty[Int, Ordering.Int.type])
    checkNotEqual(set1, ArraySet(1))
    checkNotEqual(set1, ArraySet(1, 3))
    checkNotEqual(set1, ArraySet(1, 2, 3, 4))
  }

  test("ArraySet.== different orderings") {
    val ord1 = Ordering.Int
    val ord2 = Ordering.Int.reverse
    val set1 = ArraySet(1, 2, 3)(using summon, ord1)
    val set2 = ArraySet(3, 2, 1)(using summon, ord2)
    val set3 = ArraySet(1, 2, 3)
    checkEqual(set1, set3)
    checkEqual(set1, set1)
    checkNotEqual(set1, set2)
    checkNotEqual(set2, set3)
  }

  test("== with other Seqs") {
    val set = ArraySet(1, 2, 3)
    checkEqual(set, Seq(1, 2, 3))
    checkEqual(set, List(1, 2, 3))
    checkEqual(set, ArraySeq(1, 2, 3))
    checkNotEqual(set, Seq(1, 2))
    checkNotEqual(set, Seq(1, 2, 3, 4))
    checkNotEqual(set, Seq(3, 2, 1))
    checkNotEqual(set, ArraySeq(3, 2, 1))
  }

  test("SetView.== with other Sets") {
    val set = ArraySet(1, 2, 3).toSet
    checkEqual(set, set)
    checkEqual(set, Set(1, 2, 3))
    checkEqual(set, Set(3, 2, 1))
    checkEqual(set, SortedSet(3, 2, 1))
    checkEqual(set, SortedSet(3, 2, 1)(using Ordering.Int.reverse))
    checkNotEqual(set, set - 2)
    checkNotEqual(set, Set(1, 2))
    checkNotEqual(set, Set(1, 2, 3, 4))
    checkNotEqual(set, Set(4, 5))
    checkNotEqual(set, SortedSet(4, 5))
    checkNotEqual(set, SortedSet(1, 2, 3, 4))
  }

  test("ArraySet.hashCode") {
    val values = Seq.fill(10)(Random.nextInt(10))
    val set1 = ArraySet(values*)
    val set2 = ArraySet(Random.shuffle(values)*)
    assert(set1.hashCode == set2.hashCode)
    assert(set1.hashCode != ArraySet.empty[Int, Ordering.Int.type].hashCode)
  }

  test("SetView.hashCode") {
    val values = Seq.fill(10)(Random.nextInt(10))
    val set1 = ArraySet(values*).toSet
    val set2 = ArraySet(Random.shuffle(values)*).toSet
    assert(set1.hashCode == set2.hashCode)
    assert(set1.hashCode != Set.empty[Int].hashCode)
  }

  test("remove duplicates during construction") {
    val values = Seq.fill(10)(Random.nextInt(10))
    val set = ArraySet(values*)
    assert(set == values.distinct.sorted)
  }

  test("construct from sorted values") {
    val values = Seq.fill(10)(Random.nextInt(10))
    val set = ArraySet((values ++ values).sorted*)
    assert(set == values.distinct.sorted)
  }

  test("support Iterator.to(SortedSet) syntax") {
    val set = Iterator(3, 1, 2).to(ArraySet)
    summon[set.type <:< ArraySet[Int, Ordering.Int.type]]
  }

  test("contain elements after construction") {
    val values = Seq.fill(10)(Random.nextInt())
    val set = ArraySet.from(values)
    values.foreach(v => assert(set.contains(v)))
  }

  test("Have a Factory") {
    val factory = summon[Factory[Int, ArraySet.By[Ordering.Int.type]]]
    val values = Seq.fill(100)(Random.nextInt(100))
    val builder = factory.newBuilder
    values.foreach(builder.addOne)
    assert(builder.result().toSeq == values.distinct.sorted)
  }

  test("Have a reusable builder") {
    val builder = ArraySet.newBuilder[Int, Ordering.Int.type]
    builder.addOne(1)
    assert(builder.result() == ArraySet(1))
    builder.clear()
    assert(builder.result() == ArraySet.empty[Int, Ordering.Int.type])
    builder.clear()
    builder.addOne(2)
    assert(builder.result() == ArraySet(2))
  }

  test("merge using union") {
    (0 until 10).foreach { _ =>
      val values1 = Seq.fill(Random.nextInt(100))(Random.nextInt(100))
      val values2 = Seq.fill(Random.nextInt(100))(Random.nextInt(100))
      val expected = (values1 ++ values2).distinct.sorted
      val set1 = values1.to(ArraySet)
      val set2 = values2.to(ArraySet)
      val merged: ArraySet.By[Ordering.Int.type] = set1.union(set2)
      assert(merged.toSeq == expected)
    }
  }

  test("merge with empty set using union") {
    val values = Seq.fill(10)(Random.nextInt())
    val set = values.to(ArraySet)
    val empty: ArraySet.By[Ordering.Int.type] = ArraySet.empty
    assert(set.union(empty) == set)
    assert(empty.union(set) == set)
  }

  test("add new elements to SetView with +") {
    val set = Seq.fill(10)(Random.nextInt()).to(ArraySet).toSet
    val newElem = Random.nextInt()
    val newSet = set + newElem
    assert(newSet.contains(newElem))
    assert(newSet.size >= set.size)
    assert(newSet + newElem eq newSet)
  }

  test("remove elements from SetView with -") {
    val set = Seq.fill(10)(Random.nextInt()).to(ArraySet).toSet
    val existing = set.head
    val newSet = set - existing
    assert(!newSet.contains(existing))
    assert(newSet.size == set.size - 1)
    assert(newSet - existing eq newSet)
  }

  test("SortedSet.filter returns ArraySet") {
    val values = Set.fill(10)(Random.nextInt()).to(ArraySet)
    val set: ArraySet[Int, Ordering.Int.type] = values.filter(_ > 0)
    val exptected = values.filter(_ > 0).toSet
    assert(set.toSet == exptected)
  }

  test("ArraySet.map returns IndexedSeq") {
    val values = Set.fill(10)(Random.nextInt())
    val set: ArraySet[Int, Ordering.Int.type] = values.to(ArraySet)
    val mapped: IndexedSeq[Int] = set.map(_ * 2)
    val expected = values.iterator.distinct.toSeq.sorted.map(_ * 2)
    assert(mapped == expected)
  }

  test("ArraySet.empty") {
    val set = (0 to 10).to(ArraySet)
    val empty: ArraySet.By[Ordering.Int.type] = set.empty
    assert(empty.isEmpty)
    assert(empty.size == 0)
  }

  def testToMethod[C <: Iterable[Int]](method: String, f: ArraySet[Int, Ordering.Int.type] => C): Unit = {
    test(s"have efficient $method method that does not copy data") {
      val set = Seq.fill(10)(Random.nextInt()).to(ArraySet)
      val res1 = f(set)
      val res2 = f(set)
      assert(res1 eq res2)
    }
  }

  testToMethod("toSeq", _.toSeq)
  testToMethod("toIndexedSeq", _.toIndexedSeq)
  // These are a proxy checking that doing toSeq do not copy the underlying buffer.
  testToMethod("toSet.toSeq", _.toSet.toSeq)
  testToMethod("toSet.toIndexedSeq", _.toSet.toIndexedSeq)

  test("create empty set") {
    val empty = ArraySet.empty[Int, Ordering.Int.type]
    assert(empty.isEmpty)
    assert(empty.size == 0)
  }

  test("create from already sorted ArraySeq does not copy data") {
    val values = ArraySeq(1, 2, 3, 4, 5)
    val set = values.to(ArraySet)
    assert(set.toSet.toSeq eq values)

    val smallValues = ArraySeq(1)
    val smallSet = smallValues.to(ArraySet)
    assert(smallSet.toSet.toSeq eq smallValues)
  }

  test("fill with repeated elements") {
    val n = Random.nextInt(10) + 1
    val elem = Random.nextInt()
    val set = ArraySet.fill(n)(elem)
    assert(set.size == 1)
    assert(set.contains(elem))
  }

  test("tabulate with function") {
    val n = Random.nextInt(20) + 1
    val set = ArraySet.tabulate(n)(i => i * 2)
    val expected = (0 until n).map(_ * 2).toSet
    assert(set.toSet == expected)
  }

  test("iterate with transformation") {
    val start = Random.nextInt()
    val len = Random.nextInt(10) + 1
    val set = ArraySet.iterate(start, len)(_ + 1)
    val expected = (0 until len).map(start + _).toSet
    assert(set.toSet == expected)
  }

  test("unfold with state") {
    val set = ArraySet.unfold(0)(s => Option.when(s < 5)((s * s, s + 1)))
    val expected = Set(0, 1, 4, 9, 16)
    assert(set.toSet == expected)
  }

  test("should be serializable") {
    val set = Seq.fill(Random.nextInt(10))(Random.nextInt(100)).to(ArraySet)
    val bos = ByteArrayOutputStream()
    val oos = ObjectOutputStream(bos)
    oos.writeObject(set)
    oos.close()
    val bytes = bos.toByteArray
    val ois = java.io.ObjectInputStream(java.io.ByteArrayInputStream(bytes))
    // TYPE SAFETY: We just serialized the object
    val deserialized = ois.readObject().asInstanceOf[ArraySet[Int, Ordering.Int.type]]
    assert(deserialized == set)
  }

  // Checks that upcasting ArratSet to Seq will follow the Liskov substitution principle
  /* this is check by calling the same method on a ArraySet and an ArraySeq and checking that the result is as
   * expected. */
  private def checkSeqBehavior[R](name: String, f: Seq[Int] => R): Unit = {
    test(s"Method $name should behave as other Seq implementations") {
      val set = Seq.fill(Random.nextInt(10))(Random.nextInt(100)).to(ArraySet)
      val seq = set.to(ArraySeq)
      assert(f(set) == f(seq), "Treating ArraySet as Seq observable behavior difference")
    }
  }

  checkSeqBehavior("prepended(0)", _.prepended(0))
  checkSeqBehavior("take(2)", _.take(2))
  checkSeqBehavior("takeRight(2)", _.takeRight(2))
  checkSeqBehavior("drop(1)", _.drop(1))
  checkSeqBehavior("dropRight(1)", _.dropRight(1))
  checkSeqBehavior("map(_ + 1)", _.map(_ + 1))
  checkSeqBehavior("reverse", _.reverse)
  checkSeqBehavior("slice(1, 3)", _.slice(1, 3))
  checkSeqBehavior("headOption", _.headOption)
  checkSeqBehavior("knownSize", _.knownSize)
  checkSeqBehavior("search(5)", _.search(5))
  checkSeqBehavior("groupBy(1)", _.groupBy(_ % 2))
}
