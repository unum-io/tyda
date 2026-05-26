package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object SelectorSpec {
  final case class Person(name: String, age: Int) derives Arbitrary

  opaque type Size1 = Int
  object Size1 {
    def apply(value: Int): Size1 = value
    extension (s: Size1) { def toInt: Int = s }
    given Arbitrary[Size1] = Arbitrary.int
  }
  opaque type Size2 = Int
  object Size2 {
    def apply(value: Int): Size2 = value
    extension (s: Size2) { def toInt: Int = s }
    given Arbitrary[Size2] = Arbitrary.int
  }

  trait SelectSize[T] {
    def apply(t: T): Int
  }
}

class SelectorSpec extends AnyFunSuite {
  import SelectorSpec.*

  test("Selector should exists and work for case classes") {
    val person = Arbitrary[Person]()
    assert(Selector[Person, String](person) == person.name)
    assert(Selector[Person, Int](person) == person.age)
  }

  test("Should exists and work for tuples") {
    val tuple = Arbitrary[(String, Int)]()
    assert(Selector[(String, Int), String](tuple) == tuple._1)
    assert(Selector[(String, Int), Int](tuple) == tuple._2)
  }

  test("Should exists and work for named tuples") {
    type T = (name: String, age: Int, height: Double)
    val value = Arbitrary[T]()
    assert(Selector[T, String](value) == value.name)
    assert(Selector[T, Int](value) == value.age)
  }

  test("Should support selecting a opaque type") {
    type T = (Size2, Size1)
    val value = Arbitrary[T]()
    assert(Selector[T, Size1](value) == value._2)
  }
}
