package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object ValueClassMirrorSpec {
  private final case class MyInt(i: Int) extends AnyVal
  private final case class MyT[T](t: T) extends AnyVal
  private final case class HK[F[_], T](v: F[T]) extends AnyVal
  private final case class NotAValueClass(i: Int)
}

class ValueClassMirrorSpec extends AnyFunSuite {
  import ValueClassMirrorSpec.*

  private val myIntMirror = summon[ValueClassMirror[MyInt]]

  test("working fromValue") { assert(myIntMirror.fromValue(42) == MyInt(42)) }

  test("typesafe fromValue") { assertDoesNotCompile("myIntMirror.fromValue(42L)") }

  test("ValueClassMirror should support fromProductTyped") {
    assert(myIntMirror.fromProductTyped(42 *: EmptyTuple) == MyInt(42))
  }

  test("support value class with generics") {
    val m = ValueClassMirror.derive[MyT[String]]
    assert(m.fromValue("abc") == MyT("abc"))
  }

  test("support higher kinded types") {
    type Id[X] = X
    val m = ValueClassMirror.derive[HK[Id, Int]]
    assert(m.fromValue(0) == HK[Id, Int](0))
  }

  test("support higher kinded types nested value class") {
    val m = ValueClassMirror.derive[HK[MyT, Int]]
    assert(m.fromValue(MyT(0)) == HK[MyT, Int](MyT(0)))
  }

  test("not available for non-value classes") {
    assertDoesNotCompile("summon[ValueClassMirror[NotAValueClass]]")
  }
}
