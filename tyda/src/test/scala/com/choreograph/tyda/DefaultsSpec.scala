package com.choreograph.tyda

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

object DefaultsSpec {
  final case class Person(name: String, age: Int = 0)
  final case class Generic[T](value: T, label: String = "a")
}

class DefaultsSpec extends AnyFunSuite {
  import DefaultsSpec.*

  test("NamedTuple defaults should all be None") {
    val namedTupleDefaults = summon[Defaults[(name: String, age: Int)]]
    assert(namedTupleDefaults.defaults == (None, None))
  }

  test("Default values should be derived for case classes") {
    val personDefaults = summon[Defaults[Person]]
    assert(personDefaults.defaults == (None, Some(0)))
  }

  test("support case classes using generics") {
    val genericDefaults = summon[Defaults[Generic[String]]]
    assert(genericDefaults.defaults == (None, Some("a")))
  }
}
