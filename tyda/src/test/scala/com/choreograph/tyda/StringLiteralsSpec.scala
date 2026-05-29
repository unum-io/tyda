package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

class StringLiteralsSpec extends AnyFunSuite {

  def testStringLiterals[T <: Tuple: StringLiterals: TypeName](expected: IndexedSeq[String]) = {
    test(TypeName.name[T]) { assert(StringLiterals[T] == expected) }
  }

  testStringLiterals[EmptyTuple](IndexedSeq.empty)
  testStringLiterals[Tuple1["hello"]](IndexedSeq("hello"))
  testStringLiterals[("foo", "bar", "baz")](IndexedSeq("foo", "bar", "baz"))

  test("literal non-string fails to compile") {
    assertCompileTimeError(
      """StringLiterals[("foo", "bar", 1)]""",
      "tuple has at least one element which is not a string"
    )
  }

  test("non-literal string fails to compile") {
    assertCompileTimeError(
      """StringLiterals[("foo", "bar", String)]""",
      "tuple has at least one element which is not a constant type"
    )
  }

  test("non-literal non-string fails to compile") {
    assertCompileTimeError(
      """StringLiterals[("foo", "bar", Int)]""",
      "tuple has at least one element which is not a constant type"
    )
  }
}
