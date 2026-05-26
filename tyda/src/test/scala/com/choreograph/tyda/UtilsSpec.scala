package com.choreograph.tyda

import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.boolean.||
import scala.compiletime.ops.int.*
import scala.compiletime.ops.int.+
import scala.compiletime.ops.int.<=

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

class UtilsSpec extends AnyFunSuite {

  test("staticAssert should compile when condition is true") { staticAssert[true, "This should compile"] }

  test("staticAssert should compile for complex conditions") {
    staticAssert[1 + 1 <= 2, "This should compile"]
    staticAssert[2 * 2 <= 4 && true, "This should compile"]
    staticAssert[1 + 1 <= 2 || false, "This should compile"]
  }

  test("staticAssert should not compile when condition is false") {
    assertCompileTimeError(
      """staticAssert[false, "This should not compile"]""",
      "Type argument",
      "does not conform to upper bound"
    )
  }

  test("unreachable should throw an AssertionError") {
    assertThrows[AssertionError] { unreachable("This is unreachable") }
  }

  test("staticCast should cast value to the specified type") {
    inline def list = List(1, 2, 3)
    inline def seq: Seq[Int] = Seq(1, 2, 3)
    assert(staticCast[List[Int]](list) == list)
    assert(staticCast[Seq[Int]](list) == list)
    assert(staticCast[Seq[Int]](seq) == list)
  }

  test("staticCast reject invalid casts") {
    val expected = "cannot reduce inline match"
    inline def seq: Seq[Int] = Seq(1, 2, 3)
    assertCompileTimeError("staticCast[Int](1L)", expected)
    assertCompileTimeError("staticCast[List[Int]](seq)", expected)
  }
}
