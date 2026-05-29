package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

object UniqueIndexOfSpec {
  opaque type MyInt = Int
}

class UniqueIndexOfSpec extends AnyFunSuite {
  import UniqueIndexOfSpec.MyInt

  test("work for tuple") { assert(summon[UniqueIndexOf[(String, Int, Double), Int]] == 1) }

  test("work with opaque types") { assert(summon[UniqueIndexOf[(String, Int, MyInt, Float), MyInt]] == 2) }

  test("does not exists when not unique") {
    assertCompileTimeError(
      "summon[UniqueIndexOf[(String, Int, Double, Int), Int]]",
      "Type Int must occur exactly once in Tuple4[String, Int, Double, Int] occured 2 times."
    )
  }

  test("does not exists when not present") {
    assertCompileTimeError(
      "summon[UniqueIndexOf[(String, Int, Double), MyInt]]",
      "Type MyInt must occur exactly once in Tuple3[String, Int, Double] occured 0 times."
    )
  }
}
