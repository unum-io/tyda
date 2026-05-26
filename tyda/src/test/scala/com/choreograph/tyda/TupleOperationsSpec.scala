package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import TupleOperations.-

class TupleOperationsSpec extends AnyFunSuite {

  test("- removes single occurrence of type from tuple") {
    summon[(Int, String, Boolean) - String =:= (Int, Boolean)]
    summon[(String, Int, Boolean) - String =:= (Int, Boolean)]
    summon[(Int, Boolean, String) - String =:= (Int, Boolean)]
  }

  test("- removes multiple occurrences of type from tuple") {
    summon[(String, Int, String, Boolean, String) - String =:= (Int, Boolean)]
    summon[(Int, Int, Boolean) - Int =:= Boolean *: EmptyTuple]
    summon[(String, String, String) - String =:= EmptyTuple]
  }

  test("- does nothing when type is not found in tuple") {
    summon[(Int, Boolean) - String =:= (Int, Boolean)]
    summon[(String, Double) - Int =:= (String, Double)]
  }

  test("- handles empty tuple") {
    summon[EmptyTuple - String =:= EmptyTuple]
    summon[EmptyTuple - Int =:= EmptyTuple]
  }

  test("- handles single element tuple") {
    summon[Int *: EmptyTuple - Int =:= EmptyTuple]
    summon[String *: EmptyTuple - Int =:= String *: EmptyTuple]
  }

  test("- preserves order of remaining elements") {
    summon[(Int, String, Boolean, Double, String) - String =:= (Int, Boolean, Double)]
    summon[(String, Int, String, Boolean, String, Double) - String =:= (Int, Boolean, Double)]
  }

  test("- handles nested tuple types") {
    summon[((Int, String), Boolean, String) - String =:= ((Int, String), Boolean)]
    summon[(Int, (String, Boolean)) - String =:= (Int, (String, Boolean))]
  }

  test("- removes all occurrences including duplicates at the end") {
    summon[(Int, Boolean, String, String) - String =:= (Int, Boolean)]
    summon[(String, String, Int, Boolean) - String =:= (Int, Boolean)]
  }

  test("- handles complex type combinations") {
    summon[(Int, Long, String, Boolean, Int, Double, String) - String =:= (Int, Long, Boolean, Int, Double)]
    summon[(Option[Int], String, Either[Int, String], String) - String =:= (Option[Int], Either[Int, String])]
  }

  test("- removes only exact type matches") {
    summon[(Int, String, Option[String], Boolean) - String =:= (Int, Option[String], Boolean)]
    summon[(List[Int], Int, Vector[Int]) - Int =:= (List[Int], Vector[Int])]
  }
}
