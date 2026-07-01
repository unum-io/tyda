package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object TupleMapperSpec {

  /** A simple ExprDepFn1 that casts any numeric type to String. */
  trait ToStr[T] extends ExprDepFn1[T] {
    type Out = String
  }

  given ToStr[Int] with {
    def apply(e: Expr[Int]): Expr[String] = e.cast[String]
  }

  given ToStr[Long] with {
    def apply(e: Expr[Long]): Expr[String] = e.cast[String]
  }

  given ToStr[Short] with {
    def apply(e: Expr[Short]): Expr[String] = e.cast[String]
  }
}

class TupleMapperSpec extends AnyFunSuite {
  import TupleMapperSpec.*

  test("TupleMapper maps EmptyTuple") {
    val mapper = summon[TupleMapper[ToStr, EmptyTuple]]
    val input: Expr[EmptyTuple] = Expr.lift(ExprNode.Reference[EmptyTuple]())
    val result = mapper(input)
    // Verify the compiler knows the full resulting type
    summon[mapper.Out =:= EmptyTuple]
    assert(result.codec == Codec[EmptyTuple])
  }

  test("TupleMapper maps a single-element tuple") {
    val mapper = summon[TupleMapper[ToStr, Int *: EmptyTuple]]
    summon[mapper.Out =:= (String *: EmptyTuple)]
    val input: Expr[Int *: EmptyTuple] = Expr.lift(ExprNode.Reference[Int *: EmptyTuple]())
    val result: Expr[String *: EmptyTuple] = mapper(input)
    assert(result.codec == Codec[String *: EmptyTuple])
  }

  test("TupleMapper maps a multi-element tuple") {
    val mapper = summon[TupleMapper[ToStr, (Int, Long, Short)]]
    summon[mapper.Out =:= (String, String, String)]
    val input = Expr.lift(ExprNode.Reference[(Int, Long, Short)]())
    val result: Expr[(String, String, String)] = mapper(input)
    assert(result.codec == Codec[(String, String, String)])
  }
}
