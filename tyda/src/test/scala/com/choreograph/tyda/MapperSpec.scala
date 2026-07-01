package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object MapperSpec {

  /** A simple ExprDepFn1 that casts any numeric type to String. */
  trait ToStr[T] extends ExprDepFn1[T] {
    type Out = String
  }

  given ToStr[Int] = _.cast[String]
  given ToStr[Short] = _.cast[String]
  given ToStr[Long] = _.cast[String]
}

class MapperSpec extends AnyFunSuite {
  import MapperSpec.*

  test("Mapper maps EmptyTuple") {
    val mapper = summon[Mapper[ToStr, EmptyTuple]]
    val input: Expr[EmptyTuple] = Expr.lift(ExprNode.Reference[EmptyTuple]())
    val result = mapper(input)
    // Verify the compiler knows the full resulting type
    summon[mapper.Out =:= EmptyTuple]
    assert(result.codec == Codec[EmptyTuple])
  }

  test("Mapper maps a single-element tuple") {
    val mapper = summon[Mapper[ToStr, Int *: EmptyTuple]]
    summon[mapper.Out =:= (String *: EmptyTuple)]
    val input: Expr[Int *: EmptyTuple] = Expr.lift(ExprNode.Reference[Int *: EmptyTuple]())
    val result: Expr[String *: EmptyTuple] = mapper(input)
    assert(result.codec == Codec[String *: EmptyTuple])
  }

  test("Mapper maps a multi-element tuple") {
    val mapper = summon[Mapper[ToStr, (Int, Long, Short)]]
    summon[mapper.Out =:= (String, String, String)]
    val input = Expr.lift(ExprNode.Reference[(Int, Long, Short)]())
    val result: Expr[(String, String, String)] = mapper(input)
    assert(result.codec == Codec[(String, String, String)])
  }
}
