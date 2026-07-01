package com.choreograph.tyda

import scala.NamedTuple.NamedTuple

import org.scalatest.funsuite.AnyFunSuite

object MapperSpec {

  trait ToStr[T] extends ExprDepFn1[T] {
    type Out = String
  }

  given ToStr[Int] = _.cast[String]
  given ToStr[Long] = _.cast[String]
  given ToStr[Short] = _.cast[String]
  given ToStr[String] = e => e
  final case class Person(name: String, age: Int) derives Codec
}

class MapperSpec extends AnyFunSuite {
  import MapperSpec.*

  test("Mapper maps a named tuple preserving field names") {
    val mapper = summon[Mapper[ToStr, (name: String, age: Int)]]
    summon[mapper.Out =:= (name: String, age: String)]
    val input = Expr.lift(ExprNode.Reference[(name: String, age: Int)]())
    val result: Expr[(name: String, age: String)] = mapper(input)
    assert(result.codec == Codec[(name: String, age: String)])
  }

  test("Mapper maps a case class producing a named tuple") {
    val mapper = summon[Mapper[ToStr, Person]]
    summon[mapper.Out =:= (name: String, age: String)]
    val input = Expr.lift(ExprNode.Reference[Person]())
    val result: Expr[(name: String, age: String)] = mapper(input)
    assert(result.codec == Codec[(name: String, age: String)])
  }

  test("Mapper maps an unnamed tuple as named tuple") {
    val mapper = summon[Mapper[ToStr, (Int, Long, Short)]]
    summon[mapper.Out =:= NamedTuple[("_1", "_2", "_3"), (String, String, String)]]
    val input = Expr.lift(ExprNode.Reference[(Int, Long, Short)]())
    val result = mapper(input)
    assert(result.codec == Codec[(String, String, String)])
  }
}
