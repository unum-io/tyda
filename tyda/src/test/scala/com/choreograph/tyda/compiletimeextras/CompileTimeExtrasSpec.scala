package com.choreograph.tyda.compiletimeextras

import scala.deriving.Mirror

import org.scalatest.funsuite.AnyFunSuite

object CompileTimeExtrasSpec {
  enum A {
    case B
    case C(c: Int)
  }
}

class CompileTimeExtrasSpec extends AnyFunSuite {
  import CompileTimeExtrasSpec.*
  private inline val companionPrefix = "com.choreograph.tyda.compiletimeextras.CompileTimeExtrasSpec"
  private val aMirror = summon[Mirror.SumOf[A]]

  test("generate the short type name") {
    assert(typeNameShort[Int] == "Int")
    assert(typeNameShort[String] == "String")
    assert(typeNameShort[A.B.type] == "B")
    assert(typeNameShort[A] == "A")
    assert(typeNameShort[A.C] == "C")
  }

  test("generate a comma separate short names") {
    assert(typeNameShortAll[EmptyTuple] == "")
    assert(typeNameShortAll[Int *: EmptyTuple] == "Int")
    assert(typeNameShortAll[aMirror.MirroredElemTypes] == "B, C")
  }

  private inline def matchShortInt = inline typeNameShort[Int] match { case "Int" => () }
  test("typeNameShort should be inline reducible") { matchShortInt }

  private inline def matchShortIntAndString =
    inline typeNameAll[Int *: String *: EmptyTuple] match {
      case "Int, String" => ()
      case _ => pending // Will be fixed by https://github.com/scala/scala3/pull/24431
    }
  test("typeNameShortAll should be inline reducible") { matchShortIntAndString }

  test("typeNameShort be usable in compiletime error messages") {
    assertCompileTimeError("""scala.compiletime.error("type is " + typeNameShort[Int])""", "type is Int")
  }

  test("typeNameShortAll should be usable in compiletime error messages") {
    assertCompileTimeError(
      """scala.compiletime.error("types are " + typeNameShortAll[Int *: String *: EmptyTuple])""",
      "types are Int, String"
    )
  }

  test("generate the type name") {
    assert(typeName[Int] == "scala.Int")
    assert(typeName[String] == "scala.Predef.String")
    assert(typeName[A] == s"$companionPrefix.A")
    assert(typeName[A.B.type] == s"$companionPrefix.A.B.type")
    assert(typeName[A.C] == s"$companionPrefix.A.C")
  }

  test("generate a comma separate names") {
    assert(typeNameAll[EmptyTuple] == "")
    assert(typeNameAll[Int *: EmptyTuple] == "scala.Int")
    assert(typeNameAll[aMirror.MirroredElemTypes] == s"$companionPrefix.A.B.type, $companionPrefix.A.C")
  }

  private inline def matchInt = inline typeName[Int] match { case "scala.Int" => () }
  test("typeName should be inline reducible") { matchInt }

  private inline def matchIntAndString =
    inline typeNameAll[Int *: String *: EmptyTuple] match {
      case "scala.Int, scala.Predef.String" => ()
      case _ => pending // Will be fixed by https://github.com/scala/scala3/pull/24431
    }
  test("typeNameAll should be inline reducible") { matchIntAndString }

  test("typeName be usable in compiletime error messages") {
    assertCompileTimeError("""scala.compiletime.error("type is " + typeName[Int])""", "type is scala.Int")
  }

  test("typeNameAll should be usable in compiletime error messages") {
    assertCompileTimeError(
      """scala.compiletime.error("types are " + typeNameAll[Int *: String *: EmptyTuple])""",
      "types are scala.Int, java.lang.String"
    )
  }

  test("constToString works ") {
    assert(constToString(0) == "0")
    assert(constToString(0L) == "0")
    assert(constToString(3.14f) == "3.14")
    assert(constToString(2.71828) == "2.71828")
    assert(constToString(true) == "true")
    assert(constToString('c') == "c")
    assert(constToString("hello") == "hello")
  }

  test("constToString fail for non-constant values") {
    assertCompileTimeError(
      "com.choreograph.tyda.compiletimeextras.constToString(Seq().size)",
      "cannot take constValue"
    )
  }
}
