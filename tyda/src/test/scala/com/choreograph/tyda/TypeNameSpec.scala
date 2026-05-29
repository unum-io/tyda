package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

class TypeNameSpec extends AnyFunSuite {

  test("TypeName.name should return the fully qualified name for concrete types") {
    assert(TypeName.name[Int] == "scala.Int")
    assert(TypeName.name[Int.type] == "scala.Int.type")
    assert(TypeName.name[String] == "java.lang.String")
    assert(TypeName.name[List[Int]] == "scala.collection.immutable.List[scala.Int]")
  }

  test("Does not exist for unbounded type parameters") {
    assertCompileTimeError("def test[T] = TypeName.name[T]", "consider adding TypeName as a context bound")
  }

  test("Exists for types parameters with context bound") {
    def test[T: TypeName] = TypeName.name[T]
    assert(test[Int] == "scala.Int")
  }

  test("Does not exist for types using type parameters") {
    assertCompileTimeError(
      "def test[T] = TypeName.name[List[T]]",
      "consider adding TypeName as a context bound"
    )
  }
}
