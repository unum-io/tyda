package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

class SimpleTypeNameSpec extends AnyFunSuite {

  test("TypeName.name should return the fully qualified name for concrete types") {
    assert(SimpleTypeName.name[Int] == "Int")
    assert(SimpleTypeName.name[Int.type] == "Int.type")
    assert(SimpleTypeName.name[String] == "String")
    assert(SimpleTypeName.name[List[Int]] == "List[Int]")
    assert(SimpleTypeName.name[(Int, Long, String)] == "Tuple3[Int,Long,String]")
    assert(SimpleTypeName.name[(Int, (Long, Byte))] == "Tuple2[Int,Tuple2[Long,Byte]]")
  }

  test("Does not exist for unbounded type parameters") {
    assertCompileTimeError(
      "def test[T] = SimpleTypeName.name[T]",
      "consider adding SimpleTypeName as a context bound"
    )
  }

  test("Exists for types parameters with context bound") {
    def test[T: SimpleTypeName] = SimpleTypeName.name[T]
    assert(test[Int] == "Int")
  }

  test("Does not exist for types using type parameters") {
    assertCompileTimeError(
      "def test[T] = SimpleTypeName.name[List[T]]",
      "consider adding SimpleTypeName as a context bound"
    )
  }

  test("Simple backticked names works") {
    type `MyBackTickedTypeName`
    assert(SimpleTypeName.name[`MyBackTickedTypeName`] == "MyBackTickedTypeName")

    type `Square[Brack]ets`
    assert(SimpleTypeName.name[`Square[Brack]ets`] == "Square[Brack]ets")

    type `Com,mas`
    assert(SimpleTypeName.name[`Com,mas`] == "Com,mas")

    type `Spa   ces`
    assert(SimpleTypeName.name[`Spa   ces`] == "Spa   ces")

    type `🤔`
    assert(SimpleTypeName.name[`🤔`] == "🤔")
  }
}
