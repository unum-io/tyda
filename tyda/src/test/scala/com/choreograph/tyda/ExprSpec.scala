package com.choreograph.tyda

import scala.deriving.Mirror

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.compiletimeextras.assertCompileTimeError
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.functions.tuple

object ExprSpec {
  final case class Person(name: String, age: Int)
  enum MyEnum extends EnumStableHashCode {
    case A, B, C
    case D(i: Int)
  }

  val exprPerson = Expr.lift(ExprNode.Reference[Person]())
}

class ExprSpec extends AnyFunSuite {
  import ExprSpec.Person

  test("support accessing fields using name") {
    val eNode = ExprNode.Reference[(Int, Int)]()
    val e: Expr[(Int, Int)] = Expr.lift(eNode)
    assert(e._1.node == ExprNode.Select[(Int, Int), Int](eNode, "_1"))

    // Sanity check of compile check macros
    assertCompiles("e._2")
    // Should fail due to `_3` does not exists
    assertTypeError("e._3")
  }

  test("support boolean operators") {
    val r: Expr[(Boolean, Boolean, Boolean)] = Expr.lift(ExprNode.Reference[(Boolean, Boolean, Boolean)]())
    val b0 = r._1
    val b1 = r._2
    val b2 = r._3

    val expr = !(b0 && b1) || b2
    assert(
      expr.node == ExprNode.Or(ExprNode.Not(ExprNode.And(Expr.unlift(b0), Expr.unlift(b1))), Expr.unlift(b2))
    )
  }

  test("support extractor for tuples") {
    val rNode = ExprNode.Reference[(Int, Boolean)]()
    val r = Expr.lift(rNode)
    r match {
      case Expr(a, b) =>
        assert(a.node == ExprNode.Select[(Int, Boolean), Int](rNode, "_1"))
        assert(b.node == ExprNode.Select[(Int, Boolean), Int](rNode, "_2"))
    }
  }

  test("support extractor for products") {
    val rNode = ExprNode.Reference[Person]()
    val r = Expr.lift(rNode)
    r match {
      case Expr(name, age) =>
        assert(name.node == ExprNode.Select[Person, String](rNode, "name"))
        assert(age.node == ExprNode.Select[Person, Int](rNode, "age"))
    }
  }

  test("support extractor for products with explicit type") {
    val rNode = ExprNode.Reference[Person]()
    val r = Expr.lift(rNode)
    r match {
      case Expr[Person](name, age) =>
        assert(name.node == ExprNode.Select[Person, String](rNode, "name"))
        assert(age.node == ExprNode.Select[Person, Int](rNode, "age"))
    }
  }

  test("support named extractor for products with explicit type") {
    val rNode = ExprNode.Reference[Person]()
    val r = Expr.lift(rNode)
    r match {
      case Expr[Person](name = name) => assert(name.node == ExprNode.Select[Person, String](rNode, "name"))
    }
  }

  test("allow copy with named tuple") { ExprSpec.exprPerson.copy((name = "Alice", age = 30)) }

  test("allow copy with auto-tupling") { ExprSpec.exprPerson.copy(name = "Alice", age = 30) }

  test("disallow copy with wrong field name") {
    assertCompileTimeError(
      "ExprSpec.exprPerson.copy((nonExistentField = 42))",
      "no field named 'nonExistentField' in Person"
    )
  }

  test("disallow copy with wrong field type") {
    assertCompileTimeError("ExprSpec.exprPerson.copy((age = \"notAnInt\"))", "has type Int")
  }

  test("disallow copy with duplicate field names") {
    assertCompileTimeError(
      "ExprSpec.exprPerson.copy((name = \"Alice\", name = \"Bob\"))",
      "Duplicate tuple element name"
    )
  }

  test("create literal using lit function") {
    val expr = lit(42)
    assert(expr.node == ExprNode.Literal(42))
  }

  test("support comparision with literal") {
    val rNode = ExprNode.Reference[Int]()
    val r: Expr[Int] = Expr.lift(rNode)
    val expr = r > 0
    assert(expr.node == ExprNode.Not(ExprNode.LessThanOrEqual(Comparable[Int], rNode, ExprNode.Literal(0))))
  }

  test("support comparision with other expr") {
    val r1Node = ExprNode.Reference[Int]()
    val r1: Expr[Int] = Expr.lift(r1Node)
    val r2Node = ExprNode.Reference[Int]()
    val r2: Expr[Int] = Expr.lift(r2Node)
    val expr = r1 > r2
    assert(expr.node == ExprNode.Not(ExprNode.LessThanOrEqual(Comparable[Int], r1Node, r2Node)))
  }

  test("support select") {
    val r1Node = ExprNode.Reference[(String, Int)]()
    val r1: Expr[(String, Int)] = Expr.lift(r1Node)
    val expr = r1.select[Int]
    assert(expr.node == ExprNode.Select[(String, Int), Int](r1Node, "_2"))
  }

  test("support split") {
    val rNode = ExprNode.Reference[(String, String)]()
    val r = Expr.lift(rNode)
    val str = r._1
    val del = r._2
    val expr = str.split(del)
    assert(expr.node == ExprNode.Split(Expr.unlift(str), Expr.unlift(del)))
  }

  test("support add") {
    val rNode = ExprNode.Reference[(Int, Int)]()
    val r = Expr.lift(rNode)
    val lhs = r._1
    val rhs = r._2
    val expr = lhs + rhs
    assert(expr.node == ExprNode.Add(AdditiveExpr[Int], Expr.unlift(lhs), Expr.unlift(rhs)))
  }

  test("support quotient") {
    val rNode = ExprNode.Reference[(Int, Int)]()
    val r = Expr.lift(rNode)
    val lhs = r._1
    val rhs = r._2
    val expr = lhs / rhs
    assert(expr.node == ExprNode.Quotient(Integral[Int], Expr.unlift(lhs), Expr.unlift(rhs)))
  }

  test("Expr.apply requires exact field match") {
    assertCompiles("Expr[(a: Int)]((a = 0))")
    assertCompileTimeError("""Expr[(a: Int)]((a = ""))""", "has type Int", "has type String")
    assertCompileTimeError("Expr[(a: Int, b: Int)]((a = 0))", "no field named 'b'")
    assertCompileTimeError("Expr[(a: Int)]((a = 0, b = 0))", "no field named 'b'")
  }

  test("error on ++ for duplicate names") {
    val rNode = Expr.lift(ExprNode.Reference[(a: String, b: Int)]())
    val _ = rNode ++ (c = 42)
    assertCompileTimeError("""rNode ++ (a="duplicate")""", "Cannot prove that Tuple.Disjoint")
  }

  test("traverse subqueries in TreeApi") {
    val ds = Dataset.from[Int](Seq()).aggregate(v => count(v) > 10L)
    val exprNode = tuple(ds.value, 1).node
    assert(exprNode.exists {
      case ExprNode.Literal(10, _) => true
      case _ => false
    })
  }

  test("Expr should have a mirror") { summon[Mirror.SumOf[ExprNode[?]]] }

  test("reject invalid cast to smaller integral type") {
    val a: Expr[Int] = lit(42)
    val _ = a.cast[Long]
    assertCompileTimeError("a.cast[Short]", "Cannot cast from Int to Short")
  }

  test("reject invalid cast to smaller decimal") {
    val a: Expr[Decimal[10, 0]] = lit(Decimal[10, 0](1))
    val _ = a.cast[Decimal[38, 2]]
    assertCompileTimeError("a.cast[Decimal[10, 3]]", "Cannot cast from")
  }

  test("Expr.cases requires exhaustive handlers") {
    val rNode = ExprNode.Reference[ExprSpec.MyEnum]()
    val r = Expr.lift(rNode)
    val _: Expr[Int] = r
      .cases[Int]
      .when[ExprSpec.MyEnum.A.type](_ => Expr.lit(1))
      .when[ExprSpec.MyEnum.B.type](_ => Expr.lit(2))
      .when[ExprSpec.MyEnum.C.type](_ => Expr.lit(3))
      .when[ExprSpec.MyEnum.D](e => e.i)

    assert(
      r.cases[Int]
        .when[ExprSpec.MyEnum.A.type](_ => Expr.lit(1))
        .when[ExprSpec.MyEnum.D](e => e.i)
        .isInstanceOf[com.choreograph.tyda.Expr.UnhandledCases[
          (ExprSpec.MyEnum.B.type, ExprSpec.MyEnum.C.type), // Missing cases
          ExprSpec.MyEnum,
          Int
        ]]
    )
  }
}
