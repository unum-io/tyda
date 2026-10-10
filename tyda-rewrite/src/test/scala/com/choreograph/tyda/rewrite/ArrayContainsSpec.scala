package com.choreograph.tyda.rewrite

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.ExprNode

class ArrayContainsSpec extends AnyFunSuite {

  test("ArrayContains should match Expr[Seq[T]].contains") {
    val rNode = ExprNode.Reference[Seq[Int]]()
    val contains = rNode.contains(1)
    assert(contains match {
      case ArrayContains(`rNode`, ExprNode.Literal(1, _)) => true
      case _ => false
    })
  }

  test("ArrayContains should match Expr[Vector[T]].contains") {
    val rNode = ExprNode.Reference[Vector[Int]]()
    val contains = rNode.contains(1)
    assert(contains match {
      case ArrayContains(ExprNode.ToRepr(`rNode`, _), ExprNode.Literal(1, _)) => true
      case _ => false
    })
  }

  test("ArrayContains should match Expr[T].in") {
    val rNode = ExprNode.Reference[Int]()
    val contains = rNode.in(1, 2, 3)
    assert(contains match {
      case ArrayContains(ExprNode.MakeSeq(_, _), `rNode`) => true
      case _ => false
    })
  }
}
