package com.choreograph.tyda.rewrite

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode

class IsNoneSpec extends AnyFunSuite {

  test("IsNone should match Expr[T].isEmpty") {
    val rNode = ExprNode.Reference[Option[Int]]()
    assert(Expr.unlift(Expr.lift(rNode).isEmpty) match {
      case IsNone(`rNode`) => true
      case _ => false
    })
  }

  test("IsNone.unapply should match IsNone.apply") {
    val rNode = ExprNode.Reference[Option[String]]()
    val isNoneExpr = IsNone(rNode)
    isNoneExpr match {
      case IsNone(_) => ()
      case _ => fail("IsNone.unapply did not match IsNone.apply")
    }
  }
}
