package com.choreograph.tyda.rewrite

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode

class MapOptionSpec extends AnyFunSuite {

  test("MapOption should match Expr[T].map") {
    val rNode = ExprNode.Reference[Option[Int]]()
    assert(Expr.unlift(Expr.lift(rNode).map(identity)) match {
      case MapOption(arg, body) if arg == body => true
      case _ => false
    })
  }
}
