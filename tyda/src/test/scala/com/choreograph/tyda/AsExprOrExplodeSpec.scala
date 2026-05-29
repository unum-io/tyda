package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError
import com.choreograph.tyda.functions.explode

class AsExprOrExplodeSpec extends AnyFunSuite {
  type Row = (Option[Int], Seq[Int], Int)

  val ref = ExprNode.Reference[Row]()

  def create[R, I: AsExprOrExplode.Of[R]](f: Expr[Row] => I): Expr[R] | ExplodeExpr[R] =
    AsExprOrExplode(f(Expr.lift(ref)))

  test("support explode Option") {
    val actual = create(explode(_._1))
    assert(actual == ExplodeExpr(ExprNode.OptionToIterable(ExprNode.Select[Row, Option[Int]](ref, "_1"))))
  }

  test("support explode Seq") {
    val actual = create(explode(_._2))
    assert(actual == ExplodeExpr(ExprNode.UpcastToIterable(ExprNode.Select[Row, Seq[Int]](ref, "_2"))))
  }

  test("support expr") {
    val actual = create(_._3)
    actual match {
      case e: Expr[?] => assert(e.node == ExprNode.Select[Row, Int](ref, "_3"))
      case _ => fail("Expected Expr, but got something else")
    }
  }

  test("do not allow explode for Int") {
    assertCompileTimeError("create(explode(_._3))", "method explode in object Expr")
  }
}
