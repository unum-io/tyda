package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Expr.AsExpr

class AsExprSpec extends AnyFunSuite {
  val ref = ExprNode.Reference[(Int, Int)]()

  def create[R, I: AsExpr.Of[R]](f: Expr[(Int, Int)] => I): ExprNode[R] =
    Expr.unlift(AsExpr(f(Expr.lift(ref))))

  test("support expr") {
    val actual = create(_._1)
    assert(actual == ExprNode.Select[(Int, Int), Int](ref, "_1"))
  }

  test("support literal") {
    val actual = create(_ => 1)
    assert(actual == ExprNode.Literal(1))
  }

  test("support empty tuple") {
    val actual = create(_ => EmptyTuple)
    assert(actual == ExprNode.makeTuple[EmptyTuple](EmptyTuple))
  }

  test("support literal tuple") {
    val actual = create(_ => (1, 2))
    assert(actual == ExprNode.makeTuple[(Int, Int)]((ExprNode.Literal(1), ExprNode.Literal(2))))
  }

  test("support tuple of exprs") {
    val actual = create(t => (t._1, t._2))
    assert(
      actual == ExprNode.makeTuple[(Int, Int)](
        (ExprNode.Select[(Int, Int), Int](ref, "_1"), ExprNode.Select[(Int, Int), Int](ref, "_2"))
      )
    )
  }

  test("support tuple of exprs and primitive literals") {
    val actual = create(t => (t._1, 1))
    assert(
      actual ==
        ExprNode.makeTuple[(Int, Int)]((ExprNode.Select[(Int, Int), Int](ref, "_1"), ExprNode.Literal(1)))
    )
  }

  test("support tuple of exprs and collection literals") {
    val actual = create(t => (t._1, Map[Int, Int]()))
    assert(
      actual == ExprNode.makeTuple[(Int, Map[Int, Int])]((
        ExprNode.Select[(Int, Int), Int](ref, "_1"),
        ExprNode.MakeMap(ExprNode.MakeSeq(Seq.empty[ExprNode[(Int, Int)]]))
      ))
    )
  }

  test("support nested tuple of exprs") {
    val actual = create(t => (t._1, (t._2, 1)))
    assert(
      actual == ExprNode.makeTuple[(Int, (Int, Int))]((
        ExprNode.Select[(Int, Int), Int](ref, "_1"),
        ExprNode.makeTuple[(Int, Int)]((ExprNode.Select[(Int, Int), Int](ref, "_2"), ExprNode.Literal(1)))
      ))
    )
  }
}
