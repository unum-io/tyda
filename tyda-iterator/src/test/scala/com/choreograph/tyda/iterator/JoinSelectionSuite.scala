package com.choreograph.tyda.iterator

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.ExprNode

class JoinSelectionSuite extends AnyFunSuite {
  test("extract simple == join predicate") {
    val lhs = ExprNode.Reference[Int]()
    val rhs = ExprNode.Reference[Int]()
    val expr = ExprNode.Equals(lhs, rhs)
    val joinCondition = JoinSelection.extractEquijoinPredicates(CompiledExpr2(lhs, rhs, expr))
    assert(joinCondition == JoinCondition(lhs, rhs, Set((lhs, rhs)), None))
  }

  test("extract equi predicate inside conjunction") {
    val lhs = ExprNode.Reference[Int]()
    val rhs = ExprNode.Reference[Int]()
    val nonEqui = ExprNode.Equals(lhs, ExprNode.Literal(0))
    val expr = ExprNode.Equals(lhs, rhs) && nonEqui
    val joinCondition = JoinSelection.extractEquijoinPredicates(CompiledExpr2(lhs, rhs, expr))
    assert(joinCondition == JoinCondition(lhs, rhs, Set((lhs, rhs)), Some(nonEqui)))
  }

  test("extract multiple equi predicate inside conjunction") {
    val lRef = ExprNode.Reference[(Int, Int)]()
    val rRef = ExprNode.Reference[(Int, Int)]()
    val lhs: ExprNode[(Int, Int)] = lRef
    val rhs: ExprNode[(Int, Int)] = rRef
    val expr = (ExprNode.Equals(lhs._1, rhs._1)) && ExprNode.Equals(lhs._2, rhs._2)
    val joinCondition = JoinSelection.extractEquijoinPredicates(CompiledExpr2(lRef, rRef, expr))
    assert(joinCondition == JoinCondition(lRef, rRef, Set((lhs._1, rhs._1), (lhs._2, rhs._2)), None))
  }

  test("only extract == conditions where both sides has references") {
    val lhs = ExprNode.Reference[Int]()
    val rhs = ExprNode.Reference[Int]()
    val expr = ExprNode.Equals(lhs, ExprNode.Literal(0)) && ExprNode.Equals(rhs, ExprNode.Literal(1))
    val joinCondition = JoinSelection.extractEquijoinPredicates(CompiledExpr2(lhs, rhs, expr))
    assert(joinCondition == JoinCondition(lhs, rhs, Set(), Some(expr)))
  }

  test("do not extract equi consition in disjunction") {
    val lhs = ExprNode.Reference[Int]()
    val rhs = ExprNode.Reference[Int]()
    val expr = ExprNode.Equals(lhs, rhs) || ExprNode.Equals(lhs, ExprNode.Literal(0))
    val joinCondition = JoinSelection.extractEquijoinPredicates(CompiledExpr2(lhs, rhs, expr))
    assert(joinCondition == JoinCondition(lhs, rhs, Set(), Some(expr)))
  }
}
