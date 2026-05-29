package com.choreograph.tyda.iterator

import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ReferenceId
import com.choreograph.tyda.iterator.ExprEvaluation.lambda2

private[tyda] object JoinSelection {
  private object Empty {
    def unapply[T](e: Set[T]): Boolean = e.isEmpty
  }

  def join[T, U](left: Iterator[T], right: Iterator[U], p: CompiledExpr2[T, U, Boolean]): Iterator[(T, U)] =
    extractEquijoinPredicates(p) match {
      case JoinCondition(equi = Empty()) => NestedLoopJoin.join(left, right, lambda2(p))
      case equi =>
        val (leftKey, rightKey, predicate) = equi.toKeysAndPredicate
        HashJoin.join(left, right, leftKey, rightKey, predicate)
    }

  def leftOuterJoin[T, U](
      left: Iterator[T],
      right: Iterator[U],
      p: CompiledExpr2[T, U, Boolean]
  ): Iterator[(T, Option[U])] =
    extractEquijoinPredicates(p) match {
      case JoinCondition(equi = Empty()) => NestedLoopJoin.leftOuterJoin(left, right, lambda2(p))
      case equi =>
        val (leftKey, rightKey, predicate) = equi.toKeysAndPredicate
        HashJoin.leftOuterJoin(left, right, leftKey, rightKey, predicate)
    }

  def fullOuterJoin[T, U](
      left: Iterator[T],
      right: Iterator[U],
      p: CompiledExpr2[T, U, Boolean]
  ): Iterator[(Option[T], Option[U])] =
    extractEquijoinPredicates(p) match {
      case JoinCondition(equi = Empty()) => NestedLoopJoin.fullOuterJoin(left, right, lambda2(p))
      case equi =>
        val (leftKey, rightKey, predicate) = equi.toKeysAndPredicate
        HashJoin.fullOuterJoin(left, right, leftKey, rightKey, predicate)
    }

  def leftAntiJoin[T, U](
      left: Iterator[T],
      right: Iterator[U],
      p: CompiledExpr2[T, U, Boolean]
  ): Iterator[T] =
    extractEquijoinPredicates(p) match {
      case JoinCondition(equi = Empty()) => NestedLoopJoin.leftAntiJoin(left, right, lambda2(p))
      case equi =>
        val (leftKey, rightKey, predicate) = equi.toKeysAndPredicate
        HashJoin.leftAntiJoin(left, right, leftKey, rightKey, predicate)
    }

  def extractEquijoinPredicates[T, U](compiled: CompiledExpr2[T, U, Boolean]): JoinCondition[T, U] = {
    def extract(
        e: ExprNode[Boolean]
    ): (equi: Set[(ExprNode[?], ExprNode[?])], other: Option[ExprNode[Boolean]]) =
      e match {
        case ExprNode.Equals(left, right) =>
          def collectRefIds(e: ExprNode[?]): Set[ReferenceId] =
            e.collect { case ExprNode.Reference(id = id) => id }.toSet

          val leftId = compiled.arg1.id
          val rightId = compiled.arg2.id

          // Both side of the eq condition references one and only one side of the join
          def isEquiCondition(refsL: Set[ReferenceId], refsR: Set[ReferenceId]): Boolean =
            refsL.contains(leftId) && !refsL.contains(rightId) && refsR.contains(rightId) &&
              !refsR.contains(leftId)

          val leftRefs = collectRefIds(left)
          val rightRefs = collectRefIds(right)

          if isEquiCondition(leftRefs, rightRefs) then (Set((left, right)), None)
          else if isEquiCondition(rightRefs, leftRefs) then (Set((right, left)), None)
          else (Set(), Some(e))

        case ExprNode.And(left, right) =>
          val (leftEqui, leftOther) = extract(left)
          val (rightEqui, rightOther) = extract(right)
          (leftEqui ++ rightEqui, leftOther.zip(rightOther).map(_ && _).orElse(leftOther).orElse(rightOther))
        case other => (Set(), Some(other))
      }
    val (equi, other) = extract(compiled.expr)
    JoinCondition(compiled.arg1, compiled.arg2, equi, other)
  }
}
