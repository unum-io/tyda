package com.choreograph.tyda.iterator

import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.iterator.ExprEvaluation.lambda
import com.choreograph.tyda.iterator.ExprEvaluation.lambda2

private[tyda] final case class JoinCondition[T, U](
    rightRef: ExprNode.Reference[T],
    leftRef: ExprNode.Reference[U],
    equi: Set[(ExprNode[?], ExprNode[?])],
    other: Option[ExprNode[Boolean]]
) {
  def toKeysAndPredicate: (T => Tuple, U => Tuple, (T, U) => Boolean) = {
    val leftExtractors = equi.iterator.map(c => CompiledExpr(rightRef, c._1)).map(lambda).toArray
    val rightExtractors = equi.iterator.map(c => CompiledExpr(leftRef, c._2)).map(lambda).toArray
    val otherCondition = CompiledExpr2(rightRef, leftRef, other.getOrElse(ExprNode.Literal(true)))
    (
      t => Tuple.fromArray(leftExtractors.map(_(t))),
      u => Tuple.fromArray(rightExtractors.map(_(u))),
      lambda2(otherCondition)
    )
  }
}
