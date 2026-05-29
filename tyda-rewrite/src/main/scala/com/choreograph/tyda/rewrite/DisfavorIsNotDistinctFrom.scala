package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

/** A rewrite pass that tries to rewrite some IS NOT DISTINCT FROM to other
  * equivalent expressions.
  *
  * This is used for query engines like BigQuery that tends too poorly optimize
  * IS NOT DISTINCT FROM. For example, it not considered a valid partition
  * filter.
  */
object DisfavorIsNotDistinctFrom extends ExprRule {
  def unapply(node: ExprNode.Equals[?]): Option[ExprNode[Boolean]] =
    node match {
      case ExprNode.Equals(Nullable(lhs), ExprNode.MakeSome(rhs)) =>
        Some(!lhs.isEmpty && ExprNode.Equals(lhs.get, rhs))
      case ExprNode.Equals(ExprNode.MakeSome(lhs), Nullable(rhs)) =>
        Some(!rhs.isEmpty && ExprNode.Equals(lhs, rhs.get))
      case _ => None
    }

  def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case DisfavorIsNotDistinctFrom(simplified) => simplified
      case other => other
    }
}
