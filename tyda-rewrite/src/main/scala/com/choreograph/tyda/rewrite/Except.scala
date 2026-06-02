package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode

object Except {
  def unapply[T](ds: Dataset[T]): Option[(Dataset[T], Dataset[T])] =
    ds match {
      case Dataset.Distinct(Dataset.LeftAntiJoin(left, right, on)) => on.expr match {
          case ExprNode.Equals(lhs, rhs) => (on.arg1, on.arg2) match {
              case (`lhs`, `rhs`) | (`rhs`, `lhs`) => Some((left, right))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
}
