package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

private[tyda] object Deterministic {

  /** Matches ExprNode instances that are non-deterministic, i.e. may return
    * different values for different rows or invocations.
    */
  def unapply(expr: ExprNode[?]): Boolean =
    expr match {
      case ExprNode.Rand() => false
      case _ => true
    }
}

def allDeterministic(expr: ExprNode[?]): Boolean = expr.forall(Deterministic.unapply)
