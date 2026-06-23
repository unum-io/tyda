package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

private[tyda] object NonDeterministic {

  /** Matches ExprNode instances that are non-deterministic, i.e. may return
    * different values for different rows or invocations.
    */
  def unapply(expr: ExprNode[?]): Boolean =
    expr match {
      case ExprNode.Rand() => true
      case _ => false
    }
}
