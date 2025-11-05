package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ExprNode.WhenThen

private[tyda] object If {

  /** Matches an if condition. Also normalized negated conditions so pattern
    * matches looking for specific patterns do not need to handle both cases
    * explicitly.
    */
  def unapply[U](
      expr: ExprNode[U]
  ): Option[(cond: ExprNode[Boolean], ifTrue: ExprNode[U], ifFalse: ExprNode[U])] =
    expr match {
      case ExprNode.Cases(WhenThen(ExprNode.Not(cond), ifTrue), Seq(), ifFalse) =>
        Some((cond, ifFalse, ifTrue))
      case ExprNode.Cases(WhenThen(cond, ifTrue), Seq(), ifFalse) => Some((cond, ifTrue, ifFalse))
      case _ => None
    }
}
