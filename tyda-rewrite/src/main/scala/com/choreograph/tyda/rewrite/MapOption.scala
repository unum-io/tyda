package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

private[tyda] object MapOption {

  /** Matches an ExprNode[Option[U]] that is created by mapping over an Option
    * and extracts the argument and body of the mapping function.
    */
  def unapply[U](expr: ExprNode[Option[U]]): Option[(arg: ExprNode.KnownNotNull[?], body: ExprNode[U])] =
    expr match {
      case If(IsNone(opt), ExprNode.None(_), ExprNode.MakeSome(body)) =>
        Some((ExprNode.KnownNotNull(opt), body))
      case _ => None
    }
}
