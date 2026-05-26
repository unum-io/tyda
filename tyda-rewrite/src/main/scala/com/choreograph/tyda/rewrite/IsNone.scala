package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

private[tyda] object IsNone {

  /** If expr represents an equality comparison against None, will extract the
    * node being compared.
    */
  def unapply[U](expr: ExprNode[U]): Option[ExprNode[Option[?]]] =
    expr match {
      case ExprNode.Equals(Nullable(opt), ExprNode.None(_)) => Some(opt)
      case ExprNode.Equals(ExprNode.None(_), Nullable(opt)) => Some(opt)
      case _ => None
    }

  def apply[U](expr: ExprNode[Option[U]]): ExprNode[Boolean] =
    ExprNode.Equals(expr, ExprNode.None(expr.codec.element))
}
