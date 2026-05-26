package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

private[tyda] object ToFromRepr {

  /** Extracts the `value` from `ToRepr(FromRepr(value))`.
    */
  def unapply[T, Repr](toRepr: ExprNode.ToRepr[T, Repr]): Option[ExprNode[Repr]] =
    val codec = toRepr.injectionCodec
    toRepr.expr match {
      case ExprNode.FromRepr(value, `codec`) => Some(value)
      case _ => None
    }
}
