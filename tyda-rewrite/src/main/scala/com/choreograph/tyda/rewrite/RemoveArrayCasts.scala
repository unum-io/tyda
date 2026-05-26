package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CanCast
import com.choreograph.tyda.ExprNode

/** Rewrites casts on arrays into a map casting the elements.
  *
  * This is used for query engines like BigQuery that doesn't support casts on
  * arrays.
  */
object RemoveArrayCasts extends ExprRule {
  def unapply[T](node: ExprNode.Cast[?, T]): Option[ExprNode[T]] =
    node match {
      case ExprNode.Cast(in, CanCast.SeqToSeq(given CanCast[?, b])) => Some(in.map(_.cast[b]))
      case _ => None
    }

  def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case RemoveArrayCasts(simplified) => simplified
      case other => other
    }
}
