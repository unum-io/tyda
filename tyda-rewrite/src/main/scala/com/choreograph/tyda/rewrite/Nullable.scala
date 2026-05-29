package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.ExprNode

private[tyda] object Nullable {

  /** Refines an ExprNode[T] into an ExprNode[T] & ExprNode[Option[?]] if its
    * codec is Codec.Option[?].
    */
  def unapply[T](expr: ExprNode[T]): Option[expr.type & ExprNode[Option[?]]] =
    // TYPE SAFETY: Codec.Option implies that T <: Option[?]
    Option.when(expr.codec.isInstanceOf[Codec.Option[?]])(expr.asInstanceOf[expr.type & ExprNode[Option[?]]])
}
