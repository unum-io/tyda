package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.shapeless3extras.mapConst

private[tyda] object StructFields {

  /** Extractor matches an [[ExprNode]] that is a struct and extracts the fields
    */
  def unapply[T](expr: ExprNode[T]): Option[Seq[ExprNode[?]]] =
    expr.codec match {
      case Codec.Product(_, fields, _) =>
        Some(fields.mapConst[ExprNode[?]]([t] => f => ExprNode.Select(expr, f.name)))
      case codec @ Codec.FromInjection(_, _) => unapply(ExprNode.ToRepr(expr, codec))
      case _ => None
    }
}
