package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.shapeless3extras.mapConst

/** A rewrite pass that removes uneccessary [[ExprNode.Select]].
  *
  * When a [[ExprNode.MakeProduct]] sits directly inside a [[ExprNode.Select]]
  * we can peform the select directly and remove both of the expressions.
  */
object SimplifySelects extends ExprRule {
  def unapply[T](node: ExprNode.Select[?, T]): Option[ExprNode[T]] =
    node match {
      case ExprNode.Select(ExprNode.MakeProduct(values, codec @ Codec.Product(_, fields, _)), field) =>
        val idx = fields.mapConst([t] => _.name).indexOf(field)
        // TYPE SAFETY: All elements in value are of type ExprNode[_]
        Some(values.productElement(idx).asInstanceOf[ExprNode[T]])
      case _ => None
    }

  def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case SimplifySelects(simplified) => simplified
      case other => other
    }
}
