package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CanCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.unreachable

/** Rewrites casts on arrays into a map casting the elements, including arrays
  * nested inside product casts.
  *
  * This is used for query engines like BigQuery that doesn't support casts on
  * arrays.
  */
object RemoveArrayCasts extends ExprRule {
  override def applyOrder: Rule.ApplyOrder = Rule.ApplyOrder.TopDown

  def unapply[T](node: ExprNode.Cast[?, T]): Option[ExprNode[T]] =
    node match {
      case ExprNode.Cast(in, CanCast.SeqToSeq(given CanCast[?, b])) => Some(in.map(_.cast[b]))
      case ExprNode.Cast(in, cast @ CanCast.ProductToProduct(casts)) if needsRemoval(cast) =>
        val fieldCasts = casts.mapConst[CanCast[?, ?]]([a, b] => identity(_))
        val fromFieldNames = in.codec match {
          case Codec.Product(_, fields, _) => fields.mapConst[String]([t] => _.name)
          case _ => unreachable(s"Only products should be casted to products, but found ${in.codec}")
        }
        val targetProd = node.codec match {
          case prod @ Codec.Product(_, _, _) => prod
          case _ => unreachable(s"Only products should have product casts, but found ${node.codec}")
        }
        val elements = fromFieldNames
          .zip(fieldCasts)
          .map((fieldName, fieldCast) => ExprNode.Cast(ExprNode.Select(in, fieldName), fieldCast))
        Some(ExprNode.makeProductUnsafe(elements, targetProd))
      case _ => None
    }

  private def needsRemoval(canCast: CanCast[?, ?]): Boolean =
    canCast match {
      case CanCast.SeqToSeq(_) => true
      case CanCast.ProductToProduct(casts) =>
        casts.mapConst[Boolean]([a, b] => needsRemoval(_)).exists(identity)
      case _ => false
    }

  def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case RemoveArrayCasts(simplified) => simplified
      case other => other
    }
}
