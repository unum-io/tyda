package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.PrimitiveAggregate

/** A rewrite pass adds a struct around Option in collects.
  *
  * This is needed if the backends (like Spark) that always filters out null
  * values in as part of collect aggregates.
  */
object WrapOptionInCollect extends ExprRule {
  def unapply[T, R](node: ExprNode.Aggregate[T, R]): Option[ExprNode[R]] =
    node match {
      case ExprNode.Aggregate(Nullable(arg), _: PrimitiveAggregate.Collect[t]) =>
        given Codec[t] = arg.codec
        val wrappedAgg =
          ExprNode.Aggregate(ExprNode.makeTuple[Tuple1[t]](Tuple1(arg)), PrimitiveAggregate.Collect())
        Some(ExprNode.MapSeq(wrappedAgg, CompiledExpr[Tuple1[t], t](_._1)))
      case _ => None
    }

  override def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case WrapOptionInCollect(checked) => checked
      case _ => node
    }
}
