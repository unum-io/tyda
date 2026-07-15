package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.PrimitiveAggregate

private[tyda] object ArrayContains {
  final case class Result[T](arr: ExprNode[Seq[T]], element: ExprNode[T])

  private object EqPredicate {
    def unapply[T, R](compiled: CompiledExpr[T, R]): Option[ExprNode[T]] = {
      val arg = compiled.arg
      def isIndependentOfArg(e: ExprNode[?]): Boolean = e.forall(_ != arg)
      compiled.expr match {
        case ExprNode.Equals(`arg`, e) if isIndependentOfArg(e) => Some(e)
        case ExprNode.Equals(e, `arg`) if isIndependentOfArg(e) => Some(e)
        case _ => None
      }
    }
  }

  private object ArrayAny {
    def unapply[U](expr: ExprNode[U]): Option[ExprNode[?]] =
      expr match {
        case ExprNode.AggregateSeq(arr, ExprNode.Literal(false, _), PrimitiveAggregate.BoolOr()) => Some(arr)
        case _ => None
      }
  }

  private object MapEq {
    def unapply[U](expr: ExprNode[U]): Option[Result[?]] =
      expr match {
        case ExprNode.MapSeq(arr, EqPredicate(elem)) => Some(Result(arr, elem))
        case _ => None
      }
  }

  // Matches trees that do a contains check on a array that can we rewritten into a `In` expression in SQL.
  def unapply[U](expr: ExprNode[U]): Option[Result[?]] =
    expr match {
      case ArrayAny(MapEq(res)) => Some(res)
      case ArrayAny(ToFromRepr(MapEq(res))) => Some(res)
      case _ => None
    }
}
