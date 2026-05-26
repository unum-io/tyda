package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.TreeApi.Continue

/** Rewrites null-safe equals in join conditions into a combination of regular
  * equals and null checks.
  *
  * This is needed because BigQuery does not consider null-safe equals in outer
  * joins as an equi condition, which is required for the join to be allowed.
  */
object RemoveNullSafeEqualsInJoinCondition extends DatasetRule {

  private object RewritableJoinCondition {

    private def valueOrDummy[U](node: ExprNode[Option[U]]): ExprNode[U] = {
      val dummyValue = NotNullNonEmptyDummyLiteral.create(node.codec.element)
      ExprNode.KnownNotNull(ExprNode.Coalesce(Seq(node, ExprNode.MakeSome(dummyValue))))
    }

    private object RewriteableExprNode {
      def unapply[T](node: ExprNode[T]): Option[ExprNode[T]] =
        node match {
          case IsNone(_) => None
          case ExprNode.Equals(Nullable(lhs), Nullable(rhs)) => Some(ExprNode.And(
              ExprNode.Equals(valueOrDummy(lhs), valueOrDummy(rhs)),
              ExprNode.Equals(IsNone(lhs), IsNone(rhs))
            ))
          case _ => None
        }
    }

    def unapply[T, U](cond: CompiledExpr2[T, U, Boolean]): Option[CompiledExpr2[T, U, Boolean]] =
      val hasRewriteableExpr = cond
        .expr
        .exists {
          case RewriteableExprNode(_) => true
          case _ => false
        }
      Option.when(hasRewriteableExpr)(cond.copy(expr =
        cond
          .expr
          .transformDown([t] =>
            _ match {
              case RewriteableExprNode(rewritten) => Continue(rewritten)
              case other => Continue(other)
            }
          )
      ))
  }

  def unapply[T](ds: Dataset[T]): Option[Dataset[T]] =
    ds match {
      case Dataset.Join(left, right, RewritableJoinCondition(newCond)) =>
        Some(Dataset.Join(left, right, newCond))
      case Dataset.LeftOuterJoin(left, right, RewritableJoinCondition(newCond)) =>
        Some(Dataset.LeftOuterJoin(left, right, newCond))
      case Dataset.FullOuterJoin(left, right, RewritableJoinCondition(newCond)) =>
        Some(Dataset.FullOuterJoin(left, right, newCond))
      case _ => None
    }

  def apply[T](ds: Dataset[T]): Dataset[T] = unapply(ds).getOrElse(ds)
}
