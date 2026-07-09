package com.choreograph.tyda

// In most query engine explode/unnest is only supported as top level operations in select
// statements. So this class does not subtype Expr, so we can enforce this constraint.
final case class ExplodeExpr[T](private[tyda] val asNode: ExprNode.Explode[T]) {
  private[tyda] def asExpr: Expr[T] = Expr.lift(asNode)
}
