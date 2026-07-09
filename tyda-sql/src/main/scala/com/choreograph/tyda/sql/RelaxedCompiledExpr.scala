package com.choreograph.tyda.sql

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.ExprNode

/** Similar to [[CompiledExpr]], but the expr may also contains Aggregate nodes
  * or Explode nodes.
  */
private[tyda] final case class RelaxedCompiledExpr[T, R](arg: ExprNode.Reference[T], expr: ExprNode[R]) {
  def codec: Codec[R] = expr.codec

  def compose[A](g: CompiledExpr[A, T]): RelaxedCompiledExpr[A, R] =
    RelaxedCompiledExpr(g.arg, expr.replace(arg, g.expr))

  def compose[A](g: CompiledAggregateExpr[A, T]): RelaxedCompiledExpr[A, R] =
    RelaxedCompiledExpr(g.arg, expr.replace(arg, g.expr))

  def compose[A](g: CompiledExpr[A, T] | CompiledAggregateExpr[A, T]): RelaxedCompiledExpr[A, R] =
    g match {
      case c: CompiledExpr[A, T] => compose(c)
      case c: CompiledAggregateExpr[A, T] => compose(c)
    }
}

private[tyda] object RelaxedCompiledExpr {
  def apply[T, R](compiled: CompiledExpr[T, R]): RelaxedCompiledExpr[T, R] =
    RelaxedCompiledExpr(compiled.arg, compiled.expr)

  def apply[T, R](compiled: CompiledExplodeExpr[T, R]): RelaxedCompiledExpr[T, R] =
    RelaxedCompiledExpr(compiled.arg, compiled.expr)

  def apply[T, R](compiled: CompiledAggregateExpr[T, R]): RelaxedCompiledExpr[T, R] =
    RelaxedCompiledExpr(compiled.arg, compiled.expr)

  def apply[T, R](
      compiled: CompiledExpr[T, R] | CompiledExplodeExpr[T, R] | CompiledAggregateExpr[T, R]
  ): RelaxedCompiledExpr[T, R] =
    compiled match {
      case c: CompiledExpr[T, R] => apply(c)
      case c: CompiledExplodeExpr[T, R] => apply(c)
      case c: CompiledAggregateExpr[T, R] => apply(c)
    }
}
