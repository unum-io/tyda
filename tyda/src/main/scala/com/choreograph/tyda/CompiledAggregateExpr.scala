package com.choreograph.tyda

import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances

/** Same as [[CompiledExpr]] but the result is an [[AggregateExpr]].
  */
private[tyda] final case class CompiledAggregateExpr[T, R](arg: ExprNode.Reference[T], expr: ExprNode[R]) {
  def codec: Codec[R] = expr.codec

  /** Composes this compiled expression with another one, with this function
    * applied last.
    *
    * Same as [[Function1#compose]] but for compiled expressions.
    */
  def compose[A](g: CompiledExpr[A, T]): CompiledAggregateExpr[A, R] = {
    val newArg = ExprNode.Reference[A]()(using g.arg.codec)
    CompiledAggregateExpr(newArg, expr.replace(arg, g.expr.replace(g.arg, newArg)))
  }

  /** Composes this compiled expression with another one, with this function
    * applied first.
    *
    * Same as [[Function1#andThen]] but for compiled expressions.
    */
  def andThen[A](g: CompiledExpr[R, A]): CompiledAggregateExpr[T, A] = {
    val newArg = ExprNode.Reference[T]()(using arg.codec)
    CompiledAggregateExpr(newArg, g.expr.replace(g.arg, expr.replace(arg, newArg)))
  }

  /** Combines this compiled expression with another one, producing a tuple of
    * results.
    */
  def combine[R2](g: CompiledExpr[T, R2]): CompiledAggregateExpr[T, (R2, R)] = {
    val newArg = ExprNode.Reference[T]()(using arg.codec)
    CompiledAggregateExpr(
      newArg,
      ExprNode.makeTuple[(R2, R)](g.expr.replace(g.arg, newArg), expr.replace(arg, newArg))
    )
  }
}

private[tyda] object CompiledAggregateExpr {
  def apply[T: Codec, R](f: Expr[T] => AggregateExpr[R]): CompiledAggregateExpr[T, R] = {
    val arg = ExprNode.Reference[T]()
    CompiledAggregateExpr(arg, AggregateExpr.unlift(f(Expr.lift(arg))))
  }

  def apply[T: Codec, R <: Tuple](
      functions: Tuple.Map[R, [X] =>> Expr[T] => AggregateExpr[X]]
  ): CompiledAggregateExpr[T, R] = {
    val arg = ExprNode.Reference[T]()
    val exprs = tupleInstances(functions).mapK([t] => f => AggregateExpr.unlift(f(Expr.lift(arg))))
    CompiledAggregateExpr(arg, ExprNode.makeTuple(exprs.toTuple))
  }
}
