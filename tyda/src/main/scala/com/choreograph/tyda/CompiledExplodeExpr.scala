package com.choreograph.tyda

/** Same as [[CompiledExpr]] but the result should be exploded.
  */
private[tyda] final case class CompiledExplodeExpr[T, R](
    arg: ExprNode.Reference[T],
    expr: ExprNode[Iterable[R]]
) {
  def codec: Codec[R] = expr.codec.element
  def asCompiledExpr: CompiledExpr[T, Iterable[R]] = CompiledExpr(arg, expr)

  /** Composes this compiled expression with another one, with this function
    * applied last.
    *
    * Same as [[Function1#compose]] but for compiled expressions.
    */
  def compose[A](g: CompiledExpr[A, T]): CompiledExplodeExpr[A, R] = {
    val newArg = ExprNode.Reference[A]()(using g.arg.codec)
    CompiledExplodeExpr(newArg, expr.replace(arg, g.expr.replace(g.arg, newArg)))
  }
}
