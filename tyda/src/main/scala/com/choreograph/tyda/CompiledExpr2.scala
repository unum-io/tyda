package com.choreograph.tyda

import com.choreograph.tyda.Expr.AsExpr

/** Same as [[CompiledExpr]] but for a function taking two arguments.
  */
private[tyda] final case class CompiledExpr2[T1, T2, R](
    arg1: ExprNode.Reference[T1],
    arg2: ExprNode.Reference[T2],
    expr: ExprNode[R]
) {
  def codec: Codec[R] = expr.codec

  /** Compose this compiled expression with two other compiled expressions, one
    * for each argument.
    */
  def compose[A1, A2](g1: CompiledExpr[A1, T1], g2: CompiledExpr[A2, T2]): CompiledExpr2[A1, A2, R] = {
    val newArg1 = ExprNode.Reference[A1]()(using g1.arg.codec)
    val newArg2 = ExprNode.Reference[A2]()(using g2.arg.codec)
    val replacedExpr = expr
      .replace(arg1, g1.expr.replace(g1.arg, newArg1))
      .replace(arg2, g2.expr.replace(g2.arg, newArg2))
    CompiledExpr2(newArg1, newArg2, replacedExpr)
  }
}

private[tyda] object CompiledExpr2 {
  def apply[T1: Codec, T2: Codec, R](f: (Expr[T1], Expr[T2]) => Expr[R]): CompiledExpr2[T1, T2, R] = {
    val arg1 = ExprNode.Reference[T1]()
    val arg2 = ExprNode.Reference[T2]()
    CompiledExpr2(arg1, arg2, Expr.unlift(f(Expr.lift(arg1), Expr.lift(arg2))))
  }
  def apply[T1: Codec, T2: Codec, R, I: AsExpr.Of[R]](
      f: (Expr[T1], Expr[T2]) => I
  ): CompiledExpr2[T1, T2, R] = apply((t1, t2) => AsExpr(f(t1, t2)))
}
