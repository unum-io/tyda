package com.choreograph.tyda

import com.choreograph.tyda.Expr.AsExpr

/** Represents a function Expr[T] => Expr[R] that been compiled with a specific
  * argument.
  *
  * Compiled here just mean that the function was executed with specific
  * `Expr.Reference[T]`. The reference contains a unique [[ReferenceId]] that
  * makes it possible to detect if compiled expr references any other
  * references.
  */
private[tyda] final case class CompiledExpr[T, R](arg: ExprNode.Reference[T], expr: ExprNode[R]) {
  def codec: Codec[R] = expr.codec

  /** Composes this compiled expression with another one, with this function
    * applied last.
    *
    * Same as [[Function1#compose]] but for compiled expressions.
    */
  def compose[A](g: CompiledExpr[A, T]): CompiledExpr[A, R] = {
    val newArg = ExprNode.Reference[A]()(using g.arg.codec)
    CompiledExpr(newArg, expr.replace(arg, g.expr.replace(g.arg, newArg)))
  }

  /** Composes this compiled expression with another one, with this function
    * applied first.
    *
    * Same as [[Function1#andThen]] but for compiled expressions.
    */
  def andThen[A](g: CompiledExpr[R, A]): CompiledExpr[T, A] = g.compose(this)

  /** Combines this compiled expression with another one, producing a tuple of
    * results.
    */
  def combine[R2](g: CompiledExpr[T, R2]): CompiledExpr[T, (R, R2)] = {
    val newArg = ExprNode.Reference[T]()(using arg.codec)
    CompiledExpr(
      newArg,
      ExprNode.makeTuple[(R, R2)](expr.replace(arg, newArg), g.expr.replace(g.arg, newArg))
    )
  }
}

private[tyda] object CompiledExpr {
  def apply[T: Codec, R](f: Expr[T] => Expr[R]): CompiledExpr[T, R] = {
    val arg = ExprNode.Reference[T]()
    CompiledExpr(arg, Expr.unlift(f(Expr.lift(arg))))
  }
  def apply[T: Codec, R, I: AsExpr.Of[R]](f: Expr[T] => I): CompiledExpr[T, R] = apply(f.andThen(AsExpr(_)))

  extension [T](compiled: CompiledExpr[T, Boolean]) {
    infix def &&(other: CompiledExpr[T, Boolean]): CompiledExpr[T, Boolean] = {
      val newArg = ExprNode.Reference[T]()(using compiled.arg.codec)
      val newBody =
        ExprNode.And(compiled.expr.replace(compiled.arg, newArg), other.expr.replace(other.arg, newArg))
      CompiledExpr(newArg, newBody)
    }
  }
}
