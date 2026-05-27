package com.choreograph.tyda

private[tyda] type CompiledExprOrExplode[T, R] = CompiledExpr[T, R] | CompiledExplodeExpr[T, R]

private[tyda] object CompiledExprOrExplode {
  type From[T] = [R] =>> CompiledExprOrExplode[T, R]

  def apply[T: Codec, R](f: Expr[T] => Expr[R] | ExplodeExpr[R]): CompiledExprOrExplode[T, R] = {
    val arg = ExprNode.Reference[T]()
    val expr = f(Expr.lift(arg))
    expr match {
      case e: Expr[R] => CompiledExpr(arg, Expr.unlift(e))
      case e: ExplodeExpr[R] => CompiledExplodeExpr(arg, e.expr)
    }
  }
  def apply[T: Codec, R, I: AsExprOrExplode.Of[R]](f: Expr[T] => I): CompiledExprOrExplode[T, R] =
    apply(f.andThen(AsExprOrExplode(_)))

  extension [T, R](compiled: CompiledExprOrExplode[T, R]) {
    def compose[A](g: CompiledExpr[A, T]): CompiledExprOrExplode[A, R] =
      compiled match {
        case compiled @ CompiledExpr(_, _) => compiled.compose(g)
        case compiled @ CompiledExplodeExpr(_, _) => compiled.compose(g)
      }
  }
}
