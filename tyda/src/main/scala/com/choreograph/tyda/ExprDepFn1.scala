package com.choreograph.tyda

/** Dependently typed function on a Expr */
trait ExprDepFn1[T] {
  type Out
  def apply(e: Expr[T]): Expr[Out]
}
