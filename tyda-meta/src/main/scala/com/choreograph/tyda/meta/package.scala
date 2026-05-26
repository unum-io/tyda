package com.choreograph.tyda

/** Type aliases for functions over Expr and AggregateExpr
  */
package object meta {
  type ExprFn0[R] = Function0[Expr[R]]
  type ExprFn[T, R] = Function1[Expr[T], Expr[R]]
  type ExprFn2[T1, T2, R] = Function2[Expr[T1], Expr[T2], Expr[R]]
  type AggregateFn[T, R] = Function1[Expr[T], AggregateExpr[R]]
}
