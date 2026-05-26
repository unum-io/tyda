package com.choreograph.tyda.sql

import com.choreograph.tyda.CompiledExpr

private object CompiledExprIdentity {
  def unapply[R, T](expr: CompiledExpr[T, R]): Boolean = expr.arg == expr.expr
}
