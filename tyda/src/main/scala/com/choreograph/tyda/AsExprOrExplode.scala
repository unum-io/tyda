package com.choreograph.tyda

import com.choreograph.tyda.Expr.AsExpr

/* Type class that provide conversion to Expr[R] from ExplodeExpr[R] or T: AsExpr[R]
 *
 * This is used as a bound for methods that can take an explode expression like [[Dataset.select]]. */
trait AsExprOrExplode[T, R] extends Conversion[T, Expr[R] | ExplodeExpr[R]]

object AsExprOrExplode {
  type Of[R] = [T] =>> AsExprOrExplode[T, R]

  def apply[R, T](using i: AsExprOrExplode[R, T]): AsExprOrExplode[R, T] = i

  given explodeExpr[T]: AsExprOrExplode[ExplodeExpr[T], T] = identity(_)
  given asExpr[T, R: AsExpr.Of[T]]: AsExprOrExplode[R, T] = AsExpr(_)
}
