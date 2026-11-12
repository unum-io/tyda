package com.choreograph.tyda

import com.choreograph.tyda.Expr.AsExpr
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances

import NamedTuple.NamedTuple

/* Type class that provide conversion to Expr[R] from ExplodeExpr[R] or T: AsExpr[R]
 *
 * This is used as a bound for methods that can take an explode expression like [[Dataset.select]]. */
trait AsExprOrExplode[T, R] extends Conversion[T, Expr[R] | ExplodeExpr[R]]

object AsExprOrExplode {
  type Of[R] = [T] =>> AsExprOrExplode[T, R]

  def apply[R, T](using i: AsExprOrExplode[R, T]): AsExprOrExplode[R, T] = i

  given explodeExpr[T]: AsExprOrExplode[ExplodeExpr[T], T] = identity(_)
  given asExpr[T, R: AsExpr.Of[T]]: AsExprOrExplode[R, T] = AsExpr(_)

  type ExprOrExplode[T] = Expr[T] | ExplodeExpr[T]

  trait TupleAsExprOrExplode[T <: Tuple, TRes <: Tuple] {
    def apply(t: T): Tuple.Map[TRes, ExprOrExplode]
  }

  object TupleAsExprOrExplode {
    def apply[T <: Tuple, R <: Tuple](using i: TupleAsExprOrExplode[T, R]): TupleAsExprOrExplode[T, R] = i

    type Of[TRes <: Tuple] = [T <: Tuple] =>> TupleAsExprOrExplode[T, TRes]

    given empty: TupleAsExprOrExplode[EmptyTuple, EmptyTuple] with {
      def apply(t: EmptyTuple): EmptyTuple = EmptyTuple
    }

    given head[R, H: AsExprOrExplode.Of[R], TRes <: Tuple, T <: Tuple: TupleAsExprOrExplode.Of[TRes]]
        : TupleAsExprOrExplode[H *: T, R *: TRes] with {
      def apply(t: H *: T): Tuple.Map[R *: TRes, ExprOrExplode] =
        AsExprOrExplode[H, R](t.head) *: TupleAsExprOrExplode[T, TRes](t.tail)
    }
  }

  given tuple[TRes <: Tuple, T <: Tuple: TupleAsExprOrExplode.Of[TRes]]: AsExprOrExplode[T, TRes] =
    inputTuple => {
      val tupledExpression = tupleInstances(TupleAsExprOrExplode(inputTuple)).mapK([t] =>
        _ match {
          case expr: Expr[`t`] => Expr.unlift(expr)
          case explode: ExplodeExpr[`t`] => Expr.unlift(explode.asExpr)
        }
      )
      Expr.lift(ExprNode.makeTuple(tupledExpression.toTuple))
    }

  given namedTuple[Fields <: Tuple: StringLiterals, TRes <: Tuple, T <: Tuple: TupleAsExprOrExplode.Of[TRes]]
      : AsExprOrExplode[NamedTuple[Fields, T], NamedTuple[Fields, TRes]] =
    inputTuple => {
      val nodes = tupleInstances(TupleAsExprOrExplode(inputTuple))
        .mapK([t] =>
          _ match {
            case expr: Expr[`t`] => Expr.unlift(expr)
            case explode: ExplodeExpr[`t`] => Expr.unlift(explode.asExpr)
          }
        )
        .toTuple
      Expr.lift(ExprNode.makeNamedTuple(nodes))
    }

}
