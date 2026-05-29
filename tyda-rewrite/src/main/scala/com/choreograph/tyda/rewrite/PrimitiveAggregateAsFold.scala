package com.choreograph.tyda.rewrite
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.PrimitiveAggregate

/** Converts a `PrimitiveAggregate` into a fold-like structure consisting of an
  * initial value, a merge function, and a finish function.
  *
  * This is used to rewrite aggregates over arrays into a fold operation that
  * can be used with query engines like Spark that exposes reduce/aggregate
  * function over arrays.
  */
object PrimitiveAggregateAsFold {
  final case class Result[T, I, R](
      initial: ExprNode[I],
      merge: CompiledExpr2[I, T, I],
      finish: CompiledExpr[I, R]
  )

  private def result[T: Codec, I, R](
      initial: ExprNode[I],
      merge: (Expr[I], Expr[T]) => Expr[I],
      finish: Expr[I] => Expr[R]
  ): Result[T, I, R] = {
    given Codec[I] = initial.codec
    Result(initial, CompiledExpr2(merge), CompiledExpr(finish))
  }

  def apply[T: Codec, R](
      onEmpty: ExprNode[R],
      agg: PrimitiveAggregate[T, R] & ExprNode.AggregateSeq.SupportedAggregates
  ): Result[T, ?, R] = {
    import Expr.{&&, ||}
    agg match {
      case PrimitiveAggregate.BoolAnd() => result(onEmpty, _ && _, identity)
      case PrimitiveAggregate.BoolOr() => result(onEmpty, _ || _, identity)
    }
  }
}
