package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances

object ExplodeOptionToFilter extends DatasetRule {

  private object ExplodeOption {
    def unapply[T, R](
        compilerOrExplode: CompiledExprOrExplode[T, R]
    ): Option[(select: CompiledExpr[T, R], filter: CompiledExpr[T, Boolean])] =
      compilerOrExplode match {
        case CompiledExplodeExpr(arg, ExprNode.OptionToIterable(e)) =>
          Some(CompiledExpr(arg, ExprNode.KnownNotNull(e)), CompiledExpr(arg, !e.isEmpty))
        case _ => None
      }
  }

  def unapply[T](ds: Dataset[T]): Option[Dataset[T]] =
    ds match {
      case Dataset.Select1(input, ExplodeOption(select, filter)) =>
        val filtered = Dataset.Filter(input, filter)
        Some(Dataset.Select1(filtered, select))

      case selectN: Dataset.SelectN[u, ?] =>
        val instances = tupleInstances(selectN.exprs)
        val maybeFilter = instances.foldLeft0[Option[CompiledExpr[u, Boolean]]](None)([t] =>
          (acc, expr) =>
            expr match {
              case ExplodeOption(_, filter) => Some(acc.fold(filter)(_ && filter))
              case _ => acc
            }
        )

        maybeFilter.map { filter =>
          val newSelects = instances.mapK([t] =>
            _ match {
              case ExplodeOption(select, _) => select
              case other => other
            }
          )
          val filtered = Dataset.Filter(selectN.input, filter)
          Dataset.SelectN(filtered, newSelects.toTuple)
        }
      case _ => None
    }

  def apply[T](ds: Dataset[T]): Dataset[T] = unapply(ds).getOrElse(ds)
}
