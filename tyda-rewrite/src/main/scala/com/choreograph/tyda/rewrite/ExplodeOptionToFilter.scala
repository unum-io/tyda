package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.TreeApi.Continue

object ExplodeOptionToFilter extends DatasetRule {

  def unapply[T](ds: Dataset[T]): Option[Dataset[T]] =
    ds match {
      case Dataset.Select(input, compiled) =>
        val (maybeFilterNode, node) = compiled
          .expr
          .transformAccumulateDown(None: Option[ExprNode[Boolean]])([t] =>
            (acc, node) =>
              node match {
                case ExprNode.Explode(ExprNode.OptionToIterable(e)) =>
                  Continue(Some(acc.fold(!e.isEmpty)(_ && !e.isEmpty)), ExprNode.KnownNotNull(e))
                case _ => Continue(acc, node)
              }
          )
        maybeFilterNode.map(filterNode =>
          val filter = Dataset.Filter(input, CompiledExpr(compiled.arg, filterNode))
          Dataset.Select(filter, CompiledExplodeExpr(compiled.arg, node))
        )

      case _ => None
    }

  def apply[T](ds: Dataset[T]): Dataset[T] = unapply(ds).getOrElse(ds)
}
