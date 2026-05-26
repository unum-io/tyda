package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.rewrite.Rule.ApplyOrder

trait ExprRule extends Rule[ExprNode]

object ExprRule {
  extension (rules: Seq[ActionRule | DatasetRule | ExprRule]) {
    def transform[T](ds: Dataset[T]): Dataset[T] =
      rules.foldLeft(ds)((ds, rule) =>
        rule match {
          case rule: DatasetRule => rule.applyOrder match {
              case ApplyOrder.TopDown => ds.transformDown([t] => node => Continue(rule(node)))
              case ApplyOrder.BottomUp => ds.transformUp([t] => node => Continue(rule(node)))
            }
          case rule: ExprRule => rule.applyOrder match {
              case ApplyOrder.TopDown => ds.transformDownExprs([t] => node => Continue(rule(node)))
              case ApplyOrder.BottomUp => ds.transformUpExprs([t] => node => Continue(rule(node)))
            }
          // ActionRule only need to be applied to actions, since actions are always root nodes
          case rule: ActionRule => ds
        }
      )
    def transform(action: Dataset.Action): Dataset.Action =
      rules.foldLeft(action)((action, rule) =>
        rule match {
          case rule: ActionRule => rule(action)
          case rule: DatasetRule => rule.applyOrder match {
              case ApplyOrder.TopDown => action.transformDown([t] => node => Continue(rule(node)))
              case ApplyOrder.BottomUp => action.transformUp([t] => node => Continue(rule(node)))
            }
          case rule: ExprRule => rule.applyOrder match {
              case ApplyOrder.TopDown => action.transformDownExprs([t] => node => Continue(rule(node)))
              case ApplyOrder.BottomUp => action.transformUpExprs([t] => node => Continue(rule(node)))
            }
        }
      )
  }
}
