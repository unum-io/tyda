package com.choreograph.tyda.rewrite

/** Specifies a rewrite rule for an node.
  *
  * The rule will be applied to each node in a tree according to the specified
  * apply order.
  */
trait Rule[N[_]] {
  def apply[T](node: N[T]): N[T]
  def applyOrder: Rule.ApplyOrder = Rule.ApplyOrder.BottomUp
}

object Rule {

  /** The order an Rule should be applied in. */
  enum ApplyOrder {
    case TopDown
    case BottomUp
  }
}
