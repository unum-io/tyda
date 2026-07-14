package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ExprNode.raiseError
import com.choreograph.tyda.ExprNode.ternary

/** Adds a check that throws an error if a negative index is used to access a
  * sequence.
  *
  * This is used with backends that support negative indexing natively to ensure
  * consistent behavior across all backends.
  */
object CheckArrayIndexPositive extends ExprRule {
  def unapply[T](node: ExprNode.ElementSeq[T]): Some[ExprNode.ElementSeq[T]] = {
    val checkedIndex = ternary(node.index < 0, raiseError[Int]("Array index is negative"), node.index)
    Some(node.copy(index = checkedIndex))
  }

  override def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case CheckArrayIndexPositive(checked) => checked
      case _ => node
    }
}
