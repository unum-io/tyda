package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Comparable
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Num

final case class CheckFloatingOverflow(checkFloat: Boolean, checkDouble: Boolean) extends ExprRule {
  import CheckFloatingOverflow.{FloatingOperation, doubleOverflowPredicate, floatOverflowPredicate}

  def unapply[T](node: ExprNode[T]): Option[FloatingOperation[T]] =
    node match {
      case ExprNode.Add(_, lhs, rhs) => checkOverflow(node, lhs, rhs)
      case ExprNode.Subtract(_, lhs, rhs) => checkOverflow(node, lhs, rhs)
      case ExprNode.Multiply(_, lhs, rhs) => checkOverflow(node, lhs, rhs)
      case ExprNode.Quotient(_, lhs, rhs) => checkOverflow(node, lhs, rhs)
      case _ => None
    }

  def apply[T](node: ExprNode[T]): ExprNode[T] = unapply(node).fold(node)(_.expr)

  private def checkOverflow[T](
      operation: ExprNode[T],
      lhs: ExprNode[T],
      rhs: ExprNode[T]
  ): Option[FloatingOperation[T]] =
    operation.codec match {
      case Codec.Float if checkFloat =>
        // TYPE SAFETY: Matching on the codec proves this node is ExprNode[Float].
        val operationFloat = operation.asInstanceOf[ExprNode[Float]]
        // TYPE SAFETY: lhs and operation have the same type, so lhs is also ExprNode[Float].
        val lhsFloat = lhs.asInstanceOf[ExprNode[Float]]
        // TYPE SAFETY: rhs and operation have the same type, so rhs is also ExprNode[Float].
        val rhsFloat = rhs.asInstanceOf[ExprNode[Float]]
        val floatingOperation =
          FloatingOperation(operationFloat, lhsFloat, rhsFloat, floatOverflowPredicate, "Float overflow")
        // TYPE SAFETY: Matching on the codec proves T is Float in this branch.
        Some(floatingOperation.asInstanceOf[FloatingOperation[T]])
      case Codec.Double if checkDouble =>
        // TYPE SAFETY: Matching on the codec proves this node is ExprNode[Double].
        val operationDouble = operation.asInstanceOf[ExprNode[Double]]
        // TYPE SAFETY: lhs and operation have the same type, so lhs is also ExprNode[Double].
        val lhsDouble = lhs.asInstanceOf[ExprNode[Double]]
        // TYPE SAFETY: rhs and operation have the same type, so rhs is also ExprNode[Double].
        val rhsDouble = rhs.asInstanceOf[ExprNode[Double]]
        val floatingOperation =
          FloatingOperation(operationDouble, lhsDouble, rhsDouble, doubleOverflowPredicate, "Double overflow")
        // TYPE SAFETY: Matching on the codec proves T is Double in this branch.
        Some(floatingOperation.asInstanceOf[FloatingOperation[T]])
      case _ => None
    }
}

object CheckFloatingOverflow {
  private[tyda] final case class OverflowPredicate[T](
      lhs: ExprNode.Reference[T],
      rhs: ExprNode.Reference[T],
      result: ExprNode.Reference[T],
      expr: ExprNode[Boolean]
  ) {
    def bind(lhs: ExprNode[T], rhs: ExprNode[T], result: ExprNode[T]): ExprNode[Boolean] =
      expr.replace(this.lhs, lhs).replace(this.rhs, rhs).replace(this.result, result)
  }

  private[tyda] final case class FloatingOperation[T](
      operation: ExprNode[T],
      lhs: ExprNode[T],
      rhs: ExprNode[T],
      overflow: OverflowPredicate[T],
      errorMessage: String
  ) {
    def expr: ExprNode[T] =
      ExprNode
        .Cases
        .ternary(
          overflow.bind(lhs, rhs, operation),
          ExprNode.RaiseError(ExprNode.Literal(errorMessage), operation.codec),
          operation
        )
  }

  val FloatOnly: CheckFloatingOverflow = CheckFloatingOverflow(checkFloat = true, checkDouble = false)
  val FloatAndDouble: CheckFloatingOverflow = CheckFloatingOverflow(checkFloat = true, checkDouble = true)

  private val floatOverflowPredicate =
    overflowPredicate(Float.MaxValue, Float.PositiveInfinity, ExprNode.IsNaN(_))
  private val doubleOverflowPredicate =
    overflowPredicate(Double.MaxValue, Double.PositiveInfinity, ExprNode.IsNaN(_))

  private def overflowPredicate[T: Codec: Num: Comparable](
      maxValue: T,
      positiveInfinity: T,
      isNaN: ExprNode.Reference[T] => ExprNode[Boolean]
  ): OverflowPredicate[T] = {
    val lhs = ExprNode.Reference[T]()
    val rhs = ExprNode.Reference[T]()
    val result = ExprNode.Reference[T]()
    val overflow = !isInfinite(lhs, positiveInfinity) && !isInfinite(rhs, positiveInfinity) &&
      !isNaN(result) && !(ExprNode.Abs(Num[T], result) <= ExprNode.Literal(maxValue))
    OverflowPredicate(lhs, rhs, result, overflow)
  }

  private def isInfinite[T: Codec: Num](expr: ExprNode[T], positiveInfinity: T): ExprNode[Boolean] =
    ExprNode.Equals(ExprNode.Abs(Num[T], expr), ExprNode.Literal(positiveInfinity))

}
