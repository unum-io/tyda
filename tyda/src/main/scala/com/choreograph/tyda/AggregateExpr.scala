package com.choreograph.tyda

/** An expression that represents an aggregated value.
  *
  * This class is closely related to [[Expr]], and supports most of the same
  * operations. But contains some aggregate expression and can therefore only be
  * used in [[Dataset.aggregate]] and [[GroupedDataset.aggregate]].
  *
  * Use aggregate functions in [[aggregates]] to create instances from [[Expr]].
  */
final class AggregateExpr[T] private (private[tyda] val node: ExprNode[T]) extends Selectable {
  type Fields = NamedTuple.Map[NamedTuple.From[T], AggregateExpr]

  def selectDynamic(fieldName: String) = new AggregateExpr(ExprNode.Select(this.node, fieldName))

  def codec: Codec[T] = node.codec

  def ==(rhs: AggregateExpr[T]): AggregateExpr[Boolean] =
    new AggregateExpr(ExprNode.Equals(this.node, rhs.node))
  def ==(rhs: T): AggregateExpr[Boolean] =
    new AggregateExpr(ExprNode.Equals(this.node, ExprNode.Literal.create(rhs, codec)))

  final override def toString: String = node.simpleShow
}

object AggregateExpr extends ExprApi[AggregateExpr] {
  private[tyda] def lift[T](e: ExprNode[T]): AggregateExpr[T] = new AggregateExpr(e)
  private[tyda] def unlift[T](e: AggregateExpr[T]): ExprNode[T] = e.node
}
