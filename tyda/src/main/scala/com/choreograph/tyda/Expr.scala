package com.choreograph.tyda

/** A typed expression using in the Dataset API.
  *
  * This leverages NamedTuple to provide type safe access to fields when `T` is
  * a product type.
  */
final class Expr[T] private (private[tyda] val node: ExprNode[T]) extends Selectable {
  type Fields = NamedTuple.Map[NamedTuple.From[T], Expr]

  def selectDynamic(fieldName: String) = new Expr(node.selectDynamic(fieldName))

  def codec: Codec[T] = node.codec

  def ==(rhs: Expr[T]): Expr[Boolean] = new Expr(ExprNode.Equals(this.node, rhs.node))
  def ==(rhs: T): Expr[Boolean] = new Expr(ExprNode.Equals(this.node, ExprNode.Literal.create(rhs, codec)))
  def !=(rhs: Expr[T]): Expr[Boolean] = !(this == rhs)
  def !=(rhs: T): Expr[Boolean] = !(this == rhs)
  override def equals(rhs: Any): Boolean =
    throw new RuntimeException(
      "Expr was compared using universal equals. " + "This is unlikely to be what was intended. " +
        "Please check the types being compared and make sure they match either Expr[T] == Expr[T] or Expr[T] == T"
    )

  final override def toString: String = node.simpleShow
}

object Expr extends ExprApi[Expr] {
  private[tyda] def lift[T](e: ExprNode[T]): Expr[T] = new Expr(e)
  private[tyda] def unlift[T](e: Expr[T]): ExprNode[T] = e.node
  private[tyda] def knownNotNull[T](e: Expr[Option[T]]): Expr[T] = lift(ExprNode.KnownNotNull(unlift(e)))

  /** Create a new row for each element in a Option, Map or Iterable column.
    *
    * This can be used inside a select, for example:
    * ```scala
    * import com.choreograph.tyda.functions.explode
    * val ds: Dataset[Seq[Int]] = ???
    * ds.select(explode)
    * ```
    */
  def explode[E, I: AsExpr.Of[Iterable[E]]](expr: I): ExplodeExpr[E] =
    ExplodeExpr(ExprNode.Explode(unlift(AsExpr((expr)))))

  /** Provided for backwards compatibility with the syntax
    *
    * ```scala
    * import com.choreograph.tyda.functions.explode
    * val ds: Dataset[(Seq[Int], Long)] = ???
    * ds.select(explode(_._1), _._2)
    * ```
    */
  def explode[T, E, I: AsExpr.Of[Iterable[E]]](f: Expr[T] => I): Expr[T] => ExplodeExpr[E] =
    f.andThen(explode)
}
