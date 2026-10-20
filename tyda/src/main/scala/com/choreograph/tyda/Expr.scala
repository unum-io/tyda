package com.choreograph.tyda

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple

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
    * ds.select(explode(identity))
    * ```
    */
  def explode[T, E, I: AsExpr.Of[Iterable[E]]](f: Expr[T] => I): Expr[T] => ExplodeExpr[E] =
    expr => ExplodeExpr(unlift(AsExpr((f(expr)))))

  /** Group by every subset of the fields in `key`, like SQL's `GROUP BY
    * CUBE(...)`.
    *
    * Fields that are not part of a grouping set are None in the result. This
    * can only be used as a key in [[Dataset.groupBy]], where it can be combined
    * with other keys using `++`. For example:
    * ```scala
    * import com.choreograph.tyda.aggregates.sum
    * import com.choreograph.tyda.functions.cube
    * val ds: Dataset[(a: Int, b: Int, v: Long)] = ???
    * // Groups by (a, b), (a), (b) and ()
    * ds.groupBy(r => cube((a = r.a, b = r.b))).aggregate(r => (v = sum(r.v)))
    * ```
    */
  def cube[N <: Tuple, K <: Tuple, I <: AnyNamedTuple: AsExpr.Of[NamedTuple[N, K]]](
      key: I
  ): GroupingExpr[NamedTuple.Map[NamedTuple[N, K], Option]] =
    GroupingExpr.groupingSets(AsExpr(key), n => (n to 0 by -1).flatMap((0 until n).combinations))

  /** Group by every prefix of the fields in `key`, like SQL's `GROUP BY
    * ROLLUP(...)`.
    *
    * Fields that are not part of a grouping set are None in the result. This
    * can only be used as a key in [[Dataset.groupBy]], where it can be combined
    * with other keys using `++`. For example:
    * ```scala
    * import com.choreograph.tyda.aggregates.sum
    * import com.choreograph.tyda.functions.rollup
    * val ds: Dataset[(a: Int, b: Int, v: Long)] = ???
    * // Groups by (a, b), (a) and ()
    * ds.groupBy(r => rollup((a = r.a, b = r.b))).aggregate(r => (v = sum(r.v)))
    * ```
    */
  def rollup[N <: Tuple, K <: Tuple, I <: AnyNamedTuple: AsExpr.Of[NamedTuple[N, K]]](
      key: I
  ): GroupingExpr[NamedTuple.Map[NamedTuple[N, K], Option]] =
    GroupingExpr.groupingSets(AsExpr(key), n => (n to 0 by -1).map(0 until _))
}
