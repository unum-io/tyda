package com.choreograph.tyda

import scala.NamedTuple.NamedTuple

import com.choreograph.tyda.shapeless3extras.mapConst

/** An expression that can be used as a key in [[Dataset.groupBy]].
  *
  * Every [[Expr]] is a GroupingExpr, but grouping only expressions such as
  * [[Expr.cube]] and [[Expr.rollup]] are not [[Expr]]s and can therefore only
  * be used as grouping keys.
  */
opaque type GroupingExpr[T] >: Expr[T] = Expr[T]

object GroupingExpr extends ExprApi[GroupingExpr] {
  private[tyda] def lift[T](e: ExprNode[T]): GroupingExpr[T] = Expr.lift(e)
  private[tyda] def unlift[T](e: GroupingExpr[T]): ExprNode[T] = Expr.unlift(e)

  given fromExpr[T]: AsExpr[Expr[T], T] = AsExpr.expr

  /** Create a GroupingExpr that groups `key` once for each grouping set.
    *
    * `sets` is given the number of fields in `key` and returns the grouping
    * sets as indices of the fields to group by.
    */
  private[tyda] def groupingSets[N <: Tuple, K <: Tuple](
      key: Expr[NamedTuple[N, K]],
      sets: Int => Seq[Seq[Int]]
  ): GroupingExpr[NamedTuple.Map[NamedTuple[N, K], Option]] =
    unlift(key).codec match {
      case Codec.Product(_, fields, _) =>
        val nullableFields = fields.mapConst([t] => f => f.copy(codec = Codec.Option(f.codec)))
        // TYPE SAFETY: The map above only adds Option to each field.
        val codec = Codec
          .unsafeNamedTuple(nullableFields)
          .asInstanceOf[Codec.Product[NamedTuple.Map[NamedTuple[N, K], Option]]]
        lift(ExprNode.GroupingSets(unlift(key), sets(fields.arity), codec))
      case unsupported => unreachable(s"NamedTuples must use Product codec, but found ${unsupported}")
    }
}
