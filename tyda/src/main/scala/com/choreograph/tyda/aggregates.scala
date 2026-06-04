package com.choreograph.tyda

import scala.annotation.unused

import com.choreograph.tyda.Expr.AsExpr
import com.choreograph.tyda.PrimitiveAggregate.BoolAnd
import com.choreograph.tyda.PrimitiveAggregate.BoolOr
import com.choreograph.tyda.PrimitiveAggregate.Collect
import com.choreograph.tyda.PrimitiveAggregate.Count
import com.choreograph.tyda.PrimitiveAggregate.CountSome
import com.choreograph.tyda.PrimitiveAggregate.Max
import com.choreograph.tyda.PrimitiveAggregate.MaxBy
import com.choreograph.tyda.PrimitiveAggregate.Min
import com.choreograph.tyda.PrimitiveAggregate.MinBy
import com.choreograph.tyda.PrimitiveAggregate.Reduce
import com.choreograph.tyda.PrimitiveAggregate.Sum
import com.choreograph.tyda.functions.tuple

object aggregates {
  private def aggregate[T, R](node: Expr[T], agg: PrimitiveAggregate[T, R]): AggregateExpr[R] =
    AggregateExpr.lift(ExprNode.Aggregate(Expr.unlift(node), agg))

  /** AggregateExpr collecting all values into a [[Seq]].
    */
  def collect[T]: Expr[T] => AggregateExpr[Seq[T]] = e => aggregate(e, Collect()(using e.codec))

  /** AggregateExpr collecting all values of specified [[Expr]] into a [[Seq]].
    */
  def collect[T, R, I: Expr.AsExpr.Of[R]](f: Expr[T] => I): Expr[T] => AggregateExpr[Seq[R]] =
    collect.compose(f.andThen(AsExpr(_)))

  /** AggregateExpr counting the number of rows.
    *
    * Note: This is effectivly a `count(1)` in SQL and will count all rows,
    * including those with null values. Use [[countSome]] to count only non-null
    */
  def count[T](
      @unused
      e: Expr[T]
  ): AggregateExpr[Long] = aggregate(Expr.lit(1), Count())

  /** AggregateExpr counting the number of rows.
    *
    * Note: This is effectivly a `count(1)` in SQL and will count all rows,
    * including those with null values. Use [[countSome]] to count only non-null
    */
  def count[T](): AggregateExpr[Long] = aggregate(Expr.lit(1), Count())

  /** AggregateExpr counting the number of [[Some]] rows.
    */
  def countSome[T](e: Expr[Option[T]]): AggregateExpr[Long] = aggregate(e, CountSome())

  /** AggregateExpr counting the number of [[Some]] rows of specified [[Expr]].
    */
  def countSome[T, R, I: AsExpr.Of[Option[R]]](f: Expr[T] => I): Expr[T] => AggregateExpr[Long] =
    f.andThen(AsExpr(_)).andThen(countSome)

  /** AggregateExpr counting the number of rows where the predicate is true.
    */
  def countIf[T](e: Expr[Boolean]): AggregateExpr[Long] = countSome(Expr.when(e, 1))

  /** AggregateExpr counting the number of rows where the specified [[Expr]]
    * predicate is true.
    */
  def countIf[T](f: Expr[T] => Expr[Boolean]): Expr[T] => AggregateExpr[Long] = f.andThen(countIf)

  /** AggregateExpr returning true if all values are true.
    */
  def boolAnd(e: Expr[Boolean]): AggregateExpr[Boolean] = aggregate(e, BoolAnd())

  /** AggregateExpr returning true if all values of specified [[Expr]] are true.
    */
  def boolAnd[T](f: Expr[T] => Expr[Boolean]): Expr[T] => AggregateExpr[Boolean] = f.andThen(boolAnd)

  /** AggregateExpr returning true if any value is true.
    */
  def boolOr(e: Expr[Boolean]): AggregateExpr[Boolean] = aggregate(e, BoolOr())

  /** AggregateExpr returning true if any value of specified [[Expr]] is true.
    */
  def boolOr[T](f: Expr[T] => Expr[Boolean]): Expr[T] => AggregateExpr[Boolean] = f.andThen(boolOr)

  /** AggregateExpr returning the minium value using the given [[Comparable]].
    */
  def min[T: Comparable](e: Expr[T]): AggregateExpr[T] = aggregate(e, Min[T](summon)(using e.codec))

  /** AggregateExpr returning the minium of specified [[Expr]] value using the
    * given [[Comparable]].
    */
  def min[T, R, I: AsExpr.Of[R]](f: Expr[T] => I)(using Comparable[R]): Expr[T] => AggregateExpr[R] =
    f.andThen(AsExpr(_)).andThen(min)

  /** AggregateExpr returning the maximum value using the given [[Comparable]].
    */
  def max[T: Comparable](e: Expr[T]): AggregateExpr[T] = aggregate(e, Max[T](summon)(using e.codec))

  /** AggregateExpr returning the maximum of specified [[Expr]] value using the
    * given [[Comparable]].
    */
  def max[T, R, I: AsExpr.Of[R]](f: Expr[T] => I)(using Comparable[R]): Expr[T] => AggregateExpr[R] =
    f.andThen(AsExpr(_)).andThen(max)

  /** AggregateExpr returning minimum value of the first [[Expr]] when ordered
    * by the second [[Expr]].
    */
  def minBy[V, O, I1: AsExpr.Of[V], I2: AsExpr.Of[O]](value: I1, by: I2)(using
      Comparable[O]
  ): AggregateExpr[V] = {
    val valueExpr = AsExpr[I1, V](value)
    val byExpr = AsExpr[I2, O](by)
    given Codec[V] = valueExpr.codec
    given Codec[O] = byExpr.codec
    aggregate(tuple((valueExpr, byExpr)), MinBy[V, O](Comparable[O]))
  }

  /** AggregateExpr returning minimum value of the first [[Expr]] when ordered
    * by the second [[Expr]].
    */
  def minBy[T, V, O, I1: AsExpr.Of[V], I2: AsExpr.Of[O]](value: Expr[T] => I1, by: Expr[T] => I2)(using
      Comparable[O]
  ): Expr[T] => AggregateExpr[V] = e => minBy(value(e), by(e))

  /** AggregateExpr returning maximum value of the first [[Expr]] when ordered
    * by the second [[Expr]].
    */
  def maxBy[V, O, I1: AsExpr.Of[V], I2: AsExpr.Of[O]](value: I1, by: I2)(using
      Comparable[O]
  ): AggregateExpr[V] = {
    val valueExpr = AsExpr[I1, V](value)
    val byExpr = AsExpr[I2, O](by)
    given Codec[V] = valueExpr.codec
    given Codec[O] = byExpr.codec
    aggregate(tuple((valueExpr, byExpr)), MaxBy[V, O](Comparable[O]))
  }

  /** AggregateExpr returning maximum value of the first [[Expr]] when ordered
    * by the second [[Expr]].
    */
  def maxBy[T, V, O, I1: AsExpr.Of[V], I2: AsExpr.Of[O]](value: Expr[T] => I1, by: Expr[T] => I2)(using
      Comparable[O]
  ): Expr[T] => AggregateExpr[V] = e => maxBy(value(e), by(e))

  /** Aggregate from a binary function.
    */
  def reduce[T](f: (T, T) => T): Expr[T] => AggregateExpr[T] = e => aggregate(e, Reduce(f)(using e.codec))

  /** AggregateExpr returning the sum of the values.
    *
    * For all details on the sum aggregation, see [[sum]].
    */
  def sum[T: SumMagnet as magnet](e: Expr[T]): AggregateExpr[magnet.Result] =
    aggregate(e, Sum(summon)(using magnet.codec))

  /** AggregateExpr returning the sum of specified [[Expr]].
    *
    * For built-in integral types Byte, Short, Int, Long, the return type is
    * Long. For floating point types Float, Double, the return type is Double.
    *
    * Also support Option[T] for the same types, then the return type is also
    * Option[R] for the corresponding R. If all values are None, the result is
    * None otherwise the sum of all non-None values. Note, that this is not the
    * same as summing with nulls treated as zero, as that would produce 0
    * instead of null when all inputs are null.
    */
  def sum[T, R](f: Expr[T] => Expr[R])(using magnet: SumMagnet[R]): Expr[T] => AggregateExpr[magnet.Result] =
    f.andThen(AsExpr(_)).andThen(sum)
}
