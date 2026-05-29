package com.choreograph.tyda

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.Concat
import scala.NamedTuple.NamedTuple
import scala.compiletime.deferred

import com.choreograph.tyda.Expr.AsExpr

sealed trait GroupedDataset[N <: Tuple, KV <: Tuple, V: Codec](using Codec[NamedTuple[N, KV]]) {
  import GroupedDataset.CanMerge
  protected type Row
  final type K = NamedTuple[N, KV]
  protected def rows: Dataset[Row]
  protected given rowCodec: Codec[Row] = deferred
  protected given groupableKey: Groupable[KV] = deferred
  def keyCodec: Codec[K] = summon
  def valueCodec: Codec[V] = summon

  private[tyda] def value: Expr[Row] => Expr[V]
  private[tyda] def key: Expr[Row] => Expr[K]

  /** Create a new GroupedDataset by transforming each value using the given
    * expression.
    */
  def selectValues[U, I: AsExpr.Of[U]](f: Expr[V] => I): GroupedDataset[N, KV, U] = {
    val newValue = value.andThen(f).andThen(AsExpr(_))
    given Codec[U] = newValue(Expr.lift(ExprNode.Reference())).codec
    GroupedDataset.FromDataset(rows, key, newValue)
  }

  /** Create a new [[GroupedDataset]] by applying the provided function to each
    * value.
    *
    * Note: If performance is important consider using [[selectValues]] with the
    * expression api directly instead.
    */
  def mapValues[U: Codec](f: V => U): GroupedDataset[N, KV, U] = selectValues(_.udf(f))

  /** Aggregate the values using a named tuple of [[AggregateExpr]]s.
    *
    * [[AggregateExpr]]s can be created using the methods in the [[aggregates]].
    *
    * Example usage:
    * ```scala
    * import com.choreograph.tyda.aggregates.min
    * val ds: Dataset[(key: Int, v1: Int, v2: Int)] = ???
    * val agg = ds.groupByKey(_.key).aggregate(r => (min1 = min(r.v1), min2 = min(r.v2)))
    * ```
    */
  def aggregate[NR <: Tuple, RV <: Tuple, I <: AnyNamedTuple: AggregateExpr.AsExpr.Of[NamedTuple[NR, RV]]](
      e: Expr[V] => I
  )(using CanMerge[K, NamedTuple[NR, RV]]): Dataset[Concat[K, NamedTuple[NR, RV]]] = {
    val agg = CompiledAggregateExpr(value.andThen(e).andThen(AggregateExpr.AsExpr(_)))
    Dataset.GroupedAggregate(rows, CompiledExpr(key), agg).select { case Expr(keys, aggs) => keys ++ aggs }
  }

  /** Aggregate the values using a single [[AggregateExpr]].
    *
    * [[AggregateExpr]]s can be created using the methods in the [[aggregates]].
    *
    * Example usage:
    * ```scala
    * import com.choreograph.tyda.aggregates.min
    * val pairs: Dataset[(Int, Int)] = ???
    * val agg = pairs.groupByKey(_._1).aggregateValue(min(_._2))
    * ```
    */
  def aggregateValue[R, I: AggregateExpr.AsExpr.Of[R]](agg: Expr[V] => I)(using
      CanMerge[K, (value: V)]
  ): Dataset[Concat[K, (value: R)]] = aggregate(v => (value = agg(v)))

  /** Count the number of values in each group.
    */
  def count(using CanMerge[K, (count: Long)]): Dataset[Concat[K, (count: Long)]] =
    aggregate(_ => (count = aggregates.count()))

  /** Reduce all the values in each group using the provided binary function.
    *
    * This is convince shorthand for performing `aggregate(reduce(f))`.
    */
  def reduce(f: (V, V) => V)(using CanMerge[K, (value: V)]): Dataset[Concat[K, (value: V)]] =
    aggregateValue(com.choreograph.tyda.aggregates.reduce(f))
}

object GroupedDataset {
  type For[K <: AnyNamedTuple, V] = GroupedDataset[NamedTuple.Names[K], NamedTuple.DropNames[K], V]

  type CanMerge[NT1 <: AnyNamedTuple, NT2 <: AnyNamedTuple] =
    Tuple.Disjoint[NamedTuple.Names[NT1], NamedTuple.Names[NT2]] =:= true
  private[tyda] final case class FromDataset[T: Codec, N <: Tuple, K <: Tuple: Groupable, V: Codec](
      rows: Dataset[T],
      key: Expr[T] => Expr[NamedTuple[N, K]],
      value: Expr[T] => Expr[V]
  )(using Codec[NamedTuple[N, K]])
      extends GroupedDataset[N, K, V] {
    type Row = T
  }
}
