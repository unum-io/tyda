package com.choreograph.tyda.rewrite

import scala.NamedTuple.AnyNamedTuple

import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.PrimitiveAggregate
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.TreeApi.Stop
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.functions.namedTuple

/** Rewrites `COUNT(DISTINCT expr)` where `expr` is a complex type (collection,
  * struct) into an equivalent grouped subquery, for engines that do not support
  * `DISTINCT` on such types (e.g. BigQuery).
  *
  * Each unsupported aggregate is rewritten as:
  * {{{
  * SELECT key, COUNT(*) FROM (SELECT DISTINCT key, expr FROM input) GROUP BY key
  * }}}
  *
  * Supported aggregates are evaluated in a separate subquery and the results
  * are joined back together before the final expression is reconstructed.
  */
object RemoveDistinctAggregatesOnArraysAndStructs extends DatasetRule {

  private final case class RewriteCandidate[A, T, R](
      aggRef: ExprNode.Reference[A],
      agg: ExprNode.Aggregate[T, R],
      replacement: CompiledAggregateExpr[T, R]
  ) {
    def compute[K <: AnyNamedTuple](
        input: Dataset[A],
        key: CompiledExpr[A, K]
    ): AggregatePart[K, (value: R)] = {
      val distinctInput: Dataset[(K, T)] =
        Dataset.Distinct(Dataset.Select1(input, key.combine(CompiledExpr(aggRef, agg.arg))))
      given Codec[K] = key.codec
      given Codec[T] = agg.arg.codec
      given Codec[R] = agg.codec
      val wrapped = replacement.andThen(CompiledExpr(v => namedTuple((value = v))))
      val ds =
        Dataset.GroupedAggregate(distinctInput, CompiledExpr(_._1), wrapped.compose(CompiledExpr(_._2)))
      AggregatePart(ds, Seq(Replacement(agg, CompiledExpr[(value: R), R](_.value))))
    }
  }

  private object UnsupportedDistinctInput {
    def unapply[T](agg: ExprNode[T]): Boolean =
      agg.codec match {
        case CollectionCodec(_) => true
        case StructCodec() => true
        case _ => false
      }
  }
  private object UnsupportedAggregate {
    def unapply[T, R](agg: ExprNode.Aggregate[T, R]): Option[CompiledAggregateExpr[T, R]] =
      agg match {
        case ExprNode.Aggregate(arg @ UnsupportedDistinctInput(), PrimitiveAggregate.CountDistinct()) =>
          Some(CompiledAggregateExpr[T, R](count(_))(using arg.codec))
        case _ => None
      }
  }

  /* Hold an unsupported aggregate `agg` and `extractor` that camptures how to extract the same result from
   * the output after the rewrite. */
  private final case class Replacement[V, T, R](
      agg: ExprNode.Aggregate[T, R],
      extractor: CompiledExpr[V, R]
  ) {
    def compose[A: Codec](expr: Expr[A] => Expr[V]): Replacement[A, T, R] =
      Replacement(agg, extractor.compose(CompiledExpr(expr)))
  }

  private final case class AggregatePart[K, V](ds: Dataset[(K, V)], replacements: Seq[Replacement[V, ?, ?]]) {
    def join[U](other: AggregatePart[K, U]): AggregatePart[K, (V, U)] = {
      val joined = ds.join(other.ds)
      given Codec[U] = other.ds.codec.elements._2
      given Codec[V] = ds.codec.elements._2
      AggregatePart(
        joined,
        replacements.map(_.compose[(V, U)](_._1)) ++ other.replacements.map(_.compose[(V, U)](_._2))
      )
    }

    def finish[U](originalExpr: CompiledAggregateExpr[?, U]): Dataset[(K, U)] = {
      val resultRef = ExprNode.Reference()(using ds.codec)
      val replaced = replacements
        .map(_.compose[(K, V)](_._2)(using ds.codec))
        .foldLeft(originalExpr.expr) { case (expr, Replacement(agg, extractor)) =>
          expr.replace(agg, extractor.expr.replace(extractor.arg, resultRef))
        }
      val key = CompiledExpr[(K, V), K](_._1)(using ds.codec)
      val value = CompiledExpr(resultRef, replaced)
      Dataset.Select1(ds, key.combine(value))
    }
  }

  private def computeSupportedPart[A, K <: AnyNamedTuple, R <: AnyNamedTuple](
      input: Dataset[A],
      key: CompiledExpr[A, K],
      aggregate: CompiledAggregateExpr[A, R],
      supported: Seq[ExprNode.Aggregate[?, ?]]
  ): AggregatePart[K, ?] = {
    type Result <: AnyNamedTuple

    val fields = supported.zipWithIndex.map(_._2).map(i => s"result${i}")
    /* TYPE SAFETY: We construct the output so that supportedCompiledAggregate and initialReplacements have
     * compatible types. */
    val supportedCompiledAggregate: CompiledAggregateExpr[A, Result] =
      CompiledAggregateExpr(aggregate.arg, ExprNode.makeNamedTupleUnsafe(supported, fields).asInstanceOf)
    val supportedComputed = Dataset.GroupedAggregate(input, key, supportedCompiledAggregate)
    val ref = ExprNode.Reference()(using supportedCompiledAggregate.expr.codec)
    val initialReplacements: Seq[Replacement[Result, ?, ?]] = supported
      .zip(fields)
      .map((agg, name) => Replacement(agg, CompiledExpr(ref, ExprNode.Select(ref, name))))
    AggregatePart(supportedComputed, initialReplacements)
  }

  def rewriteGrouped[V, K <: AnyNamedTuple, R <: AnyNamedTuple](
      groupedAggregate: Dataset.GroupedAggregate[V, K, R]
  ): Option[Dataset[(K, R)]] = {
    val input = groupedAggregate.input
    val key = groupedAggregate.key
    val aggregate = groupedAggregate.agg
    val (unsupported, supported) = aggregate
      .expr
      .collect { case node @ ExprNode.Aggregate(_, _) => node }
      .partitionMap {
        case agg @ UnsupportedAggregate(replacementAgg) =>
          Left(RewriteCandidate(aggregate.arg, agg, replacementAgg))
        case other => Right(other)
      }
    if unsupported.isEmpty then return None

    val parts = NonEmpty.from(supported).map(computeSupportedPart(input, key, aggregate, _)) ++
      unsupported.map(_.compute(input, key))
    Some(parts.reduce(_.join(_)).finish(aggregate))
  }

  def rewriteAggregate[V, R](aggregate: Dataset.Aggregate[V, R]): Option[Dataset[Option[R]]] = {
    val input = aggregate.input
    val agg = aggregate.agg
    val hasUnsupported = agg
      .expr
      .exists {
        case UnsupportedAggregate(_) => true
        case _ => false
      }
    if !hasUnsupported then return None

    val nbrAggregates = agg
      .expr
      .count {
        case ExprNode.Aggregate(_, _) => true
        case _ => false
      }

    def computeDistinctInput[A, B](unsupported: ExprNode.Aggregate[A, B]): Dataset[A] = {
      val extractArg = {
        val ref = ExprNode.Reference()(using agg.arg.codec)
        CompiledExpr(ref, unsupported.arg.replace(agg.arg, ref))
      }
      Dataset.Distinct(Dataset.Select1(input, extractArg))
    }

    if nbrAggregates == 1 then {
      /* Acc case class is only use to make sure the compiler tracks that the distinctInput and newRef has the
       * same type parameter. */
      final case class Acc[T](distinctInput: Dataset[T], newRef: ExprNode.Reference[T])
      val (Some(acc), newExpr) = agg
        .expr
        .transformAccumulateDown[Option[Acc[?]]](None)([t] =>
          (_, node) =>
            node match {
              case unsupported @ UnsupportedAggregate(replacement) =>
                val newRef = ExprNode.Reference()(using unsupported.arg.codec)
                Stop(
                  Some(Acc(computeDistinctInput(unsupported), newRef)),
                  replacement.expr.replace(replacement.arg, newRef)
                )
              case other => Continue((None, other))
            }
        ): @unchecked // Safe since we already checked that there is exactly one unssupported aggregate
      Some(Dataset.Aggregate(acc.distinctInput, CompiledAggregateExpr(acc.newRef, newExpr)))
    } else {

      val exprWithSubqueries = agg
        .expr
        .transformDown([t] =>
          _ match {
            case unsupported @ UnsupportedAggregate(replacement) =>
              val computed = Dataset.Aggregate(computeDistinctInput(unsupported), replacement)
              Skip(ExprNode.ScalarSubquery(computed).get)
            case other => Continue(other)
          }
        )
      Some(Dataset.Aggregate(input, CompiledAggregateExpr(agg.arg, exprWithSubqueries)))
    }
  }

  def apply[T](dataset: Dataset[T]): Dataset[T] =
    dataset match {
      case ds @ Dataset.GroupedAggregate(_, _, _) => rewriteGrouped(ds).getOrElse(dataset)
      case ds @ Dataset.Aggregate(_, _) => rewriteAggregate(ds).getOrElse(dataset)
      case _ => dataset
    }
}
