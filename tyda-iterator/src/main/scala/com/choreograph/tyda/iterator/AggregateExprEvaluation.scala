package com.choreograph.tyda.iterator
import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.Aggregator
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.PrimitiveAggregateEvaluation
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.shapeless3extras.tupleInstances

object AggregateExprEvaluation {

  /** Create an aggregator that can be used to evaluate a
    * [[com.choreograph.tyda.AggregateExpr AggregateExpr]] on actual values of
    * From.
    */
  def lambda[From: Codec, To](f: Expr[From] => AggregateExpr[To]): Seq[From] => Option[To] = {
    val agg = aggregator(CompiledAggregateExpr(f))
    values => values.iterator.map(agg.reduce(agg.zero, _)).reduceOption(agg.merge).map(agg.finish)
  }

  private[tyda] def aggregator[From, To](
      compiled: CompiledAggregateExpr[From, To]
  ): Aggregator[From, ?, To] = {
    val aggregateCodecs = compiled
      .expr
      .fold(Vector.empty[Codec[?]])((acc, node) =>
        node match {
          case ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_) => Skip(acc)
          case ExprNode.Aggregate(_, agg) => Skip(acc :+ agg.codec)
          case _ => Continue(acc)
        }
      )
    /* TYPE SAFETY: aggregateCodecs is a Seq[Codec[?]] so it can be converted to a Tuple.Map[? <: Tuple,
     * Codec] */
    val aggCodec: Codec[Tuple] = Codec.tuple(tupleInstances(
      Tuple.fromArray(aggregateCodecs.toArray).asInstanceOf[Tuple.Map[Tuple, Codec]]
    ))
    val refOutput = ExprNode.Reference()(using aggCodec)
    val (aggs, exprWithoutAggregates) = compiled
      .expr
      .transformAccumulateDown(Vector.empty[ExprNode.Aggregate[?, ?]])([t] =>
        (aggs, node: ExprNode[t]) =>
          node match {
            case subquery @ (ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_)) => Skip(aggs, subquery)
            case agg @ ExprNode.Aggregate(_, _) =>
              Skip(aggs :+ agg, ExprNode.Select(refOutput, s"_${aggs.size + 1}"))
            case _ => Continue(aggs, node)
          }
      )
    val aggregators = aggs.map(aggregateNode =>
      Aggregator.Compose(
        ExprEvaluation.lambda(CompiledExpr(compiled.arg, aggregateNode.arg)),
        PrimitiveAggregateEvaluation.aggregator(aggregateNode.agg)
      )
    )
    val combined = Aggregator.Combined(IArray.from(aggregators))
    Aggregator.AndThen(combined, ExprEvaluation.lambda(CompiledExpr(refOutput, exprWithoutAggregates)))
  }
}
