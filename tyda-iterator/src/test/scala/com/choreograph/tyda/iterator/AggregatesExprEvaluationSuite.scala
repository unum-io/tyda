package com.choreograph.tyda.iterator

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.AggregateExpr.tuple
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Decimal.MaxPrecision
import com.choreograph.tyda.Expr
import com.choreograph.tyda.aggregates.collect
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.aggregates.countIf
import com.choreograph.tyda.aggregates.countSome
import com.choreograph.tyda.aggregates.max
import com.choreograph.tyda.aggregates.maxBy
import com.choreograph.tyda.aggregates.min
import com.choreograph.tyda.aggregates.minBy
import com.choreograph.tyda.aggregates.reduce
import com.choreograph.tyda.aggregates.sum

class AggregatesExprEvaluationSuite extends AnyFunSuite {
  def test[T: Codec, R](data: Seq[T], expected: R)(agg: Expr[T] => AggregateExpr[R]): Unit = {
    val compiled = CompiledAggregateExpr(agg)
    test(s"test aggregate expr ${compiled.expr}") {
      val aggregator = AggregateExprEvaluation.aggregator[T, R](compiled)
      def reduce(data: Seq[T]) = data.foldLeft(aggregator.zero)(aggregator.reduce)
      val (data1, data2) = data.splitAt(data.length / 2)
      val result = aggregator.finish(aggregator.merge(reduce(data1), reduce(data2)))
      assert(result == expected)
    }
  }

  test(Seq(1, 5, 10), Seq(1, 5, 10))(collect)
  test(Seq(1, 5, 10).map(Tuple1(_)), Seq(1, 5, 10))(collect(_._1))

  test((0 to 10).toSeq, 0)(min)
  test((0 to 10).toSeq, 0)(e => min(e)) // Verify no type hint is needed here
  test((0 to 10).toSeq.map(i => Tuple1(i)), 0)(min(_._1))
  test((0 to 10).toSeq, 10)(max)
  test((0 to 10).toSeq.map(i => Tuple1(i)), 10)(max(_._1))

  test(Seq((1, 5), (2, 3), (4, 2)), 4)(minBy(_._1, _._2))
  test(Seq((1, 5), (2, 3), (4, 2)), 1)(maxBy(_._1, _._2))

  test(Seq(1, 5, 10), 3L)(count)
  test(Seq(true, false, true, true), 3L)(countIf)
  test(Seq(true, false, true, true).map(Tuple1(_)), 3L)(countIf(_._1))
  test(Seq(1, 5, 10, -3, 8), 4L)(countIf(_ > 0))
  test(Seq(Some(1), None, Some(10)), 2L)(countSome)
  test(Seq(Some(1), None, Some(10)).map(Tuple1(_)), 2L)(countSome(_._1))
  test((0 to 10).toSeq, 55)(reduce(_ + _))
  test((0 to 10).toSeq, 65)(reduce((a, b) => a + b + 1))

  test((0 to 10).map(_.toByte), 55L)(sum)
  test((0 to 10).map(_.toShort), 55L)(sum)
  test((0 to 10), 55L)(sum)
  test((0 to 10).map(_.toLong), 55L)(sum)
  test((0 to 10).map(i => Tuple1(i)), 55L)(sum(_._1))
  test((0 to 10).map(_.toFloat), 55.0)(sum)
  test((0 to 10).map(_.toDouble), 55.0)(sum)
  test((0 to 10).map(_.toDouble), 55.0)(e => sum(e)) // Verify no type hint is needed here
  test(Seq[Option[Int]](None, None), None)(sum)
  test(Seq(None, Some(1)), Some(1L))(sum)
  test((0 to 10).map(Option(_)), Option(55L))(sum)
  test((0 to 10).map(Decimal[MaxPrecision, 0](_)), Decimal[MaxPrecision, 0](55L))(sum)

  test((0 to 10).map(_.toDouble), expected = true)(i => min(i) < max(i))
  test((0 to 10).map(_.toDouble), Tuple1(0.0))(i => tuple(Tuple1(min(i))))
  test((0 to 10).map(_.toDouble), (0.0, 10.0))(i => tuple(min(i), max(i)))
  test((0 to 10).map(_.toDouble), (0.0, 10.0, 55.0))(i => tuple(min(i), max(i), sum(i)))
}
