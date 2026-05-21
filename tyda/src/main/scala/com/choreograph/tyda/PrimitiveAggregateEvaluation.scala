package com.choreograph.tyda

import com.choreograph.tyda.Aggregator.AndThen
import com.choreograph.tyda.Aggregator.Compose
import com.choreograph.tyda.Aggregator.Reduce

private[tyda] object PrimitiveAggregateEvaluation {
  def aggregator[From, To](agg: PrimitiveAggregate[From, To]): Aggregator[From, ?, To] =
    aggregatorAndIntermediateCodec(agg).aggregator

  final case class AggregatorAndIntermediateCodec[From, I1, To](
      aggregator: Aggregator[From, I1, To],
      codec: Codec[I1]
  )
  private def make[From, I: Codec, To](
      agg: Aggregator[From, I, To]
  ): AggregatorAndIntermediateCodec[From, I, To] = AggregatorAndIntermediateCodec(agg, Codec[I])

  def aggregatorAndIntermediateCodec[From, To](
      agg: PrimitiveAggregate[From, To]
  ): AggregatorAndIntermediateCodec[From, ?, To] =
    agg match {
      case PrimitiveAggregate.Collect() =>
        given Codec[To] = agg.codec
        make(Compose(Seq(_), Reduce[Seq[From]](_ ++ _)))
      case PrimitiveAggregate.Count() => make(Compose(_ => 1L, Reduce[Long](_ + _)))
      case PrimitiveAggregate.CountSome() =>
        make(Compose(t => if t.isDefined then 1L else 0L, Reduce[Long](_ + _)))
      case PrimitiveAggregate.BoolAnd() => make(Reduce[Boolean](_ && _))
      case PrimitiveAggregate.BoolOr() => make(Reduce[Boolean](_ || _))
      case PrimitiveAggregate.Min(comparable) =>
        given Codec[To] = agg.codec
        make(Reduce(comparableToOrd(comparable).min))
      case PrimitiveAggregate.Max(comparable) =>
        given Codec[To] = agg.codec
        make(Reduce(comparableToOrd(comparable).max))
      case minBy: PrimitiveAggregate.MinBy[?, ?] =>
        minByAggregator(comparableToOrd(minBy.comparable))(using minBy.inputCodec)
      case maxBy: PrimitiveAggregate.MaxBy[?, ?] =>
        minByAggregator(comparableToOrd(maxBy.comparable).reverse)(using maxBy.inputCodec)
      case PrimitiveAggregate.Reduce(f) =>
        given Codec[To] = agg.codec
        make(Reduce(f))
      case PrimitiveAggregate.Sum(magnet) =>
        given Codec[To] = magnet.codec
        make(Compose(magnet.toResult, Reduce(magnet.add)))
    }

  def minByAggregator[V, O](
      ord: Ord[O]
  )(using codec: Codec[(V, O)]): AggregatorAndIntermediateCodec[(V, O), ?, V] =
    make(AndThen(Reduce[(V, O)]((a, b) => if ord.lt(a._2, b._2) then a else b), _._1))

  def comparableToOrd[T](comparable: Comparable[T]): Ord[T] =
    comparable match {
      case Comparable.Boolean => Ord[Boolean]
      case Comparable.Byte => Ord[Byte]
      case Comparable.Short => Ord[Short]
      case Comparable.Int => Ord[Int]
      case Comparable.Long => Ord[Long]
      case Comparable.Float => Ord[Float]
      case Comparable.Double => Ord[Double]
      case Comparable.String => Ord[String]
      case Comparable.Date => Ord[Date]
      case Comparable.Timestamp => Ord[Timestamp]
      case Comparable.Duration => Ord[Duration]
      case _: Comparable.Decimal[s, p] => Ord[Decimal[s, p]]
    }
}
