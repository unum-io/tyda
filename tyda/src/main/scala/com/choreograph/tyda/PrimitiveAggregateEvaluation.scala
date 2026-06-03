package com.choreograph.tyda

import scala.reflect.ClassTag

import com.choreograph.tyda.Aggregator.AndThen
import com.choreograph.tyda.Aggregator.Compose
import com.choreograph.tyda.Aggregator.Reduce

private[tyda] object PrimitiveAggregateEvaluation {
  def aggregator[From, To](agg: PrimitiveAggregate[From, To]): Aggregator[From, ?, To] =
    agg match {
      case PrimitiveAggregate.Collect() =>
        given Codec[To] = agg.codec
        Compose(Seq(_), Reduce[Seq[From]](_ ++ _))
      case PrimitiveAggregate.Count() => Compose(_ => 1L, Reduce[Long](_ + _))
      case PrimitiveAggregate.CountSome() => Compose(t => if t.isDefined then 1L else 0L, Reduce[Long](_ + _))
      case countDistinct: PrimitiveAggregate.CountDistinct[?] =>
        given Codec[From] = countDistinct.inputCodec
        given Codec[Set[From]] = Codec.iterable[From, Set[From]]
        AndThen(Compose(Set(_), Reduce[Set[From]](_ ++ _)), _.size.toLong)
      case PrimitiveAggregate.BoolAnd() => Reduce[Boolean](_ && _)
      case PrimitiveAggregate.BoolOr() => Reduce[Boolean](_ || _)
      case PrimitiveAggregate.Min(comparable) => Reduce(comparableToOrd(comparable).min)(using agg.codec)
      case PrimitiveAggregate.Max(comparable) => Reduce(comparableToOrd(comparable).max)(using agg.codec)
      case minBy: PrimitiveAggregate.MinBy[?, ?] =>
        minByAggregator(comparableToOrd(minBy.comparable), minBy.inputCodec)
      case maxBy: PrimitiveAggregate.MaxBy[?, ?] =>
        minByAggregator(comparableToOrd(maxBy.comparable).reverse, maxBy.inputCodec)
      case PrimitiveAggregate.Reduce(f) => Reduce(f)(using agg.codec)
      case PrimitiveAggregate.Sum(magnet) => Compose(magnet.toResult, Reduce(magnet.add)(using magnet.codec))
    }

  def minByAggregator[V, O](ord: Ord[O], codec: Codec[(V, O)]): Aggregator[(V, O), ?, V] =
    AndThen(Reduce[(V, O)]((a, b) => if ord.lt(a._2, b._2) then a else b)(using codec), _._1)

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
