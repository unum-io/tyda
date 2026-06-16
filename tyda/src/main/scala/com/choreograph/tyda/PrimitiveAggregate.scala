package com.choreograph.tyda

import com.choreograph.tyda.shapeless3extras.tupleInstances

private sealed trait PrimitiveAggregate[T, R: Codec] {
  def codec: Codec[R] = summon
}

private object PrimitiveAggregate {
  final case class Collect[T](elementCodec: Codec[T])
      extends PrimitiveAggregate[T, Seq[T]](using Codec.Seq(elementCodec))
  def Collect[T: Codec](): Collect[T] = Collect(Codec[T])
  final case class Count[T]() extends PrimitiveAggregate[T, Long]
  final case class CountSome[T]() extends PrimitiveAggregate[Option[T], Long]
  final case class BoolAnd() extends PrimitiveAggregate[Boolean, Boolean]
  final case class BoolOr() extends PrimitiveAggregate[Boolean, Boolean]

  final case class Min[T](comparable: Comparable[T], elementCodec: Codec[T])
      extends PrimitiveAggregate[T, T](using elementCodec)
  def Min[T: Codec](comparable: Comparable[T]): Min[T] = Min(comparable, Codec[T])

  final case class MinBy[V, O](comparable: Comparable[O], valueCodec: Codec[V], orderingCodec: Codec[O])
      extends PrimitiveAggregate[(V, O), V](using valueCodec) {
    def inputCodec: Codec[(V, O)] = Codec.tuple(tupleInstances((valueCodec, orderingCodec)))
  }
  def MinBy[V: Codec, O: Codec](comparable: Comparable[O]): MinBy[V, O] =
    MinBy(comparable, Codec[V], Codec[O])

  final case class Max[T](comparable: Comparable[T], elementCodec: Codec[T])
      extends PrimitiveAggregate[T, T](using elementCodec)
  def Max[T: Codec](comparable: Comparable[T]): Max[T] = Max(comparable, Codec[T])

  final case class MaxBy[V, O](comparable: Comparable[O], valueCodec: Codec[V], orderingCodec: Codec[O])
      extends PrimitiveAggregate[(V, O), V](using valueCodec) {
    def inputCodec: Codec[(V, O)] = Codec.tuple(tupleInstances((valueCodec, orderingCodec)))
  }
  def MaxBy[V: Codec, O: Codec](comparable: Comparable[O]): MaxBy[V, O] =
    MaxBy(comparable, Codec[V], Codec[O])

  final case class Reduce[T](f: (T, T) => T, elementCodec: Codec[T])
      extends PrimitiveAggregate[T, T](using elementCodec)
  def Reduce[T: Codec](f: (T, T) => T): Reduce[T] = Reduce(f, Codec[T])

  final case class Sum[T, R](magnet: SumMagnet.Aux[T, R], resultCodec: Codec[R])
      extends PrimitiveAggregate[T, R](using resultCodec)
  def Sum[T, R: Codec](magnet: SumMagnet.Aux[T, R]): Sum[T, R] = Sum(magnet, Codec[R])
}
