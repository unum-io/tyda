package com.choreograph.tyda

private sealed trait PrimitiveAggregate[T, R: Codec] {
  def codec: Codec[R] = summon
}

private object PrimitiveAggregate {
  final case class Collect[T: Codec]() extends PrimitiveAggregate[T, Seq[T]]
  final case class Count[T]() extends PrimitiveAggregate[T, Long]
  final case class CountSome[T]() extends PrimitiveAggregate[Option[T], Long]
  final case class BoolAnd() extends PrimitiveAggregate[Boolean, Boolean]
  final case class BoolOr() extends PrimitiveAggregate[Boolean, Boolean]
  final case class Min[T: Codec](comparable: Comparable[T]) extends PrimitiveAggregate[T, T]
  final case class MinBy[V: Codec, O: Codec](comparable: Comparable[O])
      extends PrimitiveAggregate[(V, O), V] {
    def inputCodec: Codec[(V, O)] = summon
  }
  final case class Max[T: Codec](comparable: Comparable[T]) extends PrimitiveAggregate[T, T]
  final case class MaxBy[V: Codec, O: Codec](comparable: Comparable[O])
      extends PrimitiveAggregate[(V, O), V] {
    def inputCodec: Codec[(V, O)] = summon
  }
  final case class Reduce[T: Codec](f: (T, T) => T) extends PrimitiveAggregate[T, T]
  final case class Sum[T, R: Codec](magnet: SumMagnet.Aux[T, R]) extends PrimitiveAggregate[T, R]
}
