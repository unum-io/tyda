package com.choreograph.tyda

import com.choreograph.tyda.shapeless3extras.tupleInstances

/** Aggregator class used to evalaute an [[AggregateExpr]].
  *
  * This modelled on Spark's `Aggregator` class so that it can be easily used to
  * evaluate [[AggregateExpr]] for aggregates without builtin support in Spark.
  *
  * Note: That compared to spark we do provide an output Codec as that will be
  * taking from the [[AggregateExpr]] instead.
  */
private[tyda] sealed trait Aggregator[From, Intermediate, To] extends Serializable {
  def zero: Intermediate
  def reduce(b: Intermediate, a: From): Intermediate
  def merge(b1: Intermediate, b2: Intermediate): Intermediate
  def finish(reduction: Intermediate): To
  def intermediateCodec: Codec[Intermediate]
}

private[tyda] object Aggregator {
  final case class Compose[From, I1, I2, To](f: From => I1, agg: Aggregator[I1, I2, To])
      extends Aggregator[From, I2, To] {
    def zero: I2 = agg.zero
    def reduce(b: I2, a: From): I2 = agg.reduce(b, f(a))
    def merge(b1: I2, b2: I2): I2 = agg.merge(b1, b2)
    def finish(reduction: I2): To = agg.finish(reduction)
    def intermediateCodec: Codec[I2] = agg.intermediateCodec
  }

  final case class AndThen[From, I1, I2, To](agg: Aggregator[From, I1, I2], f: I2 => To)
      extends Aggregator[From, I1, To] {
    def zero: I1 = agg.zero
    def reduce(b: I1, a: From): I1 = agg.reduce(b, a)
    def merge(b1: I1, b2: I1): I1 = agg.merge(b1, b2)
    def finish(reduction: I1): To = f(agg.finish(reduction))
    def intermediateCodec: Codec[I1] = agg.intermediateCodec
  }

  final case class Reduce[T: Codec](r: (T, T) => T) extends Aggregator[T, Option[T], T] {
    def zero: Option[T] = None
    def reduce(b: Option[T], a: T): Option[T] = Some(b.fold(a)(r(_, a)))
    def merge(b1: Option[T], b2: Option[T]): Option[T] =
      (b1, b2) match {
        case (Some(a), Some(b)) => Some(r(a, b))
        case (Some(a), None) => Some(a)
        case (None, Some(b)) => Some(b)
        case (None, None) => None
      }
    def finish(reduction: Option[T]): T =
      reduction.getOrElse(unreachable("Aggregator.Reduce.finish called with empty reduction"))
    def intermediateCodec: Codec[Option[T]] = summon
  }

  final case class Combined[From, To <: Tuple](aggs: IArray[Aggregator[From, ?, ?]])
      extends Aggregator[From, Tuple, To] {
    def zero: Tuple = Tuple.fromIArray(aggs.map(_.zero))
    def reduce(b: Tuple, a: From): Tuple =
      // TYPE SAFETY: The tuple elements should match the aggregator intermediate types
      Tuple.fromIArray(
        aggs.zip(b.toIArray).map { case (agg, intermediate) => agg.reduce(intermediate.asInstanceOf, a) }
      )
    def merge(b1: Tuple, b2: Tuple): Tuple =
      Tuple.fromIArray(
        aggs
          .zip(b1.toIArray)
          .zip(b2.toIArray)
          .map { case ((agg, intermediate1), intermediate2) =>
            agg.merge(
              // TYPE SAFETY: The tuple elements should match the aggregator intermediate types
              intermediate1.asInstanceOf,
              // TYPE SAFETY: The tuple elements should match the aggregator intermediate types
              intermediate2.asInstanceOf
            )
          }
      )
    def finish(reduction: Tuple): To =
      // TYPE SAFETY: The tuple elements should match the aggregator intermediate types
      val results = aggs
        .zip(reduction.toIArray)
        .map { case (agg, intermediate) => agg.finish(intermediate.asInstanceOf) }
      // TYPE SAFETY: The results should match the output types of the aggregators
      Tuple.fromIArray(results).asInstanceOf[To]
    def intermediateCodec: Codec[Tuple] = {
      val codecs = aggs.map(_.intermediateCodec)
      // TYPE SAFETY: Each element in codecs is of type Codec[?]
      Codec.tuple(tupleInstances(Tuple.fromIArray(codecs).asInstanceOf[Tuple.Map[Tuple, Codec]]))
    }
  }
}
