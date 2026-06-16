package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.expressions.Aggregator as SparkAggregator
import org.apache.spark.sql.functions.bool_and
import org.apache.spark.sql.functions.bool_or
import org.apache.spark.sql.functions.collect_list
import org.apache.spark.sql.functions.count
import org.apache.spark.sql.functions.lit
import org.apache.spark.sql.functions.max
import org.apache.spark.sql.functions.max_by
import org.apache.spark.sql.functions.min
import org.apache.spark.sql.functions.min_by
import org.apache.spark.sql.functions.sum
import org.apache.spark.sql.functions.udaf

import com.choreograph.tyda.Aggregator
import com.choreograph.tyda.Codec
import com.choreograph.tyda.PrimitiveAggregate
import com.choreograph.tyda.PrimitiveAggregateEvaluation
import com.choreograph.tyda.SumMagnet
import com.choreograph.tyda.spark.CodecToEncoder.convert

private[spark] object PrimitiveAggregateOnSpark {
  def resolved[T, R](cf: ColumnFactory[T], agg: PrimitiveAggregate[T, R]): Column =
    agg match {
      // Sparks collect_list will filter out nulls, so can not be used for Option
      case PrimitiveAggregate.Collect(elementCodec) if !elementCodec.isInstanceOf[Codec.Option[?]] =>
        collect_list(cf.row)
      case PrimitiveAggregate.Count() => count(lit(1))
      case PrimitiveAggregate.CountSome() => count(cf.row)
      case PrimitiveAggregate.BoolAnd() => bool_and(cf.row)
      case PrimitiveAggregate.BoolOr() => bool_or(cf.row)
      case PrimitiveAggregate.Max(_, _) => max(cf.row)
      case PrimitiveAggregate.Min(_, _) => min(cf.row)
      case PrimitiveAggregate.MaxBy(_, _, _) =>
        assert(cf.columns.size == 2, "MaxBy requires exactly two columns but found: " + cf.columns.size)
        max_by(cf.column("_1"), cf.column("_2"))
      case PrimitiveAggregate.MinBy(_, _, _) =>
        assert(cf.columns.size == 2, "MinBy requires exactly two columns but found: " + cf.columns.size)
        min_by(cf.column("_1"), cf.column("_2"))
      case PrimitiveAggregate.Sum(CompatibleSum(), _) => sum(cf.row)
      // List PrimitiveAggregates cases explicitly to get exhaustive error when adding new aggregates
      case PrimitiveAggregate.Collect(_) => fallback(agg, cf)
      case PrimitiveAggregate.Reduce(_, _) => fallback(agg, cf)
      case PrimitiveAggregate.Sum(_, _) => fallback(agg, cf)
    }

  private def fallback[T, R](expr: PrimitiveAggregate[T, R], cf: ColumnFactory[T]): Column = {
    val aggregatorAndCodec = PrimitiveAggregateEvaluation.aggregatorAndIntermediateCodec[T, R](expr)
    val aggregator = aggregatorAndCodec.aggregator
    val intermediateCodec = aggregatorAndCodec.codec
    // Due to SPARK-52023 returning an Option from a udaf can cause data corruption/crashes.
    // To avoid this we instead wrap the output in a Tuple1 and then extract the Option afterwards.
    val outputNeedsWorkaround = expr.codec.isInstanceOf[Codec.Option[?]]
    val sparkAggregator =
      if outputNeedsWorkaround then
        aggregatorAsSparkWrappedOutput(aggregator)(using intermediateCodec, expr.codec)
      else aggregatorAsSpark(aggregator)(using intermediateCodec, expr.codec)
    val customAggregate = udaf(sparkAggregator, CodecToEncoder.convert(using cf.codec))
    val resultExpr = customAggregate(cf.columns*)
    if outputNeedsWorkaround then resultExpr("_1") else resultExpr
  }

  private def aggregatorAsSpark[From, Intermidate: Codec, To: Codec](
      aggregator: Aggregator[From, Intermidate, To]
  ): SparkAggregator[From, Intermidate, To] =
    new SparkAggregator[From, Intermidate, To] {
      def zero: Intermidate = aggregator.zero
      def reduce(b: Intermidate, a: From): Intermidate = aggregator.reduce(b, a)
      def merge(b1: Intermidate, b2: Intermidate): Intermidate = aggregator.merge(b1, b2)
      def finish(reduction: Intermidate): To = aggregator.finish(reduction)
      def bufferEncoder: Encoder[Intermidate] = summon
      def outputEncoder: Encoder[To] = summon
    }

  private def aggregatorAsSparkWrappedOutput[From, Intermidate: Codec, To: Codec](
      aggregator: Aggregator[From, Intermidate, To]
  ): SparkAggregator[From, Intermidate, Tuple1[To]] =
    new SparkAggregator[From, Intermidate, Tuple1[To]] {
      def zero: Intermidate = aggregator.zero
      def reduce(b: Intermidate, a: From): Intermidate = aggregator.reduce(b, a)
      def merge(b1: Intermidate, b2: Intermidate): Intermidate = aggregator.merge(b1, b2)
      def finish(reduction: Intermidate): Tuple1[To] = Tuple1(aggregator.finish(reduction))
      def bufferEncoder: Encoder[Intermidate] = summon
      def outputEncoder: Encoder[Tuple1[To]] = summon
    }

  object CompatibleNumeric {
    def unapply[T](numeric: Numeric[T]): Boolean =
      numeric match {
        case CompatibleIntegral() => true
        case CompatibleFractional() => true
        case _ => false
      }
  }

  object CompatibleIntegral {
    def unapply[T](numeric: Numeric[T]): Boolean =
      numeric match {
        case Numeric.ByteIsIntegral => true
        case Numeric.ShortIsIntegral => true
        case Numeric.IntIsIntegral => true
        case Numeric.LongIsIntegral => true
        case _ => false
      }
  }

  object CompatibleFractional {
    def unapply[T](numeric: Numeric[T]): Boolean =
      numeric match {
        case Numeric.FloatIsFractional => true
        case Numeric.DoubleIsFractional => true
        case _ => false
      }
  }

  object CompatibleSum {
    def unapply[T](magnet: SumMagnet[T]): Boolean =
      magnet match {
        case SumMagnet.AsLong(CompatibleNumeric()) | SumMagnet.AsDouble(CompatibleNumeric()) | SumMagnet
              .Nullable(CompatibleSum()) | SumMagnet.AsDecimal() => true
        case _ => false
      }
  }
}
