package com.choreograph.tyda.spark

import org.apache.spark.sql.Row
import org.apache.spark.sql.catalyst.expressions.*

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Variant
import com.choreograph.tyda.shapeless3extras.mapConst

/* We encode to a Row here using scala code. This way it easier to move this to the Spark 4.0.0 approach where
 * we will use this inside a TransformingEncoder. */
private[spark] trait SumToRowEncoder[S] extends SparkCodec[S, Row] {
  def encode(s: S): Row
  def decode(row: Row): S
}

private object SumToRowEncoder {
  def apply[T](s: Codec.Sum[T, ?]): SumToRowEncoder[T] = {
    val reprSize = s.reprFields.size
    val ordinalToSingleton = s
      .variants
      .mapConst[Option[T]]([t <: T] => Some(_).collect { case Variant.Singleton(value = v) => v })
      .toArray
    val ordinalToProductOfAllNone = s.variants.mapConst[scala.Option[T]]([t <: T] => _.allNone).toArray
    val ordinalToIndex = s
      .variants
      .mapConst[Boolean]([t] => _.isInstanceOf[Variant.Singleton[?]])
      .scanLeft(1)((index, isSingleton) => if (isSingleton) { index } else { index + 1 })
      .dropRight(1)
      .toArray
    val variantNames = s.variants.mapConst[String]([t] => _.name)
    val discriminantToOrdinal = variantNames.zipWithIndex.toMap
    val ordinalToDiscriminant = variantNames.toArray
    new SumToRowEncoder[T] {
      def encode(in: T): Row = {
        val values = new Array[Any](reprSize)
        val ordinal = s.ordinal(in)
        values(0) = ordinalToDiscriminant(ordinal)
        if (ordinalToSingleton(ordinal).isEmpty) values(ordinalToIndex(ordinal)) = Some(in)
        new GenericRow(values)
      }
      def decode(out: Row): T = {
        val discriminant = out.getString(0)
        val ordinal = discriminantToOrdinal(discriminant)
        ordinalToSingleton(ordinal)
          /* When reading old data where a singleton has been changed to a product spark can return null here. */
          .orElse(Option(out.getAs[Option[T]](ordinalToIndex(ordinal))).flatten)
          .orElse(ordinalToProductOfAllNone(ordinal))
          .getOrElse {
            throw new RuntimeException(
              s"Expected non-null value for variant $discriminant but found null in row $out"
            )
          }
      }
    }
  }
}
