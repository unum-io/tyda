package com.choreograph.tyda.bigquery

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.functions.namedTuple
import com.choreograph.tyda.rewrite.Cast
import com.choreograph.tyda.rewrite.Cast.buildConverterUp
import com.choreograph.tyda.rewrite.CollectionCodec

/** BigQuery supports nullable arrays and null in arrays but only for
  * intermediate expressions.
  *
  * This object has helper methods to rewrite the final result such that it can
  * be collected without loss of information. This is done by
  *
  *   1. Rewriting all nullable arrays into a struct with an isEmpty field and a
  *      value field. This allows us to distinguish between null and empty
  *      arrays.
  *   2. Rewriting all arrays of nullable elements into an array of struct with
  *      a value field.
  */
private object BigQueryCollectionRewrites {
  final case class InvertAndCodec[From, To](invert: To => From, to: Codec[To])

  /** Matches any Codec that was rewriten by the corresponding rewrite. */
  def unapply[From](codec: Codec[From]): Option[InvertAndCodec[From, ?]] =
    codec match {
      case Codec.Option(CollectionCodec(given Codec[s])) => Some(
          InvertAndCodec[From, (isEmpty: Boolean, value: s)](
            (isEmpty, value) => Option.when(!isEmpty)(value),
            summon
          )
        )
      case Codec.Seq(Codec.Option(given Codec[e])) if !CollectionCodec.unapply(Codec[e]).isDefined =>
        Some(InvertAndCodec[From, Seq[(value: Option[e])]](_.map(v => v.value), summon))
      case Codec.FromInjection(inj, BigQueryCollectionRewrites(InvertAndCodec(invert, inner))) =>
        Some(InvertAndCodec(invert.andThen(inj.invert), inner))
      case _ => None
    }

  def matches(codec: Codec[?]): Boolean = unapply(codec).isDefined

  /** Rewrite all NullableArrays into a struct with an isEmpty field and a value
    * field. This allows us to distinguish between null and empty arrays.
    */
  def rewrite[T](ds: Dataset[T]): Dataset[?] = {
    val maybeCast = buildConverterUp(ds.codec)([t] =>
      _ match {
        case Codec.Option(CollectionCodec(_: Codec[s])) =>
          Some(Cast((e: Expr[Option[s]]) => namedTuple(isEmpty = e.isEmpty, value = e)))
        case Codec.Seq(Codec.Option(element: Codec[e])) if !CollectionCodec.unapply(element).isDefined =>
          Some(Cast((e: Expr[Seq[Option[e]]]) => e.map(v => namedTuple((value = v)))))
        case _ => None
      }
    )
    maybeCast match {
      case Some(Cast(cast)) => ds.select(cast)
      case None => ds
    }
  }
}
