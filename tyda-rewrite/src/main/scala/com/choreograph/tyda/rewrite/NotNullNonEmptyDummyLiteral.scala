package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.shapeless3extras.mapConst

/** Utility to create a not-null, non-empty literal value for any type T.
  *
  * This can be used to create an expression with the correct schema (inluding
  * nullability) which is not always possible using a null/empty collection and
  * cast. This is because for example Spark does not allow `NOT NULL` in the DDL
  * eventhough it supported in the schema.
  */
object NotNullNonEmptyDummyLiteral {
  def create[T](codec: Codec[T]): ExprNode[T] =
    codec match {
      case Codec.Byte => ExprNode.Literal(0.toByte, Codec.Byte)
      case Codec.Short => ExprNode.Literal(0.toShort, Codec.Short)
      case Codec.Int => ExprNode.Literal(0, Codec.Int)
      case Codec.Long => ExprNode.Literal(0L, Codec.Long)
      case Codec.Float => ExprNode.Literal(0f, Codec.Float)
      case Codec.Double => ExprNode.Literal(0.0, Codec.Double)
      case Codec.String => ExprNode.Literal("", Codec.String)
      case Codec.Boolean => ExprNode.Literal(false, Codec.Boolean)
      case Codec.Bytes => ExprNode.Literal(Array.emptyByteArray, Codec.Bytes)
      case Codec.TimestampMicros => ExprNode.Literal(Timestamp.fromMicros(0), Codec.TimestampMicros)
      case Codec.DurationMicros => ExprNode.Literal(Duration.fromMicros(0), Codec.DurationMicros)
      case Codec.Date => ExprNode.Literal(Date.fromDays(0), Codec.Date)
      case dec @ Codec.Decimal(precision, scale) => ExprNode.Literal(Decimal.zero(using dec.valid), dec)
      case Codec.Option(element) => ExprNode.MakeSome(create(element))
      case Codec.Map(given Codec[k], given Codec[v]) => ExprNode.MakeMap(create(Codec[Seq[(k, v)]]))
      case Codec.Seq(element) => ExprNode.MakeSeq(Seq(create(element)), element)
      case codec @ Codec.Product(_, fields, _) =>
        ExprNode.makeProductUnsafe(fields.mapConst[ExprNode[?]]([t] => f => create(f.codec)), codec)
      case inj @ Codec.FromInjection(_, to) => ExprNode.FromRepr(create(to), inj)
    }
}
