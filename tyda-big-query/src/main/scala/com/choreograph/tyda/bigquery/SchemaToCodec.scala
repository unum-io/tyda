package com.choreograph.tyda.bigquery

import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.int.<=
import scala.compiletime.ops.int.>=
import scala.jdk.CollectionConverters.*

import com.google.cloud.bigquery.Field as BqField
import com.google.cloud.bigquery.Field.Mode
import com.google.cloud.bigquery.FieldList
import com.google.cloud.bigquery.Schema
import com.google.cloud.bigquery.StandardSQLTypeName

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Field
import com.choreograph.tyda.rewrite.ApproximatedCodec

/** Builds a `Codec` from a runtime BigQuery schema.
  */
object SchemaToCodec {

  def apply(schema: Schema): ApproximatedCodec[?] = recordCodec(schema.getFields)

  private def recordCodec(fields: FieldList): ApproximatedCodec[?] = {
    val nameAndApproximatedCodec = fields
      .asScala
      .iterator
      .flatMap(f => fieldCodec(f).map(c => f.getName -> c))
      .toSeq
    val tydaFields = nameAndApproximatedCodec.map((name, approx) => Field(name, approx.codec))
    val isExact = fields.size == tydaFields.size && nameAndApproximatedCodec.forall(_._2.isExact)
    ApproximatedCodec(Codec.unsafeNamedTuple(tydaFields), isExact)
  }

  private def fieldCodec(field: BqField): Option[ApproximatedCodec[?]] =
    Option(field.getMode).getOrElse(Mode.NULLABLE) match {
      case Mode.REQUIRED => baseCodec(field)
      case Mode.NULLABLE => baseCodec(field).map(c => c.copy(codec = Codec.Option(c.codec)))
      case Mode.REPEATED => baseCodec(field).map(c => c.copy(codec = Codec.Seq(c.codec)))
    }

  private def baseCodec(field: BqField): Option[ApproximatedCodec[?]] = {
    def exact[T](codec: Codec[T]) = Some(ApproximatedCodec(codec, true))
    field.getType.getStandardType match {
      case StandardSQLTypeName.STRING => exact(Codec.String)
      case StandardSQLTypeName.BYTES => exact(Codec.Bytes)
      case StandardSQLTypeName.INT64 => exact(Codec.Long)
      case StandardSQLTypeName.FLOAT64 => exact(Codec.Double)
      case StandardSQLTypeName.BOOL => exact(Codec.Boolean)
      case StandardSQLTypeName.TIMESTAMP => exact(Codec.TimestampMicros)
      case StandardSQLTypeName.DATE => exact(Codec.Date)
      case StandardSQLTypeName.NUMERIC => exact(decimalCodec(field, defaultPrecision = 38, defaultScale = 9))
      case StandardSQLTypeName.BIGNUMERIC =>
        exact(decimalCodec(field, defaultPrecision = 76, defaultScale = 38))
      case StandardSQLTypeName.STRUCT => Some(recordCodec(field.getSubFields))
      case _ => None
    }
  }

  // TODO: Check valid size
  private def decimalCodec(field: BqField, defaultPrecision: Int, defaultScale: Int): Codec[?] = {
    val precision = Option(field.getPrecision).map(_.toInt).getOrElse(defaultPrecision)
    val scale = Option(field.getScale).map(_.toInt).getOrElse(defaultScale)
    decimalCodec2(precision, scale)
  }

  private def decimalCodec2[P <: Int, S <: Int](p: P, s: S): Codec[Decimal[P, S]] = {
    assert(s <= p && s >= 0 && p >= 1 && p <= Decimal.MaxPrecision)
    // TYPE SAFETY: Spark only supports 128 bit decimal types
    given ((S <= P && S >= 0 && P >= 1 && P <= Decimal.MaxPrecision) =:= true) =
      summon[true =:= true].asInstanceOf
    given ValueOf[P] = ValueOf(p)
    given ValueOf[S] = ValueOf(s)
    Codec.Decimal[P, S](p, s)
  }
}
