package com.choreograph.tyda.arrow

import java.util.List as JList

import scala.jdk.CollectionConverters.*

import org.apache.arrow.vector.complex.MapVector
import org.apache.arrow.vector.types.DateUnit
import org.apache.arrow.vector.types.FloatingPointPrecision
import org.apache.arrow.vector.types.TimeUnit
import org.apache.arrow.vector.types.pojo.ArrowType
import org.apache.arrow.vector.types.pojo.Field
import org.apache.arrow.vector.types.pojo.FieldType
import org.apache.arrow.vector.types.pojo.Schema

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field as TydaField
import com.choreograph.tyda.shapeless3extras.mapConst

private[tyda] object CodecToArrowSchema {
  def convert[T: Codec]: Schema = {
    val field = CodecToArrowSchema.field("value", nullable = false, Codec[T])
    Codec[T] match {
      case Codec.Product(_, _, _) => Schema(field.getChildren)
      case Codec.Sum(_, _) => Schema(field.getChildren)
      case Codec.FromInjection(_, inner) => convert(using inner)
      case _: Codec[?] => Schema(JList.of(field))
    }
  }

  private[arrow] def field(name: String, nullable: Boolean, codec: Codec[?]): Field =
    codec match {
      case p: Codec.Primitive[?] => Field(name, FieldType(nullable, toType(p), null), JList.of())
      case Codec.Seq(inner) => Field(
          name,
          FieldType(nullable, ArrowType.List.INSTANCE, null),
          JList.of(field("element", nullable = false, inner))
        )
      case _: Codec.SumAsString[?] => field(name, nullable, Codec.String)
      case Codec.FromInjection(_, inner) => field(name, nullable, inner)
      case Codec.Map(key, value) => Field(
          name,
          FieldType(nullable, ArrowType.Map(false), null),
          JList.of(structField(
            MapVector.DATA_VECTOR_NAME,
            nullable = false,
            Seq(TydaField(MapVector.KEY_NAME, key), TydaField(MapVector.VALUE_NAME, value))
          ))
        )
      case Codec.Option(inner @ Codec.Option(_)) =>
        structField(name, nullable = true, Seq(TydaField("value", inner)))

      case Codec.Option(inner) => field(name, nullable = true, inner)
      case Codec.Product(_, fields, _) =>
        structField(name, nullable, fields.mapConst[TydaField[?]]([t] => identity(_)))
      case s @ Codec.Sum(_, _) => structField(name, nullable, s.reprFields)
    }

  private def structField(name: String, nullable: Boolean, fields: Seq[TydaField[?]]): Field =
    Field(
      name,
      FieldType(nullable, ArrowType.Struct.INSTANCE, null),
      fields.map(f => field(f.name, nullable = false, f.codec)).asJava
    )

  private def toType(codec: Codec.Primitive[?]): ArrowType =
    codec match {
      case Codec.Boolean => ArrowType.Bool.INSTANCE
      case Codec.Byte => ArrowType.Int(8, true)
      case Codec.Short => ArrowType.Int(16, true)
      case Codec.Int => ArrowType.Int(32, true)
      case Codec.Long => ArrowType.Int(64, true)
      case Codec.String => ArrowType.Utf8.INSTANCE
      case Codec.Float => ArrowType.FloatingPoint(FloatingPointPrecision.SINGLE)
      case Codec.Double => ArrowType.FloatingPoint(FloatingPointPrecision.DOUBLE)
      case Codec.Decimal(precision, scale) => ArrowType.Decimal(precision, scale, 128)
      case Codec.Date => ArrowType.Date(DateUnit.DAY)
      case Codec.TimestampMicros => ArrowType.Timestamp(TimeUnit.MICROSECOND, "UTC")
      case Codec.DurationMicros => ArrowType.Duration(TimeUnit.MICROSECOND)
      case Codec.Bytes => ArrowType.Binary.INSTANCE
    }
}
