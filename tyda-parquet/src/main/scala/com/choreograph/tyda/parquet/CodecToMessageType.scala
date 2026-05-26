package com.choreograph.tyda.parquet

import org.apache.parquet.schema.LogicalTypeAnnotation
import org.apache.parquet.schema.MessageType
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName
import org.apache.parquet.schema.Type
import org.apache.parquet.schema.Type.Repetition
import org.apache.parquet.schema.Types

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.shapeless3extras.mapConst

object CodecToMessageType {

  def convert[T: Codec]: MessageType = {
    def fields[A](codec: Codec[A]): Seq[Type] =
      codec match {
        case Codec.Product(_, fields, _) => fields.mapConst[Type]([t] => convert(_))
        case Codec.FromInjection(_, inner) => fields(inner)
        case _ => List(convert(codec, Repetition.REQUIRED, "value"))
      }
    Types.buildMessage().addFields(fields(Codec[T])*).named("tyda_schema")
  }

  private def convert[T](field: Field[T]): Type = convert(field.codec, Repetition.REQUIRED, field.name)

  private def convert[T](codec: Codec[T], repitition: Repetition, name: String): Type =
    codec match {
      case Codec.Boolean => Types.primitive(PrimitiveTypeName.BOOLEAN, repitition).named(name)
      case Codec.Byte => Types.primitive(PrimitiveTypeName.INT32, repitition).named(name)
      case Codec.Short => Types.primitive(PrimitiveTypeName.INT32, repitition).named(name)
      case Codec.Int => Types.primitive(PrimitiveTypeName.INT32, repitition).named(name)
      case Codec.Long | Codec.DurationMicros =>
        Types.primitive(PrimitiveTypeName.INT64, repitition).named(name)
      case Codec.Float => Types.primitive(PrimitiveTypeName.FLOAT, repitition).named(name)
      case Codec.Double => Types.primitive(PrimitiveTypeName.DOUBLE, repitition).named(name)
      case Codec.String | _: Codec.SumAsString[?] => Types
          .primitive(PrimitiveTypeName.BINARY, repitition)
          .as(LogicalTypeAnnotation.stringType())
          .named(name)
      case Codec.Bytes => Types.primitive(PrimitiveTypeName.BINARY, repitition).named(name)
      case Codec.Date =>
        val logicalType = LogicalTypeAnnotation.dateType()
        Types.primitive(PrimitiveTypeName.INT32, repitition).as(logicalType).named(name)
      case Codec.TimestampMicros =>
        val logicalType = LogicalTypeAnnotation.timestampType(true, LogicalTypeAnnotation.TimeUnit.MICROS)
        Types.primitive(PrimitiveTypeName.INT64, repitition).as(logicalType).named(name)
      case decimal: Codec.Decimal[?, ?] =>
        val logicalType = LogicalTypeAnnotation.decimalType(decimal.valid.scale, decimal.valid.precision)
        if (decimal.precision <= 9) then
          Types.primitive(PrimitiveTypeName.INT32, repitition).as(logicalType).named(name)
        else if (decimal.precision <= 19) then
          Types.primitive(PrimitiveTypeName.INT64, repitition).as(logicalType).named(name)
        else Types.primitive(PrimitiveTypeName.BINARY, repitition).as(logicalType).named(name)
      case Codec.Option(inner @ Codec.Option(_)) => Types
          .buildGroup(Repetition.OPTIONAL)
          .addField(convert(inner, Repetition.OPTIONAL, "value"))
          .named(name)
      case Codec.Option(inner) => convert(inner, Repetition.OPTIONAL, name)
      case Codec.FromInjection(_, inner) => convert(inner, repitition, name)
      case Codec.Seq(element) =>
        Types.list(repitition).element(convert(element, Repetition.REQUIRED, "element")).named(name)
      case map: Codec.Map[?, ?] => Types
          .map(repitition)
          .key(convert(map.key, Repetition.REQUIRED, "key"))
          .value(convert(map.value, Repetition.REQUIRED, "value"))
          .named(name)
      case prod: Codec.Product[?] =>
        Types.buildGroup(repitition).addFields(prod.fields.mapConst[Type]([t] => convert(_))*).named(name)
    }
}
