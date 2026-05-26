package com.choreograph.tyda.json

import scala.collection.immutable.ListMap

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.Variant
import com.choreograph.tyda.shapeless3extras.mapConst

object CodecToJsonSchema {

  /** Generates a JSON Schema string for the given Codec. */
  def create[T: Codec]: String = topLevelSchema(Codec[T]).toJson

  private def topLevelSchema[T](codec: Codec[T]): JsonSchema =
    codec match {
      case Codec.Product(_, _, _) => typeSchema(codec)
      case sum @ Codec.Sum(_, _) => sumSchema(sum)
      case sum: Codec.SumAsString[?] => valueObjectSchema(sum)
      case Codec.FromInjection(_, to) => topLevelSchema(to)
      case _ => valueObjectSchema(codec)
    }

  private def valueObjectSchema[T](codec: Codec[T]): JsonSchema =
    JsonSchema.obj(
      properties = ListMap("value" -> typeSchema(codec)),
      required = Seq("value"),
      additionalProperties = false
    )

  private def typeSchema[T](codec: Codec[T]): JsonSchema =
    codec match {
      case Codec.Boolean => JsonSchema.typed("boolean")
      case Codec.Byte | Codec.Short | Codec.Int => JsonSchema.integer
      case Codec.Long => JsonSchema.oneOf(JsonSchema.integer, JsonSchema.string)
      case Codec.Float | Codec.Double => JsonSchema.oneOf(JsonSchema.number, JsonSchema.string)
      case Codec.String => JsonSchema.string
      case Codec.Bytes => JsonSchema(`type` = Some("string"), contentEncoding = Some("base64"))
      case Codec.Decimal(_, _) => JsonSchema.oneOf(JsonSchema.number, JsonSchema.string)
      case Codec.Date => JsonSchema(`type` = Some("string"), format = Some("date"))
      case Codec.TimestampMicros => JsonSchema(`type` = Some("string"), format = Some("date-time"))
      case Codec.DurationMicros => JsonSchema.oneOf(JsonSchema.integer, JsonSchema.string)

      case Codec.Option(element @ Codec.Option(_)) => nestedOptionSchema(element)
      case Codec.Option(element) => JsonSchema.nullable(typeSchema(element))

      case Codec.Seq(element) => JsonSchema.arr(typeSchema(element))

      case Codec.Map(key, value) => mapSchema(key, value)

      case codec @ Codec.Product(_, _, _) => productSchema(codec)
      case sum @ Codec.Sum(_, _) => sumSchema(sum)
      case sum: Codec.SumAsString[?] => sumAsStringSchema(sum)
      case Codec.FromInjection(_, to) => typeSchema(to)
    }

  /** SumAsString types are encoded as plain strings with an enum constraint. */
  private def sumAsStringSchema(sum: Codec.SumAsString[?]): JsonSchema =
    JsonSchema.stringEnum(sum.encodedValues.toSeq)

  /** Nested Option[Option[T]] is encoded as a nullable object with a "value"
    * field.
    */
  private def nestedOptionSchema[T](element: Codec.Option[T]): JsonSchema =
    JsonSchema.nullable(valueObjectSchema(element))

  /** Map[K, V] is encoded as an array of key-value objects. */
  private def mapSchema[K, V](key: Codec[K], value: Codec[V]): JsonSchema =
    JsonSchema.arr(JsonSchema.obj(
      properties = ListMap("key" -> typeSchema(key), "value" -> typeSchema(value)),
      required = Seq("key", "value"),
      additionalProperties = false
    ))

  /** A field is required unless its underlying codec is Option.
    */
  private def isRequired(codec: Codec[?]): Boolean =
    codec match {
      case Codec.Option(_) => false
      case _ => true
    }

  /** Product types are encoded as JSON objects. Option fields are not required.
    */
  private def productSchema[T](codec: Codec.Product[T]): JsonSchema = {
    val fieldList = codec.fields.mapConst[Field[?]]([t] => identity(_))
    val properties = ListMap.from(fieldList.map(f => f.name -> fieldSchema(f.codec)))
    val required = fieldList.collect { case f if isRequired(f.codec) => f.name }
    JsonSchema.obj(properties = properties, required = required, additionalProperties = true)
  }

  /** Schema for the value inside a field. For Option fields, this is the
    * element schema (nullability is handled by omitting from required).
    */
  private def fieldSchema[T](codec: Codec[T]): JsonSchema =
    codec match {
      case Codec.Option(element @ Codec.Option(_)) => nestedOptionSchema(element)
      case Codec.Option(element) => JsonSchema.nullable(typeSchema(element))
      case Codec.FromInjection(_, to) => fieldSchema(to)
      case other => typeSchema(other)
    }

  /** Sum types are encoded as a oneOf where each variant is an object with the
    * discriminant as a const and (for non-singleton variants) a required field
    * for the variant data.
    */
  private def sumSchema[T](sum: Codec.Sum[T, ?]): JsonSchema = {
    val variantSchemas = sum
      .variants
      .mapConst[JsonSchema]([t] =>
        (variant: Variant[t]) =>
          variant match {
            case Variant.Product(name, codec) => JsonSchema.obj(
                properties = ListMap(
                  Codec.Sum.discriminant -> JsonSchema.stringConst(name),
                  name -> productSchema(codec)
                ),
                required = Seq(Codec.Sum.discriminant, name),
                additionalProperties = true
              )
            case Variant.Singleton(name, _, _) => JsonSchema.obj(
                properties = ListMap(Codec.Sum.discriminant -> JsonSchema.stringConst(name)),
                required = Seq(Codec.Sum.discriminant),
                additionalProperties = true
              )
          }
      )
    JsonSchema.oneOf(variantSchemas*)
  }

}
