package com.choreograph.tyda.json

import scala.collection.immutable.ListMap

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.github.plokhotnyuk.jsoniter_scala.macros.CodecMakerConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/** A simplified representation of JSON Schema. This is not meant to be a full
  * implementation of JSON Schema spec, only the subset needed to describe the
  * JSON produced by CodecToJsoniter.
  */
private[json] final case class JsonSchema(
    `type`: Option[String] = None,
    properties: Option[ListMap[String, JsonSchema]] = None,
    required: Option[Seq[String]] = None,
    items: Option[JsonSchema] = None,
    oneOf: Option[Seq[JsonSchema]] = None,
    `enum`: Option[Seq[String]] = None,
    const: Option[String] = None,
    format: Option[String] = None,
    contentEncoding: Option[String] = None,
    additionalProperties: Option[Boolean] = None
) {
  def toJson: String = writeToString(this)
}

private[json] object JsonSchema {
  def typed(t: String): JsonSchema = JsonSchema(`type` = Some(t))

  def obj(
      properties: ListMap[String, JsonSchema],
      required: Seq[String] = Nil,
      additionalProperties: Boolean = true
  ): JsonSchema =
    JsonSchema(
      `type` = Some("object"),
      properties = Option.when(properties.nonEmpty)(properties),
      required = Option.when(required.nonEmpty)(required),
      additionalProperties = Some(additionalProperties)
    )

  def arr(items: JsonSchema): JsonSchema = JsonSchema(`type` = Some("array"), items = Some(items))

  def oneOf(schemas: JsonSchema*): JsonSchema = JsonSchema(oneOf = Some(schemas.toSeq))

  def stringConst(value: String): JsonSchema = JsonSchema(const = Some(value))

  def stringEnum(values: Seq[String]): JsonSchema = JsonSchema(`type` = Some("string"), `enum` = Some(values))

  val string: JsonSchema = typed("string")
  val integer: JsonSchema = typed("integer")
  val number: JsonSchema = typed("number")
  val `null`: JsonSchema = typed("null")

  /** Wraps a schema to also accept null (for Option fields). */
  def nullable(inner: JsonSchema): JsonSchema = oneOf(inner, `null`)

  given JsonValueCodec[JsonSchema] = JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
}
