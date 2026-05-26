package com.choreograph.tyda.json

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Codec.EnumAsString
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName

object CodecToJsonSchemaSpec {
  final case class SimpleProduct(a: Int, b: Option[String]) derives Codec
  final case class AllOptionalProduct(a: Option[Int], b: Option[String]) derives Codec

  enum Color extends EnumStableHashCode derives EnumAsString {
    case Red, Green, Blue
  }
  enum Animal extends EnumStableHashCode derives Codec {
    case Dog(name: String)
    case Cat
  }
}

class CodecToJsonSchemaSpec extends AnyFunSuite {
  import CodecToJsonSchemaSpec.*

  private def testSchema[T: Codec: TypeName](expected: String): Unit =
    test(s"schema for ${TypeName.name[T]}") {
      val actual = CodecToJsonSchema.create[T]
      assert(actual == expected, s"\nExpected: $expected\nActual:   $actual")
    }

  private def testValueObjectSchema[T: Codec: TypeName](expectedInner: String): Unit =
    val valueObject =
      """{"type":"object","properties":{"value":%s},"required":["value"],"additionalProperties":false}"""
    test(s"value object schema for ${TypeName.name[T]}") {
      val actual = CodecToJsonSchema.create[T]
      val expected = valueObject.format(expectedInner)
      assert(actual == expected, s"\nExpected: $expected\nActual:   $actual")
    }

  testValueObjectSchema[Boolean]("""{"type":"boolean"}""")
  testValueObjectSchema[Byte]("""{"type":"integer"}""")
  testValueObjectSchema[Short]("""{"type":"integer"}""")
  testValueObjectSchema[Int]("""{"type":"integer"}""")
  testValueObjectSchema[Long]("""{"oneOf":[{"type":"integer"},{"type":"string"}]}""")
  testValueObjectSchema[Float]("""{"oneOf":[{"type":"number"},{"type":"string"}]}""")
  testValueObjectSchema[Double]("""{"oneOf":[{"type":"number"},{"type":"string"}]}""")
  testValueObjectSchema[String]("""{"type":"string"}""")
  testValueObjectSchema[Decimal[15, 5]]("""{"oneOf":[{"type":"number"},{"type":"string"}]}""")
  testValueObjectSchema[Date]("""{"type":"string","format":"date"}""")
  testValueObjectSchema[Timestamp]("""{"type":"string","format":"date-time"}""")
  testValueObjectSchema[Duration]("""{"oneOf":[{"type":"integer"},{"type":"string"}]}""")
  testValueObjectSchema[Option[Int]]("""{"oneOf":[{"type":"integer"},{"type":"null"}]}""")
  testValueObjectSchema[Seq[Int]]("""{"type":"array","items":{"type":"integer"}}""")
  testValueObjectSchema[Map[String, Int]](
    """{"type":"array","items":{"type":"object","properties":{"key":{"type":"string"},"value":{"type":"integer"}},"required":["key","value"],"additionalProperties":false}}"""
  )
  testValueObjectSchema[Color]("""{"type":"string","enum":["red","green","blue"]}""")
  testValueObjectSchema[Option[Option[Int]]](
    """{"oneOf":[{"type":"object","properties":{"value":{"oneOf":[{"type":"integer"},{"type":"null"}]}},"required":["value"],"additionalProperties":false},{"type":"null"}]}"""
  )
  testSchema[SimpleProduct](
    """{"type":"object","properties":{"a":{"type":"integer"},"b":{"oneOf":[{"type":"string"},{"type":"null"}]}},"required":["a"],"additionalProperties":true}"""
  )
  testSchema[EmptyTuple]("""{"type":"object","additionalProperties":true}""")
  testSchema[AllOptionalProduct](
    """{"type":"object","properties":{"a":{"oneOf":[{"type":"integer"},{"type":"null"}]},"b":{"oneOf":[{"type":"string"},{"type":"null"}]}},"additionalProperties":true}"""
  )
  testSchema[Animal]("""{
    |  "oneOf": [
    |    {
    |      "type": "object",
    |      "properties": {
    |        "discriminant": {"const": "dog"},
    |        "dog": {
    |          "type": "object",
    |          "properties": {"name": {"type": "string"}},
    |          "required": ["name"],
    |          "additionalProperties": true
    |        }
    |      },
    |      "required": ["discriminant", "dog"],
    |      "additionalProperties": true
    |    },
    |    {
    |      "type": "object",
    |      "properties": {"discriminant": {"const": "cat"}},
    |      "required": ["discriminant"],
    |      "additionalProperties": true
    |    }
    |  ]
    |}""".stripMargin.replaceAll("\\s+", ""))
}
