package com.choreograph.tyda.bigquery

import com.google.cloud.bigquery.Field as BqField
import com.google.cloud.bigquery.Field.Mode
import com.google.cloud.bigquery.Schema
import com.google.cloud.bigquery.StandardSQLTypeName
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Field
import com.choreograph.tyda.Timestamp

class SchemaToCodecSpec extends AnyFunSuite {

  test("maps standard BigQuery types") {
    val schema = Schema.of(
      BqField.newBuilder("long_field", StandardSQLTypeName.INT64).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("double_field", StandardSQLTypeName.FLOAT64).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("bool_field", StandardSQLTypeName.BOOL).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("string_field", StandardSQLTypeName.STRING).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("bytes_field", StandardSQLTypeName.BYTES).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("timestamp_field", StandardSQLTypeName.TIMESTAMP).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("date_field", StandardSQLTypeName.DATE).setMode(Mode.REQUIRED).build()
    )
    val approx = SchemaToCodec(schema)
    assert(approx.isExact)
    val expected = Codec[
      (
          long_field: Long,
          double_field: Double,
          bool_field: Boolean,
          string_field: String,
          bytes_field: Binary,
          timestamp_field: Timestamp,
          date_field: Date
      )
    ]
    assert(approx.codec == expected)
  }

  test("maps nullable (optional) and repeated (seq) fields") {
    val schema = Schema.of(
      BqField.newBuilder("opt_string", StandardSQLTypeName.STRING).setMode(Mode.NULLABLE).build(),
      BqField.newBuilder("seq_long", StandardSQLTypeName.INT64).setMode(Mode.REPEATED).build()
    )
    val approx = SchemaToCodec(schema)
    assert(approx.isExact)
    val expected = Codec[(opt_string: Option[String], seq_long: Seq[Long])]
    assert(approx.codec == expected)
  }

  test("maps standard NUMERIC decimal") {
    val schema = Schema.of(
      BqField.newBuilder("numeric_field", StandardSQLTypeName.NUMERIC).setMode(Mode.REQUIRED).build()
    )
    val approx = SchemaToCodec(schema)
    assert(approx.isExact)
    val expected = Codec[(numeric_field: Decimal[38, 9])]
    assert(approx.codec == expected)
  }

  test("standard BIGNUMERIC throws AssertionError because default precision 76 exceeds MaxPrecision 38") {
    val schema = Schema.of(
      BqField.newBuilder("bignumeric_field", StandardSQLTypeName.BIGNUMERIC).setMode(Mode.REQUIRED).build()
    )
    intercept[AssertionError] { SchemaToCodec(schema) }
  }

  test("maps decimals with custom precision and scale") {
    val schema = Schema.of(
      BqField
        .newBuilder("numeric_custom", StandardSQLTypeName.NUMERIC)
        .setPrecision(10L)
        .setScale(2L)
        .setMode(Mode.REQUIRED)
        .build(),
      BqField
        .newBuilder("bignumeric_custom", StandardSQLTypeName.BIGNUMERIC)
        .setPrecision(38L)
        .setScale(30L)
        .setMode(Mode.REQUIRED)
        .build()
    )
    val approx = SchemaToCodec(schema)
    assert(approx.isExact)
    val expected = Codec[(numeric_custom: Decimal[10, 2], bignumeric_custom: Decimal[38, 30])]
    assert(approx.codec == expected)
  }

  test("maps nested structs") {
    val subField1 = BqField.newBuilder("a", StandardSQLTypeName.INT64).setMode(Mode.REQUIRED).build()
    val subField2 = BqField.newBuilder("b", StandardSQLTypeName.STRING).setMode(Mode.NULLABLE).build()
    val schema = Schema.of(
      BqField
        .newBuilder("nested", StandardSQLTypeName.STRUCT, subField1, subField2)
        .setMode(Mode.REQUIRED)
        .build()
    )
    val approx = SchemaToCodec(schema)
    assert(approx.isExact)
    val expected = Codec[(nested: (a: Long, b: Option[String]))]
    assert(approx.codec == expected)
  }

  test("skips unsupported BigQuery types") {
    val schema = Schema.of(
      BqField.newBuilder("valid", StandardSQLTypeName.INT64).setMode(Mode.REQUIRED).build(),
      BqField.newBuilder("invalid", StandardSQLTypeName.GEOGRAPHY).setMode(Mode.NULLABLE).build()
    )
    val approx = SchemaToCodec(schema)
    assert(!approx.isExact)
    assert(approx.codec == Codec.unsafeNamedTuple(Seq(Field("valid", Codec.Long))))
  }
}
