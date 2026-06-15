package com.choreograph.tyda.bigquery

import com.google.cloud.bigquery.Field as BigQueryField
import com.google.cloud.bigquery.FieldList
import com.google.cloud.bigquery.StandardSQLTypeName
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.*

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Timestamp

object BigQuerySchemaValidatorSpec {
  private def repeated(name: String, tpe: StandardSQLTypeName): BigQueryField =
    BigQueryField.newBuilder(name, tpe).setMode(BigQueryField.Mode.REPEATED).build()
  private def optional(name: String, tpe: StandardSQLTypeName): BigQueryField =
    BigQueryField.newBuilder(name, tpe).setMode(BigQueryField.Mode.NULLABLE).build()
  private def required(name: String, tpe: StandardSQLTypeName): BigQueryField =
    BigQueryField.newBuilder(name, tpe).setMode(BigQueryField.Mode.REQUIRED).build()
  private def singleRequiredField(tpe: StandardSQLTypeName): FieldList = FieldList.of(required("value", tpe))
  private def singleNullableField(tpe: StandardSQLTypeName): FieldList =
    FieldList.of(BigQueryField.newBuilder("value", tpe).setMode(BigQueryField.Mode.NULLABLE).build())
}

class BigQuerySchemaValidatorSpec extends AnyFunSuite {
  import BigQuerySchemaValidatorSpec.*

  private def checkExpectedErrors[T](
      name: String,
      codec: Codec[T],
      fieldList: FieldList,
      expected: Seq[SchemaValidationError]
  ) = test(name) { validateSchema(codec, fieldList) must contain theSameElementsAs (expected) }

  private def checkNoErrors[T](name: String, codec: Codec[T], fieldList: FieldList) =
    checkExpectedErrors(name, codec, fieldList, Seq.empty)

  checkNoErrors("required Byte", Codec[Byte], singleRequiredField(StandardSQLTypeName.INT64))
  checkNoErrors("required Int", Codec[Int], singleRequiredField(StandardSQLTypeName.INT64))
  checkNoErrors("required Short", Codec[Short], singleRequiredField(StandardSQLTypeName.INT64))
  checkNoErrors("required Float", Codec[Float], singleRequiredField(StandardSQLTypeName.FLOAT64))
  checkNoErrors("required Double", Codec[Double], singleRequiredField(StandardSQLTypeName.FLOAT64))
  checkNoErrors("required Date", Codec[Date], singleRequiredField(StandardSQLTypeName.DATE))
  checkNoErrors("required Binary", Codec[Binary], singleRequiredField(StandardSQLTypeName.BYTES))
  checkNoErrors(
    "required Decimal[38, 9]",
    Codec[Decimal[38, 9]],
    singleRequiredField(StandardSQLTypeName.NUMERIC)
  )
  checkNoErrors(
    "required Decimal[38, 30]",
    Codec[Decimal[38, 30]],
    singleRequiredField(StandardSQLTypeName.BIGNUMERIC)
  )
  checkNoErrors("required Timestamp", Codec[Timestamp], singleRequiredField(StandardSQLTypeName.TIMESTAMP))
  checkNoErrors("optional Int", Codec[Option[Int]], singleNullableField(StandardSQLTypeName.INT64))
  checkNoErrors(
    "required seq",
    Codec[Seq[Int]],
    FieldList.of(
      BigQueryField
        .newBuilder("value", StandardSQLTypeName.INT64)
        .setMode(BigQueryField.Mode.REPEATED)
        .build()
    )
  )
  checkExpectedErrors(
    "float instead of int",
    Codec[Int],
    singleRequiredField(StandardSQLTypeName.FLOAT64),
    Seq(
      SchemaValidationError.TypeMismatch(Seq("value"), StandardSQLTypeName.FLOAT64, StandardSQLTypeName.INT64)
    )
  )
  checkExpectedErrors(
    "required Int but nullable in bigquery",
    Codec[Int],
    singleNullableField(StandardSQLTypeName.INT64),
    Seq(SchemaValidationError.UnexpectedNullability(Seq("value")))
  )
  checkExpectedErrors(
    "required Int but repeated in bigquery",
    Codec[Int],
    FieldList.of(repeated("value", StandardSQLTypeName.INT64)),
    Seq(SchemaValidationError.UnexpectedRepeated(Seq("value")))
  )
  checkExpectedErrors(
    "seq in model but required in bigquery",
    Codec[Seq[Int]],
    singleRequiredField(StandardSQLTypeName.INT64),
    Seq(SchemaValidationError.MissingRepeated(Seq("value")))
  )
  checkExpectedErrors(
    "seq in model but nullable in bigquery",
    Codec[Seq[Int]],
    singleNullableField(StandardSQLTypeName.INT64),
    Seq(SchemaValidationError.MissingRepeated(Seq("value")))
  )
  checkExpectedErrors(
    "wrong type in repeated",
    Codec[Seq[Float]],
    FieldList.of(repeated("value", StandardSQLTypeName.INT64)),
    Seq(
      SchemaValidationError.TypeMismatch(Seq("value"), StandardSQLTypeName.INT64, StandardSQLTypeName.FLOAT64)
    )
  )
  checkExpectedErrors(
    "wrong type of field in repeated",
    Codec[Seq[(a: Float, b: String)]],
    FieldList.of(
      BigQueryField
        .newBuilder(
          "value",
          StandardSQLTypeName.STRUCT,
          required("a", StandardSQLTypeName.INT64),
          required("b", StandardSQLTypeName.STRING)
        )
        .setMode(BigQueryField.Mode.REPEATED)
        .build()
    ),
    Seq(SchemaValidationError.TypeMismatch(
      Seq("value", "a"),
      StandardSQLTypeName.INT64,
      StandardSQLTypeName.FLOAT64
    ))
  )
  val stringStringMap = FieldList.of(
    BigQueryField
      .newBuilder(
        "value",
        StandardSQLTypeName.STRUCT,
        required("key", StandardSQLTypeName.STRING),
        required("value", StandardSQLTypeName.STRING)
      )
      .setMode(BigQueryField.Mode.REPEATED)
      .build()
  )
  checkExpectedErrors(
    "wrong type for key in map",
    Codec[Map[Int, String]],
    stringStringMap,
    Seq(SchemaValidationError.TypeMismatch(
      Seq("value", "key"),
      StandardSQLTypeName.STRING,
      StandardSQLTypeName.INT64
    ))
  )
  checkExpectedErrors(
    "wrong type for value in map",
    Codec[Map[String, Int]],
    stringStringMap,
    Seq(SchemaValidationError.TypeMismatch(
      Seq("value", "value"),
      StandardSQLTypeName.STRING,
      StandardSQLTypeName.INT64
    ))
  )

  val simpleStruct = FieldList.of(
    required("a", StandardSQLTypeName.INT64),
    required("b", StandardSQLTypeName.STRING),
    optional("c", StandardSQLTypeName.BOOL)
  )

  checkNoErrors("simple struct", Codec[(a: Int, b: String, c: Option[Boolean])], simpleStruct)

  checkExpectedErrors(
    "non struct type top level",
    Codec[Int],
    simpleStruct,
    Seq(SchemaValidationError.MissingField(Seq("value")))
  )
  checkExpectedErrors(
    "expected simple type found struct",
    Codec[Tuple1[Int]],
    FieldList.of(BigQueryField.of("_1", StandardSQLTypeName.STRUCT, simpleStruct)),
    Seq(SchemaValidationError.TypeMismatch(Seq("_1"), StandardSQLTypeName.STRUCT, StandardSQLTypeName.INT64))
  )
  checkExpectedErrors(
    "accumulate all errors in struct",
    Codec[(a: Boolean, b: Int, c: Double)],
    simpleStruct,
    Seq(
      SchemaValidationError.TypeMismatch(Seq("a"), StandardSQLTypeName.INT64, StandardSQLTypeName.BOOL),
      SchemaValidationError.TypeMismatch(Seq("b"), StandardSQLTypeName.STRING, StandardSQLTypeName.INT64),
      SchemaValidationError.TypeMismatch(Seq("c"), StandardSQLTypeName.BOOL, StandardSQLTypeName.FLOAT64),
      SchemaValidationError.UnexpectedNullability(Seq("c"))
    )
  )
  checkExpectedErrors(
    "missing field in struct",
    Codec[(a: Int, b: String, d: Option[Boolean])],
    simpleStruct,
    Seq(SchemaValidationError.MissingField(Seq("d")))
  )

  checkExpectedErrors(
    "nullable struct field but required in codec",
    Codec[(a: Int, b: (x: Int))],
    FieldList.of(
      required("a", StandardSQLTypeName.INT64),
      BigQueryField
        .newBuilder("b", StandardSQLTypeName.STRUCT, required("x", StandardSQLTypeName.INT64))
        .setMode(BigQueryField.Mode.NULLABLE)
        .build()
    ),
    Seq(SchemaValidationError.UnexpectedNullability(Seq("b")))
  )

  checkExpectedErrors(
    "missing field in nested struct",
    Codec[(a: Int, b: String, c: (d: Int))],
    simpleStruct,
    Seq(
      SchemaValidationError.TypeMismatch(Seq("c"), StandardSQLTypeName.BOOL, StandardSQLTypeName.STRUCT),
      SchemaValidationError.UnexpectedNullability(Seq("c")),
      SchemaValidationError.MissingField(Seq("c", "d"))
    )
  )
}
