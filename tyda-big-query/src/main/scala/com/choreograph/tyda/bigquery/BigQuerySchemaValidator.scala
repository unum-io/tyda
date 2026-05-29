package com.choreograph.tyda.bigquery

import com.google.cloud.bigquery.Field as BigQueryField
import com.google.cloud.bigquery.FieldList
import com.google.cloud.bigquery.StandardSQLTypeName
import shapeless3.deriving.K0

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field

import BigQueryField.Mode

extension (schema: FieldList) {
  private def getOption(fieldName: String): Option[BigQueryField] =
    try Some(schema.get(fieldName))
    catch case _: (IllegalArgumentException | NullPointerException) => None
}

/** Check that a given Codec is compatible with a BigQuery schema.
  *
  * Will check that all fields that the Codec expects exists and that they have
  * correct types and modes. Will not check that the schema does not contain
  * extra fields that are not expected by the Codec.
  */
def validateSchema[T](codec: Codec[T], schema: FieldList): Seq[SchemaValidationError] =
  codec match {
    case Codec.Product(_, fields, _) => validateProduct(fields, schema, Seq.empty)
    case Codec.FromInjection(_, inner) => validateSchema(inner, schema)
    case other =>
      val name = "value"
      schema
        .getOption(name)
        .fold(Seq(SchemaValidationError.MissingField(Seq(name))))(field =>
          validateField(other, field, Seq(name))
        )
  }

private def validateField[T](
    codec: Codec[T],
    field: BigQueryField,
    path: Seq[String],
    nullable: Boolean = false,
    repeated: Boolean = false
): Seq[SchemaValidationError] = {
  def checkType(expected: StandardSQLTypeName): Seq[SchemaValidationError] = {
    val bigqueryType = field.getType().getStandardType()
    if (bigqueryType == expected) Seq.empty
    else Seq(SchemaValidationError.TypeMismatch(path, bigqueryType, expected))
  }
  def checkMode(): Seq[SchemaValidationError] =
    Option(field.getMode()) match {
      case Some(Mode.NULLABLE | Mode.REQUIRED) if repeated => Seq(SchemaValidationError.MissingRepeated(path))
      case Some(Mode.REPEATED) if !repeated => Seq(SchemaValidationError.UnexpectedRepeated(path))
      case Some(Mode.NULLABLE) if !nullable => Seq(SchemaValidationError.UnexpectedNullability(path))
      case None | Some(Mode.REQUIRED | Mode.NULLABLE | Mode.REPEATED) => Seq.empty
    }

  codec match {
    case p: Codec.Primitive[?] =>
      val typeError = p match {
        case Codec.Boolean => checkType(StandardSQLTypeName.BOOL)
        case Codec.Byte | Codec.Short | Codec.Int | Codec.Long | Codec.DurationMicros =>
          checkType(StandardSQLTypeName.INT64)
        case Codec.Float | Codec.Double => checkType(StandardSQLTypeName.FLOAT64)
        case Codec.String => checkType(StandardSQLTypeName.STRING)
        case Codec.Bytes => checkType(StandardSQLTypeName.BYTES)
        case Codec.Decimal(precision, scale) if scale <= 9 && precision <= 29 + scale =>
          checkType(StandardSQLTypeName.NUMERIC)
        case Codec.Decimal(_, _) => checkType(StandardSQLTypeName.BIGNUMERIC)
        case Codec.Date => checkType(StandardSQLTypeName.DATE)
        case Codec.TimestampMicros => checkType(StandardSQLTypeName.TIMESTAMP)
      }
      typeError ++ checkMode()
    case Codec.Option(element) => validateField(element, field, path, nullable = true, repeated)
    case Codec.Seq(element) => validateField(element, field, path, nullable, repeated = true)
    case Codec.Map(given Codec[k], given Codec[v]) =>
      validateField(Codec[Seq[(key: k, value: v)]], field, path, nullable, repeated)
    case Codec.Product(_, fields, _) => checkType(StandardSQLTypeName.STRUCT) ++ checkMode() ++
        validateProduct(fields, field.getSubFields(), path)
    case Codec.FromInjection(_, inner) => validateField(inner, field, path, nullable, repeated)
  }
}

private def validateProduct[T](
    fields: K0.ProductInstances[Field, T],
    schema: FieldList,
    path: Seq[String]
): Seq[SchemaValidationError] =
  fields.foldLeft0(Seq.empty[SchemaValidationError])([t] =>
    (acc, field) =>
      val fieldName = field.name
      schema.getOption(fieldName) match {
        case None => acc :+ SchemaValidationError.MissingField(path :+ fieldName)
        case Some(bigQueryField) => acc ++ validateField(field.codec, bigQueryField, path :+ fieldName)
      }
  )
