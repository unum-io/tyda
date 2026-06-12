package com.choreograph.tyda.sql

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.sql.DdlDialect.DecimalSupport
import com.choreograph.tyda.sql.ast.DdlField
import com.choreograph.tyda.sql.ast.DdlType
import com.choreograph.tyda.sql.ast.DdlWriter
import com.choreograph.tyda.sql.ast.Identifier
import com.choreograph.tyda.sql.ast.TypeAndNullabilityAndComment
import com.choreograph.tyda.unreachable

object ToDdl {

  /** Generates a column definition DDL string for the given codec.
    *
    * The DDL string is generated in a format that is compatible with Spark SQL.
    * In some positions where Spark SQL doesn't support NOT NULL, we comment out
    * the NOT NULL constraint in the generated DDL. For enums, we also specify
    * the possible values that the discriminant field can take in a comment.
    *
    * The main purpose of the DDL string is to serve as a language-agnostic way
    * to specify and communicate the schema of a model at rest.
    *
    * @param codec
    *   The codec representing the schema.
    * @param pretty
    *   If true, the DDL string will be formatted with line breaks and
    *   indentation and include comments.
    * @return
    *   The generated DDL string.
    */
  def toDdl[T](codec: Codec[T], dialect: DdlDialect, pretty: Boolean = true): String = {
    val writer = new java.io.StringWriter()
    DdlWriter(writer, pretty).write(toDdlSchema(codec, dialect))
    if pretty then writer.write("\n")
    writer.toString
  }

  private def toDdlSchema[T](codec: Codec[T], dialect: DdlDialect): Seq[DdlField] = {
    def fieldDdl(field: Field[?]): DdlField =
      toDdlField(field.codec, dialect, notNull = true, supportsNotNull = dialect.supportsNotNullColumn).named(
        field.name
      )
    codec match {
      case Codec.Product(_, _, Some(_)) => Seq(DdlField(
          Identifier(Forbidden.column),
          DdlType.Primitive(dialect.emptyStructFieldType),
          nullable = true
        ))
      case Codec.Product(_, fields, _) => fields.mapConst([t] => fieldDdl(_))
      case sum @ Codec.Sum(_, _) =>
        val docString = sum.variants.mapConst([t] => _.name).mkString("one of: ", ", ", "")
        val repr = sum.reprFields
        fieldDdl(repr.head).copy(comment = Some(docString)) +: repr.tail.map(fieldDdl)
      case _ =>
        Seq(toDdlField(codec, dialect, notNull = true, supportsNotNull = dialect.supportsNotNullColumn).named(
          "value"
        ))
    }
  }

  def toDdlType[T](codec: Codec[T], dialect: DdlDialect): DdlType =
    toDdlField(codec, dialect, notNull = true, supportsNotNull = true).tpe

  private def toDdlField[T](
      codec: Codec[T],
      dialect: DdlDialect,
      notNull: Boolean,
      supportsNotNull: Boolean
  ): TypeAndNullabilityAndComment =
    codec match {
      case Codec.Option(element) =>
        val innerDdlType = toDdlField(element, dialect, notNull = false, supportsNotNull)
        if !notNull then
          TypeAndNullabilityAndComment(
            DdlType.Struct(Seq(
              DdlField(Identifier("value"), innerDdlType.tpe, innerDdlType.nullable, innerDdlType.comment)
            )),
            nullable = true,
            comment = None
          )
        else innerDdlType
      case sum: Codec.SumAsString[T] =>
        val docString = sum.encodedValues.mkString("one of: ", ", ", "")
        toDdlField(Codec.String, dialect, notNull, supportsNotNull).copy(comment = Some(docString))
      // We exclude Codec.Sum here so we can generate docs for the discriminant field
      case Codec.FromInjection(_, to) if !codec.isInstanceOf[Codec.Sum[?, ?]] =>
        toDdlField(to, dialect, notNull, supportsNotNull)
      case _ => TypeAndNullabilityAndComment(
          toNullableDdlType(codec, dialect),
          !(notNull && supportsNotNull),
          Option.when(notNull && !supportsNotNull)("NOT NULL")
        )
    }

  private[tyda] def toNullableDdlType[T](codec: Codec[T], dialect: DdlDialect): DdlType =
    codec match {
      case Codec.Boolean => DdlType.Primitive("BOOLEAN")
      case Codec.Byte => DdlType.Primitive("TINYINT")
      case Codec.Short => DdlType.Primitive("SMALLINT")
      case Codec.Int => DdlType.Primitive("INT")
      case Codec.Long => DdlType.Primitive("BIGINT")
      case Codec.Float => DdlType.Primitive(dialect.floatType)
      case Codec.Double => DdlType.Primitive(dialect.doubleType)
      case Codec.String => DdlType.Primitive("STRING")
      case Codec.Decimal(precision, scale) => dialect.decimal match {
          case DecimalSupport.Decimal128 => DdlType.Primitive(s"DECIMAL($precision,$scale)")
          case DecimalSupport.BigQuery(parameterized) =>
            val tpe = if scale <= 9 && precision <= 29 + scale then "DECIMAL" else "BIGDECIMAL"
            if parameterized then DdlType.Primitive(s"$tpe($precision,$scale)") else DdlType.Primitive(tpe)
        }
      case Codec.TimestampMicros => DdlType.Primitive("TIMESTAMP")
      case Codec.DurationMicros => dialect.duration match {
          case DdlDialect.DurationSupport.Native(typeName) => DdlType.Primitive(typeName)
          case DdlDialect.DurationSupport.Long => toNullableDdlType(Codec.Long, dialect)
        }
      case Codec.Date => DdlType.Primitive("DATE")
      case Codec.Bytes => DdlType.Primitive(dialect.bytesType)

      case Codec.Seq(given Codec[e]) =>
        val element = if shouldWrapArrayElement(Codec[e], dialect) then Codec[(value: e)] else Codec[e]
        DdlType.Array(
          toDdlField(element, dialect, notNull = true, supportsNotNull = dialect.supportsNotNullArrayElement)
        )
      case Codec.Map(key, value) => dialect.map match {
          case DdlDialect.MapSupport.Array => mapAsArray(dialect)(using key, value)
          case DdlDialect.MapSupport.Native(supportsNotNullKey, supportsNotNullValue) => DdlType.Map(
              toDdlField(key, dialect, notNull = true, supportsNotNull = supportsNotNullKey),
              toDdlField(value, dialect, notNull = true, supportsNotNull = supportsNotNullValue)
            )
        }

      case Codec.Product(_, _, _) | Codec.Sum(_, _) => DdlType.Struct(toDdlSchema(codec, dialect))

      case Codec.FromInjection(_, _) => unreachable("FromInjection must be handled in toDdlType")
      case Codec.Option(element) => unreachable("Option must be handled in toDdlType")
    }
  private def mapAsArray[K: Codec, V: Codec](dialect: DdlDialect): DdlType =
    toNullableDdlType(Codec.Seq(Codec[(key: K, value: V)]), dialect)
}
