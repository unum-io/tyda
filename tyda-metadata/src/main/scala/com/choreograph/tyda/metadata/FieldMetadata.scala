package com.choreograph.tyda.metadata

/** Metadata representation for a single field in a table schema.
  *
  * This case class represents a field with its name, description, SQL type, and
  * optionally nested fields for STRUCT types.
  *
  * @param name
  *   The field name
  * @param fieldDescription
  *   The field description extracted from Scaladoc
  * @param typeDescription
  *   The type description extracted from Scaladoc
  * @param fields
  *   Nested fields for STRUCT types
  */
final case class FieldMetadata(
    name: String,
    fieldDescription: Option[String],
    typeDescription: Option[String],
    fields: Seq[FieldMetadata]
)
