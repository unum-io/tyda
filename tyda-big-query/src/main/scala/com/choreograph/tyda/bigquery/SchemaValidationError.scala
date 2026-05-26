package com.choreograph.tyda.bigquery
import com.google.cloud.bigquery.StandardSQLTypeName

enum SchemaValidationError {
  case MissingField(path: Seq[String])
  case TypeMismatch(path: Seq[String], bigQueryType: StandardSQLTypeName, codecType: StandardSQLTypeName)
  case UnexpectedNullability(path: Seq[String])
  case UnexpectedRepeated(path: Seq[String])
  case MissingRepeated(path: Seq[String])

  def formatted(tableId: String): String = {
    def formatPath(path: Seq[String]): String = path.mkString(".")
    this match {
      case MissingField(path) => s"Table: $tableId Field: ${formatPath(path)} is missing in BigQuery schema"
      case TypeMismatch(path, bigQueryType, codecType) =>
        val fieldPath = formatPath(path)
        s"Table: $tableId Field: ${fieldPath} has type $bigQueryType in BigQuery schema but expected $codecType based on Codec"
      case UnexpectedNullability(path) =>
        val fieldPath = formatPath(path)
        s"Table: $tableId Field: ${fieldPath} is nullable in BigQuery schema but expected to be required based on Codec"
      case UnexpectedRepeated(path) =>
        val fieldPath = formatPath(path)
        s"Table: $tableId Field: ${fieldPath} is repeated in BigQuery schema but expected to be a single value based on Codec"
      case MissingRepeated(path) =>
        val fieldPath = formatPath(path)
        s"Table: $tableId Field: ${fieldPath} is not repeated in BigQuery schema but expected to be repeated based on Codec"
    }
  }
}
