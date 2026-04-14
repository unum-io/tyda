package com.choreograph.tyda.sql

final case class DdlDialect(
    decimal: DdlDialect.DecimalSupport,
    duration: DdlDialect.DurationSupport,
    map: DdlDialect.MapSupport,
    supportsNotNullColumn: Boolean,
    supportsNotNullArrayElement: Boolean,
    // Since empty structs are not widely supported we always add a dummy field of this type.
    // All values for this field will be null, so if the engine supports a null type that should
    // be used.
    emptyStructFieldType: String,
    /* When this is set to false a struct (with a single field value) will be generated inside nested arrays.
     * This is used for query engines like BigQuery that does not support nested arrays. This is not meant to
     * give complete support as writing nested arrays will not produce correct schemas, but intermediate
     * nested arrays should work. */
    supportsArrayAsArrayElement: Boolean = true,
    floatType: String = "FLOAT",
    doubleType: String = "DOUBLE",
    bytesType: String = "BINARY"
)

object DdlDialect {
  enum MapSupport {
    case Array
    case Native(supportsNotNullKey: Boolean, supportsNotNullValue: Boolean)
  }

  enum DecimalSupport {

    /** Supports a parameterized decimal type with a size of 128bits.
      *
      * The precision `p` should be `0 <= p <= 38` and the scale `s` should be
      * `0 <= s <= p`
      */
    case Decimal128

    /** For some reason in bigquery the max precision is scale dependent which
      * means is does not match the Decimal128 that is common in other engines.
      * BigQuery only support the parameterized version in certain contexts,
      * e.g. column definitions but not as part of casts.
      *
      * For more details see:
      * https://docs.cloud.google.com/bigquery/docs/reference/standard-sql/data-types#numeric_types
      */
    case BigQuery(parameterized: Boolean)
  }

  enum DurationSupport {

    /** There is a native duration type that is a duration in microseconds */
    case Native(typeName: String)

    /** Duration is represented as a long integer of microseconds */
    case Long
  }

  val Spark = DdlDialect(
    decimal = DecimalSupport.Decimal128,
    duration = DurationSupport.Native("INTERVAL DAY TO SECOND"),
    map = MapSupport.Native(supportsNotNullKey = false, supportsNotNullValue = false),
    supportsNotNullColumn = true,
    supportsNotNullArrayElement = false,
    emptyStructFieldType = "void"
  )
}
