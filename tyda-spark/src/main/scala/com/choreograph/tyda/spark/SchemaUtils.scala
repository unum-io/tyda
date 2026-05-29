package com.choreograph.tyda.spark

import org.apache.spark.sql.types.ArrayType
import org.apache.spark.sql.types.DataType
import org.apache.spark.sql.types.MapType
import org.apache.spark.sql.types.StructType

/* On Spark 4, we should be able to use StructType.toNullable instead
 * https://github.com/apache/spark/blob/4e5ed454fb292bc22cbdb6fc69b7de322e0afeff/sql/api/src/main/scala/org/apache/spark/sql/types/StructType.scala#L498 */
private[tyda] def withAllNullable(dataType: DataType): DataType =
  dataType match {
    case StructType(fields) =>
      StructType(fields.map(field => field.copy(nullable = true, dataType = withAllNullable(field.dataType))))
    case ArrayType(elementType, _) => ArrayType(withAllNullable(elementType))
    case MapType(key, value, _) => MapType(withAllNullable(key), withAllNullable(value))
    case other => other
  }
