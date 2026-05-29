package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.functions.col
import org.apache.spark.sql.types.ByteType
import org.apache.spark.sql.types.DataType
import org.apache.spark.sql.types.DoubleType
import org.apache.spark.sql.types.FloatType
import org.apache.spark.sql.types.IntegerType
import org.apache.spark.sql.types.LongType
import org.apache.spark.sql.types.NumericType
import org.apache.spark.sql.types.ShortType
import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.types.StructField

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.spark.CodecToEncoder.convert

object DataFrameOps {
  extension (df: DataFrame) {

    /** Unpivots the data based on the provided model.
      *
      * The input type `T` must have more than one field (n > 1). The first n \-
      * 2 fields will be treated as ID columns that remain unchanged during the
      * unpivot operation. The second-to-last column is expected to store the
      * column names (must be of type `String`), while the last column will hold
      * the corresponding values.
      *
      * @param maintainMetadata
      *   If set to `true`, the hidden `_metadata` column will be included as an
      *   ID column during the unpivot operation. This ensures that the
      *   `_metadata` column remains available after the unpivot. Note that this
      *   option can only be used if the `unpivotAs` method is called
      *   immediately after reading from disk.
      */
    def unpivotAs[T: Codec](maintainMetadata: Boolean = false): Dataset[T] = {
      val fields = Codec[T] match {
        case Codec.Product(_, fields, _) if fields.arity > 1 =>
          fields.mapConst[Field[?]]([t] => identity(_)).toArray
        case _ => throw new RuntimeException("Can only unpivot sources with at least 2 fields")
      }

      val idColumnsNames = fields.dropRight(2).map(_.name) ++ Seq("_metadata").filter(_ => maintainMetadata)
      val variableColumnField = fields.apply(fields.size - 2)
      assert(
        variableColumnField.codec == Codec.string,
        "Only String type is supported as second to last field in unpivotAs"
      )
      val valueColumnName = fields.last.name
      val (valueColumnType, valueColumnNullable) = convert[T]
        .schema
        .fields
        .find(_.name == valueColumnName)
        .map(f => (f.dataType, f.nullable))
        .head

      val valueColumns = df
        .schema
        .fields
        .filter(f => !idColumnsNames.contains(f.name))
        .map(f => castedColumnIfSupported(f, valueColumnType, valueColumnNullable))
      df.unpivot(idColumnsNames.map(col(_)), valueColumns, variableColumnField.name, valueColumnName).as[T]
    }
  }

  private object Integral {
    def unapply(dataType: DataType): Option[Int] =
      dataType match {
        case ByteType | ShortType | IntegerType | LongType => Some(dataType.defaultSize)
        case _ => None
      }
  }

  private object Fractional {
    def unapply(dataType: DataType): Option[Int] =
      dataType match {
        case FloatType | DoubleType => Some(dataType.defaultSize)
        case _ => None
      }
  }

  private def castedColumnIfSupported(field: StructField, toType: DataType, toNullable: Boolean): Column = {
    if (field.nullable && !toNullable) throw new RuntimeException(
      s"Unsupported unpivot from nullable field ${field.name} of type $toType to non nullable"
    )
    val column = col(field.name)
    (field.dataType, toType) match {
      case (from, to) if from == to => column
      case (_: NumericType, StringType) => column.cast(StringType)
      case (Integral(fromSize), Integral(toSize)) if fromSize < toSize => column.cast(toType)
      case (Fractional(fromSize), Fractional(toSize)) if fromSize < toSize => column.cast(toType)
      case (from, to) =>
        throw new RuntimeException(s"Unsupported unpivot \"$column\" of type from $from to $to")
    }
  }
}
