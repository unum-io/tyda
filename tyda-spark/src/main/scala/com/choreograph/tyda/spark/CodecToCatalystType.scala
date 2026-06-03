package com.choreograph.tyda.spark

import org.apache.spark.sql.types.*

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.shapeless3extras.mapConst

private object CodecToCatalystType {

  def catalystType[T](codec: Codec[T]): DataType =
    codec match {
      case Codec.Byte => ByteType
      case Codec.Short => ShortType
      case Codec.Int => IntegerType
      case Codec.Long => LongType
      case Codec.Float => FloatType
      case Codec.Double => DoubleType
      case Codec.Boolean => BooleanType
      case Codec.String => StringType
      case Codec.Bytes => BinaryType
      case Codec.Decimal(precision, scale) => DecimalType(precision, scale)
      case Codec.TimestampMicros => TimestampType
      case Codec.DurationMicros => DayTimeIntervalType()
      case Codec.Date => DateType
      case map: Codec.Map[?, ?] => MapType(catalystType(map.key), catalystType(map.value))
      case Codec.Seq(element) => ArrayType(catalystType(element))
      case Codec.Option(inner @ Codec.Option(_)) => StructType(Seq(StructField("value", catalystType(inner))))
      case opt: Codec.Option[?] => catalystType(opt.element)
      case Codec.Product(_, _, Some(_)) => StructType(Seq(StructField(Forbidden.column, NullType)))
      case prod: Codec.Product[T] => structFromFields(prod.fields.mapConst[Field[?]]([t] => identity(_)))
      case Codec.FromInjection(_, to) => catalystType(to)
    }

  def catalystStructType[T](codec: Codec.Product[T] | Codec.Sum[T, ?]): StructType =
    codec match {
      case prod: Codec.Product[T] => structFromFields(prod.fields.mapConst[Field[?]]([t] => identity(_)))
      case sum: Codec.Sum[T, ?] => structFromFields(sum.reprFields)
    }

  private def structFromFields(fields: Seq[Field[?]]): StructType =
    StructType(fields.map(field => StructField(field.name, catalystType(field.codec), nullable(field.codec))))

  /* This implement to match Spark view of nullability where anything that can have a `null` in the jvm is
   * considered nullable. In practice we only want types inside an Option to be nullable. */
  private[spark] def nullable[T](codec: Codec[T]): Boolean =
    codec match {
      case Codec.String | Codec.Bytes | Codec.Product(_, _, _) | Codec.Seq(_) | Codec.Option(_) |
          Codec.Map(_, _) => true
      case _: Codec.Primitive[?] | Codec.Sum(_, _) => false
      case Codec.FromInjection(_, to) => nullable(to)
    }
}
