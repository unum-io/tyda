package com.choreograph.tyda.parquet

import org.apache.parquet.example.data.Group
import org.apache.parquet.schema.GroupType
import org.apache.parquet.schema.LogicalTypeAnnotation.DecimalLogicalTypeAnnotation
import org.apache.parquet.schema.MessageType
import org.apache.parquet.schema.PrimitiveType
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName
import org.apache.parquet.schema.Type

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Timestamp

/* This object provides codec for decoding a Group in to the actual scala class using the type class
 * [[Codec]]. This implementation is not optimized for performance an is only intended to be used with small
 * datasets in tests. */
private object GroupDecoder {

  def apply[T](codec: Codec[T], fileSchema: MessageType): Group => T =
    codec match {
      case prod: Codec.Product[T] => product(prod, fileSchema.asGroupType())
      case Codec.FromInjection(inj, to) => apply(to, fileSchema).andThen(inj.invert)
      case _ => field("value", codec, fileSchema.asGroupType())
    }

  def field[T](fieldName: String, fieldCodec: Codec[T], groupType: Type): Group => T =
    field(fieldName, fieldCodec, groupType.asGroupType())

  def field[T](fieldName: String, fieldCodec: Codec[T], groupType: GroupType): Group => T =
    (fieldCodec, groupType.containsField(fieldName)) match {
      case (Codec.Option(_), false) => _ => None
      case _ => impl(fieldName, fieldCodec, groupType.getType(fieldName))
    }

  private def impl[T](fieldName: String, codec: Codec[T], parquetType: Type): Group => T =
    codec match {
      case Codec.Boolean => _.getBoolean(fieldName, 0)
      case Codec.Byte => _.getInteger(fieldName, 0).toByte
      case Codec.Short => _.getInteger(fieldName, 0).toShort
      case Codec.Int => _.getInteger(fieldName, 0)
      case Codec.Long => _.getLong(fieldName, 0)
      case Codec.Float => _.getFloat(fieldName, 0)
      case Codec.Double => _.getDouble(fieldName, 0)
      case Codec.String => _.getString(fieldName, 0)
      case d: Codec.Decimal[?, ?] => decimal(d, fieldName, parquetType.asPrimitiveType)
      case Codec.TimestampMicros => group => Timestamp.fromMicros(group.getLong(fieldName, 0))
      case Codec.DurationMicros => group => Duration.fromMicros(group.getLong(fieldName, 0))
      case Codec.Date => group => Date.fromDays(group.getInteger(fieldName, 0))
      case Codec.Option(inner) =>
        val valueDecoder = inner match {
          case Codec.Option(_) =>
            field("value", inner, parquetType).compose((g: Group) => g.getGroup(fieldName, 0))
          case _ => impl(fieldName, inner, parquetType)
        }
        group =>
          if (group.getFieldRepetitionCount(fieldName) == 1) { Some(valueDecoder(group)) } else { None }
      case Codec.Seq(element) =>
        val elementDecoder = field("element", element, parquetType.asGroupType().getType("list"))
        group => {
          val listGroup = group.getGroup(fieldName, 0)
          val elementCount = listGroup.getFieldRepetitionCount("list")
          (0 until elementCount).iterator.map(i => elementDecoder(listGroup.getGroup("list", i))).toIndexedSeq
        }
      case map: Codec.Map[?, ?] =>
        val groupType = parquetType.asGroupType().getType("key_value")
        val key = field("key", map.key, groupType)
        val value = field("value", map.value, groupType)
        group => {
          val mapGroup = group.getGroup(fieldName, 0)
          val elementCount = mapGroup.getFieldRepetitionCount("key_value")
          (0 until elementCount)
            .iterator
            .map(i =>
              val keyValueGroup = mapGroup.getGroup("key_value", i)
              (key(keyValueGroup), value(keyValueGroup))
            )
            .toMap
        }
      case prod: Codec.Product[T] => product(prod, parquetType).compose(_.getGroup(fieldName, 0))
      case Codec.Bytes => group => Binary.fromArray(group.getBinary(fieldName, 0).getBytes())
      case inj: Codec.FromInjection[T, ?] =>
        val inner = impl(fieldName, inj.to, parquetType)
        group => inj.inj.invert(inner(group))
    }

  private def isInt32(parquetType: PrimitiveType): Boolean =
    parquetType.getPrimitiveTypeName == PrimitiveTypeName.INT32

  private def isInt64(parquetType: PrimitiveType): Boolean =
    parquetType.getPrimitiveTypeName == PrimitiveTypeName.INT64

  private def isBinary(parquetType: PrimitiveType): Boolean =
    parquetType.getPrimitiveTypeName == PrimitiveTypeName.FIXED_LEN_BYTE_ARRAY ||
      parquetType.getPrimitiveTypeName == PrimitiveTypeName.BINARY

  private def decimal[P <: Int, S <: Int](
      decimal: Codec.Decimal[P, S],
      fieldName: String,
      parquetType: PrimitiveType
  ): Group => Decimal[P, S] =
    given valid: Decimal.Valid[P, S] = decimal.valid
    val (precision, scale) = parquetType.getLogicalTypeAnnotation() match {
      case dec: DecimalLogicalTypeAnnotation => (dec.getPrecision(), dec.getScale())
      case _ => throw new IllegalArgumentException(
          s"Unable to read decimal without decimal logical type annotation. Parquet type: $parquetType"
        )
    }
    if (precision > valid.precision || scale > valid.scale) throw new IllegalArgumentException(
      s"Parquet decimal precision/scale ($precision,$scale) exceeds codec" +
        s"supported precision/scale (${valid.precision},${valid.scale})"
    )
    def makeChecked(value: BigDecimal): Decimal[P, S] =
      Decimal[P, S](value).getOrElse {
        val msg = s"Decimal value $value exceeds limits for Decimal(${valid.precision}, ${valid.scale})"
        throw new RuntimeException(msg)
      }
    if isInt32(parquetType) then g => makeChecked(BigDecimal(g.getInteger(fieldName, 0), scale))
    else if isInt64(parquetType) then g => makeChecked(BigDecimal(g.getLong(fieldName, 0), scale))
    else if isBinary(parquetType) then
      g => makeChecked(BigDecimal(BigInt(g.getBinary(fieldName, 0).getBytes), scale))
    else throw new IllegalArgumentException(s"Unsupported parquet type for decimal: $parquetType")

  private def product[T](prod: Codec.Product[T], parquetType: Type): Group => T = {
    val fieldDecoders = prod.fields.mapK[[X] =>> Group => X]([t] => f => field(f.name, f.codec, parquetType))
    group => fieldDecoders.construct([t] => decoder => decoder(group))
  }
}
