package com.choreograph.tyda.bigquery

import scala.jdk.CollectionConverters.*

import com.google.cloud.bigquery.Field
import com.google.cloud.bigquery.FieldList
import com.google.cloud.bigquery.FieldValue
import com.google.cloud.bigquery.FieldValueList

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.NumericLimits
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.bigquery.BigQueryCollectionRewrites.InvertAndCodec
import com.choreograph.tyda.rewrite.CollectionOrNullableCollectionCodec

/** Create a decoder based on a Codec and a BigQuery schema.
  *
  * The schema is needed because of this bug:
  * https://github.com/googleapis/java-bigquery/issues/3389 Which means that we
  * can not look up fields by name for structs inside arrays. We do not want to
  * build de indices based on our Codec to allow fields to be reordered.
  */
def createDecoder[T](codec: Codec[T], schema: FieldList): FieldValueList => T =
  codec match {
    case Codec.Product(_, fields, _) =>
      val decoders = fields.mapK[[X] =>> (Int, FieldValue => X)] { [t] => f =>
        val index = schema.getIndex(f.name)
        (index, fieldDecoder(f.codec, schema.get(index)))
      }
      list =>
        decoders.construct { [t] => (indexAndDecoder) =>
          val (index, decoder) = indexAndDecoder
          decoder(list.get(index))
        }
    case Codec.FromInjection(inj, inner) => createDecoder(inner, schema).andThen(inj.invert)
    case BigQueryCollectionRewrites(InvertAndCodec(invert, inner)) =>
      createDecoder(inner, schema).andThen(invert)
    case other =>
      val deserializer = fieldDecoder(other, schema.get(0))
      list => deserializer(list.get(0))
  }

private def fieldDecoder[T](codec: Codec[T], field: Field): FieldValue => T = {
  def checkedConversion[A: {NumericLimits as limits, Numeric as numeric}](value: Long): A = {
    import numeric.mkNumericOps
    if (value < limits.min.toLong || value > limits.max.toLong)
      throw new RuntimeException(s"Value $value is out of range for target type")
    numeric.fromInt(value.toInt)
  }
  codec match {
    case Codec.Boolean => _.getBooleanValue
    case Codec.Byte => fv => checkedConversion[Byte](fv.getLongValue)
    case Codec.Short => fv => checkedConversion[Short](fv.getLongValue)
    case Codec.Int => fv => checkedConversion[Int](fv.getLongValue)
    case Codec.Long => _.getLongValue
    case Codec.Float => _.getDoubleValue.toFloat
    case Codec.Double => _.getDoubleValue
    case Codec.String => _.getStringValue
    case Codec.Bytes => _.getBytesValue
    case decimal @ Codec.Decimal(precision, scale) => fv =>
        Decimal(using decimal.valid)(fv.getNumericValue).getOrElse(throw new RuntimeException(s"Value ${fv
            .getNumericValue} cannot be represented as Decimal($precision, $scale)"))
    case Codec.TimestampMicros => fv => Timestamp.fromMicros(fv.getTimestampValue)
    case Codec.DurationMicros => fv => Duration.fromMicros(fv.getLongValue)
    case Codec.Date => fv =>
        Date
          .fromIsoString(fv.getStringValue)
          .getOrElse(throw new RuntimeException(s"Invalid date string: ${fv.getStringValue}"))
    case BigQueryCollectionRewrites(InvertAndCodec(invert, inner)) =>
      fieldDecoder(inner, field).andThen(invert)
    case Codec.Option(element @ Codec.Option(_)) if !BigQueryCollectionRewrites.matches(element) =>
      val inner = fieldDecoder(element, field.getSubFields.get(0))
      fv => Option.when(!fv.isNull)(inner(fv.getRecordValue().get(0)))
    case Codec.Option(element) =>
      val inner = fieldDecoder(element, field)
      fv => Option.when(!fv.isNull)(inner(fv))
    // BigQuery does not support (nullable) array type as element, so we insert a wrapper struct
    case Codec.Seq(CollectionOrNullableCollectionCodec(given Codec[e]))
        if !BigQueryCollectionRewrites.matches(Codec[e]) =>
      val actual = fieldDecoder[Seq[(value: e)]](summon, field)
      fv => actual(fv).map(_.value)
    case Codec.Seq(element) =>
      val inner = fieldDecoder(element, field)
      fv => fv.getRepeatedValue.asScala.map(inner).toIndexedSeq
    case Codec.Map(keyCodec, valueCodec) =>
      val subFields = field.getSubFields
      val keyDecoder = fieldDecoder(keyCodec, subFields.get(0))
      val valueDecoder = fieldDecoder(valueCodec, subFields.get(1))
      fv =>
        fv.getRepeatedValue
          .asScala
          .map { entry =>
            val entryRecord = entry.getRecordValue
            val key = keyDecoder(entryRecord.get(0))
            val value = valueDecoder(entryRecord.get(1))
            (key, value)
          }
          .toMap
    case codec @ (Codec.Product(_, _, _) | Codec.Sum(_, _)) =>
      val decoder = createDecoder(codec, field.getSubFields)
      fv => decoder(fv.getRecordValue())
    case Codec.FromInjection(inj, inner) => fieldDecoder(inner, field).andThen(inj.invert)
  }
}
