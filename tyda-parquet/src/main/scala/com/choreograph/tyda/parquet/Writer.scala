package com.choreograph.tyda.parquet

import java.nio.charset.StandardCharsets

import org.apache.parquet.io.api.Binary
import org.apache.parquet.io.api.RecordConsumer

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.shapeless3extras.mapConst

private type Writer[T] = (RecordConsumer, T) => Unit

private object Writer {

  def create[T](codec: Codec[T]): Writer[T] =
    codec match {
      case prod @ Codec.Product(_) =>
        val fieldsWriters = productFieldsWriter(prod)
        (consumer, prodValue) =>
          consumer.startMessage()
          fieldsWriters(consumer, prodValue)
          consumer.endMessage()
      case Codec.FromInjection(inj, innerCodec) =>
        val inner = create(innerCodec)
        (consumer, value) => inner(consumer, inj(value))
      case _ =>
        val inner = fieldWriter(codec, "value", 0)
        (consumer, value) => {
          consumer.startMessage()
          inner(consumer, value)
          consumer.endMessage()
        }
    }

  private def fieldWriter[T](codec: Codec[T], name: String, index: Int): Writer[T] = {
    val valueWriter = impl(codec)
    withNoneCheck(
      codec,
      (consumer, value) =>
        consumer.startField(name, index)
        valueWriter(consumer, value)
        consumer.endField(name, index)
    )
  }

  private def withNoneCheck[T](codec: Codec[T], writer: Writer[T]): Writer[T] =
    codec match {
      case Codec.Option(_) => (consumer, value) => value.foreach(_ => writer(consumer, value))
      case _ => writer
    }

  private def impl[T](codec: Codec[T]): Writer[T] =
    codec match {
      case Codec.Boolean => (consumer, value) => consumer.addBoolean(value)
      case Codec.Byte => (consumer, value) => consumer.addInteger(value.toInt)
      case Codec.Short => (consumer, value) => consumer.addInteger(value.toInt)
      case Codec.Int => (consumer, value) => consumer.addInteger(value)
      case Codec.Long => (consumer, value) => consumer.addLong(value)
      case Codec.Float => (consumer, value) => consumer.addFloat(value)
      case Codec.Double => (consumer, value) => consumer.addDouble(value)
      case Codec.String => (consumer, value) =>
          consumer.addBinary(Binary.fromConstantByteArray(value.getBytes(StandardCharsets.UTF_8)))
      case Codec.Bytes => (consumer, value) => consumer.addBinary(Binary.fromConstantByteArray(value.toArray))
      case Codec.DurationMicros => (consumer, value) => consumer.addLong(value.toMicros)
      case Codec.Date => (consumer, value) => consumer.addInteger(value.daysSinceEpoch)
      case decimal: Codec.Decimal[?, ?] =>
        if (decimal.precision <= 9) then
          (consumer, value) => consumer.addInteger(BigInt(value.toBigDecimal.underlying.unscaledValue).toInt)
        else if (decimal.precision <= 19) then
          (consumer, value) => consumer.addLong(BigInt(value.toBigDecimal.underlying.unscaledValue).toLong)
        else
          (consumer, value) =>
            consumer.addBinary(
              Binary.fromConstantByteArray(value.toBigDecimal.underlying.unscaledValue.toByteArray)
            )
      case Codec.TimestampMicros => (consumer, value) => consumer.addLong(value.toMicros)
      case Codec.Option(element @ Codec.Option(_)) =>
        val innerFieldWriter = fieldWriter(element, "value", 0)
        (consumer, value) =>
          value.foreach { innerValue =>
            consumer.startGroup()
            innerFieldWriter(consumer, innerValue)
            consumer.endGroup()
          }
      case Codec.Option(element) =>
        val elementWriter = impl(element)
        (consumer, value) => value.foreach(elementWriter(consumer, _))
      case Codec.FromInjection(inj, inner) =>
        val innerWriter = impl(inner)
        (consumer, value) => innerWriter(consumer, inj(value))
      case Codec.Seq(elementCodec) => iterable("list", fieldWriter(elementCodec, "element", 0))
      case Codec.Map(key, value) => map(using key, value)
      case prod @ Codec.Product(_) =>
        val fieldsWriters = productFieldsWriter(prod)
        (consumer, prodValue) =>
          consumer.startGroup()
          fieldsWriters(consumer, prodValue)
          consumer.endGroup()
    }

  private def map[K: Codec, V: Codec]: Writer[Map[K, V]] =
    iterable("key_value", productFieldsWriter[(key: K, value: V)](summon))

  private def iterable[T, C <: Iterable[T]](
      repeatedFieldName: String,
      elementWriter: Writer[T]
  ): Writer[C] = { (consumer, values) =>
    consumer.startGroup()
    if values.size > 0 then {
      consumer.startField(repeatedFieldName, 0)
      values.foreach { element =>
        consumer.startGroup()
        elementWriter(consumer, element)
        consumer.endGroup()
      }
      consumer.endField(repeatedFieldName, 0)
    }
    consumer.endGroup()
  }

  private def productFieldsWriter[T](prod: Codec.Product[T]): Writer[T] = {
    val nameToIndex = prod.fields.mapConst[String]([t] => _.name).zipWithIndex.toMap
    val fieldsWriters = prod
      .fields
      .mapK[Writer]([t] => (f: Field[t]) => fieldWriter(f.codec, f.name, nameToIndex(f.name)))
    (consumer, prodValue) =>
      fieldsWriters.foldLeft(prodValue)(())([t] => (_, writer, value) => writer(consumer, value))
  }
}
