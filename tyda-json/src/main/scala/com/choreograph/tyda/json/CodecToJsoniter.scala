package com.choreograph.tyda.json

import java.math.MathContext
import java.time.Instant
import java.time.OffsetDateTime
import java.time.temporal.ChronoUnit

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReader
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.core.JsonWriter

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Field
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.Variant
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.unreachable

object CodecToJsoniter {

  /** Create a JsonValueCodec that makes the format used by `Format.Json` in the
    * Dataset apis.
    *
    * This means that top level values that are not objects will be wrapped in
    * an object with a single "value" field.
    */
  def create[T: Codec]: JsonValueCodec[T] =
    Codec[T] match {
      case Codec.Product(_, _, _) => fromReaderWriter[T]
      case Codec.FromInjection(inj, to) => xmap(create(using to), inj.apply, inj.invert)
      case _ => xmap(create[(value: T)], (value = _), _.value)
    }

  private def getNullValue[T: Codec]: T =
    Codec[T] match {
      case Codec.Boolean => false
      case Codec.Byte => 0.toByte
      case Codec.Short => 0.toShort
      case Codec.Int => 0
      case Codec.Long => 0L
      case Codec.Float => 0.0f
      case Codec.Double => 0.0
      case Codec.String => ""
      case Codec.Bytes =>
        // This can not currently be empty because of it being used with BigInt
        // TODO: Clean up after BigInt usage is replaced with Decimal(38, 0)
        Array(0)
      case codec @ Codec.Decimal(_, _) => Decimal.zero(using codec.valid)
      case Codec.Date => Date.fromDays(0)
      case Codec.TimestampMicros => Timestamp.fromMicros(0)
      case Codec.DurationMicros => Duration.fromMicros(0)
      case Codec.Option(_) => None
      case Codec.Seq(_: Codec[e]) => Seq.empty[e]
      case Codec.Map(_: Codec[k], _: Codec[v]) => Map.empty[k, v]
      case Codec.Product(_, fields, _) => fields.construct([t] => f => getNullValue(using f.codec))
      // Sum can not use injection as the encoded null value will not have a valid discriminat
      case Codec.Sum(_, variants) => variants
          .mapConst[T]([t <: T] =>
            _ match {
              case Variant.Singleton(_, value, _) => value
              case Variant.Product(_, codec) => getNullValue(using codec)
            }
          )
          .head
      case Codec.SumAsString(_, singletons) => singletons.head
      case Codec.FromInjection(inj, to) => inj.invert(getNullValue(using to))
    }

  private def fromReaderWriter[T: Codec]: JsonValueCodec[T] =
    new JsonValueCodec[T] {
      private val read = reader(summon[Codec[T]])
      private val write = writer(summon[Codec[T]])

      override def decodeValue(in: JsonReader, default: T): T = read(in)

      override def encodeValue(x: T, out: JsonWriter): Unit = write(out, x)

      override val nullValue: T = getNullValue[T]
    }

  private def xmap[T: Codec, A](inner: JsonValueCodec[A], to: T => A, from: A => T): JsonValueCodec[T] =
    new JsonValueCodec[T] {
      override def decodeValue(in: JsonReader, default: T): T = from(inner.decodeValue(in, inner.nullValue))
      override def encodeValue(x: T, out: JsonWriter): Unit = inner.encodeValue(to(x), out)
      override val nullValue: T = getNullValue[T]
    }

  private type Reader[T] = JsonReader => T
  private type Writer[T] = (JsonWriter, T) => Unit

  extension (in: JsonReader)
    inline def nextValueIsString(): Boolean = {
      val cond = in.isNextToken('"')
      in.rollbackToken()
      cond
    }

  private def reader[T](codec: Codec[T]): Reader[T] =
    codec match {
      case Codec.Boolean => _.readBoolean()
      case Codec.Byte => in => if in.nextValueIsString() then in.readStringAsByte() else in.readByte()
      case Codec.Short => in => if in.nextValueIsString() then in.readStringAsShort() else in.readShort()
      case Codec.Int => in => if in.nextValueIsString() then in.readStringAsInt() else in.readInt()
      case Codec.Long => in => if in.nextValueIsString() then in.readStringAsLong() else in.readLong()
      case Codec.Float => in => if in.nextValueIsString() then in.readString(null).toFloat else in.readFloat()
      case Codec.Double =>
        in => if in.nextValueIsString() then in.readString(null).toDouble else in.readDouble()
      case Codec.String => _.readString(null)
      case Codec.Bytes => _.readBase64AsBytes(null)
      case codec @ Codec.Decimal(precision, scale) =>
        import codec.given
        in =>
          val bigDecimal =
            if in.nextValueIsString() then
              in.readStringAsBigDecimal(null, MathContext.UNLIMITED, scale + 1, precision + 1)
            else in.readBigDecimal(null, MathContext.UNLIMITED, scale + 1, precision + 1)
          Decimal(bigDecimal).getOrElse(in.decodeError(
            s"Value $bigDecimal is out of range for Decimal($precision, $scale)"
          ))
      case Codec.Date =>
        in => Date.fromDays(in.readLocalDate(null).toEpochDay().toInt) // TODO: Check overflow
      case Codec.TimestampMicros => in =>
          val datetime = in.readOffsetDateTime(null)
          Timestamp
            .fromInstant(datetime.toInstant)
            .getOrElse(in.decodeError(s"Value $datetime is out of range for Timestamp"))
      case Codec.DurationMicros =>
        val longReader = reader(Codec.Long)
        in => Duration.fromMicros(longReader(in))
      case Codec.Option(element @ Codec.Option(_)) => nestedOptionReader(element)
      case Codec.Option(element) =>
        val inner = reader(element)
        in =>
          if in.isNextToken('n') then in.readNullOrError(None, "expected null or value")
          else {
            in.rollbackToken()
            Some(inner(in))
          }

      case Codec.Seq(element) => seqReader(element)
      case Codec.Map(given Codec[k], given Codec[v]) =>
        val keyValueReader = reader[Seq[(key: k, value: v)]](summon)
        // We should probably error on duplicates here
        in => keyValueReader(in).map { case (k, v) => k -> v }.toMap
      case codec @ Codec.Product(_) => productReader(codec)
      case Codec.FromInjection(inj, to) => reader(to).andThen(inj.invert)
    }

  extension (in: JsonReader) {
    private def tokenError(expected: Byte): Nothing = {
      in.readNullOrTokenError[Null](null, expected)
      unreachable("readNullOrTokenError should always throw when passed null as the default value")
    }
  }

  private def nestedOptionReader[T](element: Codec.Option[T]): Reader[Option[Option[T]]] = {
    given Codec[T] = element.element
    val structReader = reader[Option[(value: Option[T])]](summon)
    in => structReader(in).map(_.value)
  }

  private def seqReader[T](elementCodec: Codec[T]): Reader[Seq[T]] = {
    val elementReader = reader(elementCodec)
    in =>
      if !in.isNextToken('[') then in.tokenError('[')
      else if in.isNextToken(']') then Seq.empty
      else {
        in.rollbackToken()
        val builder = Vector.newBuilder[T]
        while {
          builder += elementReader(in)
          in.isNextToken(',')
        } do ()
        if !in.isCurrentToken(']') then in.arrayEndOrCommaError()
        builder.result()
      }
  }

  private def productReader[T](codec: Codec.Product[T]): Reader[T] = {
    val fields = codec.fields.mapConst[Field[?]]([t] => identity(_)).toArray
    val readers = fields.map(_.codec).map(reader)
    val required = fields.map(!_.codec.isInstanceOf[Codec.Option[?]])
    val nameToIndex = fields.map(_.name).zipWithIndex.toMap
    in =>
      def fromValues(values: Array[Any]): T = {
        required.foldLeft(0) { (index, isRequired) =>
          if values(index) == null then
            if isRequired then in.decodeError("missing required field " + fields(index).name)
            else values(index) = None
          index + 1
        }: Unit
        codec.fromProduct(Tuple.fromArray(values))
      }
      if !in.isNextToken('{') then in.tokenError('{')
      else if in.isNextToken('}') then fromValues(new Array[Any](fields.size))
      else {
        in.rollbackToken()
        val values = new Array[Any](fields.size)
        while {
          val fieldName = in.readKeyAsString()
          nameToIndex.get(fieldName) match {
            case Some(index) =>
              if values(index) != null then in.decodeError("duplicate key " + fieldName)
              values(index) = readers(index)(in)
            case _ => in.skip()
          }
          val c = in.isNextToken(',')
          c
        } do ()
        if !in.isCurrentToken('}') then in.objectEndOrCommaError()
        fromValues(values)
      }
  }

  private def writer[T](codec: Codec[T]): Writer[T] =
    codec match {
      case Codec.Boolean => _.writeVal(_)
      case Codec.Byte => _.writeVal(_)
      case Codec.Short => _.writeVal(_)
      case Codec.Int => _.writeVal(_)
      case Codec.Long => _.writeVal(_)
      case Codec.Float =>
        (out, value) => if value.isFinite then out.writeVal(value) else out.writeVal(value.toString)
      case Codec.Double =>
        (out, value) => if value.isFinite then out.writeVal(value) else out.writeVal(value.toString)
      case Codec.String => (out, value) => out.writeVal(value)
      case Codec.Bytes => (out, value) => out.writeBase64Val(value, doPadding = true)
      case Codec.Decimal(_, _) => (out, value) => out.writeValAsString(value.toBigDecimal)
      case Codec.Date =>
        (out, value) => out.writeVal(java.time.LocalDate.ofEpochDay(value.daysSinceEpoch.toLong))
      case Codec.TimestampMicros => (out, value) =>
          val instant = Instant.EPOCH.plus(value.toMicros, ChronoUnit.MICROS)
          out.writeVal(OffsetDateTime.ofInstant(instant, java.time.ZoneOffset.UTC))
      case Codec.DurationMicros =>
        val longWriter = writer(Codec.Long)
        (out, value) => longWriter(out, value.toMicros)
      case Codec.Option(element @ Codec.Option(_)) =>
        val writeElement = writer(element)
        (out, value) =>
          value match {
            case Some(innerValue) =>
              out.writeObjectStart()
              out.writeKey("value")
              writeElement(out, innerValue)
              out.writeObjectEnd()
            case None => out.writeNull()
          }
      case Codec.Option(element) =>
        val writeElement = writer(element)
        (out, value) =>
          value match {
            case Some(innerValue) => writeElement(out, innerValue)
            case None => out.writeNull()
          }
      case Codec.Seq(element) =>
        val writeElement = writer(element)
        (out, value) =>
          out.writeArrayStart()
          value.foreach(writeElement(out, _))
          out.writeArrayEnd()
      case Codec.Map(given Codec[k], given Codec[v]) =>
        val writeKeyValue = writer[Seq[(key: k, value: v)]](summon)
        (out, value) =>
          val keyValuePairs = value.iterator.map { case (k, v) => k -> v }.toIndexedSeq
          writeKeyValue(out, keyValuePairs)
      case Codec.Product(_, fields, _) =>
        val writers = fields.mapK[[X] =>> (String, Writer[X])]([t] => f => f.name -> writer(f.codec))
        (out, prod) =>
          out.writeObjectStart()
          writers
            .foldLeft(prod)(out) { [t] => (out, nameAndWriter, value) =>
              val (fieldName, writer) = nameAndWriter
              out.writeKey(fieldName)
              writer(out, value)
              out
            }
            .writeObjectEnd()

      case Codec.FromInjection(inj, to) =>
        val inner = writer(to)
        (out, value) => inner(out, inj(value))
    }
}
