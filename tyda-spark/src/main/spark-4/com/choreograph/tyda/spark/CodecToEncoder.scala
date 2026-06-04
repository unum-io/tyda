package com.choreograph.tyda.spark

import java.time.Instant
import java.time.LocalDate

import scala.reflect.classTag

import org.apache.spark.sql.Encoder
import org.apache.spark.sql.Row
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoder
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoders
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoders.EncoderField
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoders.OptionEncoder
import org.apache.spark.sql.catalyst.encoders.AgnosticEncoders.TransformingEncoder
import org.apache.spark.sql.catalyst.encoders.Codec as SparkCodec
import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
import org.apache.spark.sql.catalyst.expressions.GenericRow
import org.apache.spark.sql.catalyst.util.SparkDateTimeUtils
import org.apache.spark.sql.catalyst.util.SparkIntervalUtils
import org.apache.spark.sql.types.DecimalType
import org.apache.spark.sql.types.Metadata

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Field
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.Injection
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.spark.CodecToCatalystType.nullable

object CodecToEncoder {
  given convert[T: Codec]: Encoder[T] = convertInternal[T]

  private[spark] def convertInternal[T: Codec]: ExpressionEncoder[T] = ExpressionEncoder(toAgnostic(Codec[T]))

  private object TimestampSparkCodec extends SparkCodec[Timestamp, Instant] {
    def encode(value: Timestamp): Instant = SparkDateTimeUtils.microsToInstant(value.toMicros)
    // Using Instant.EPOCH.until(value, ChronoUnit.MICROS) will do calculation in nanos
    // So we use the spark implementation here which handles all edge cases.
    def decode(value: Instant): Timestamp = Timestamp.fromMicros(SparkDateTimeUtils.instantToMicros(value))
  }

  private object DateSparkCodec extends SparkCodec[Date, LocalDate] {
    def encode(value: Date): LocalDate = LocalDate.ofEpochDay(value.daysSinceEpoch.toLong)
    def decode(value: LocalDate): Date = Date.fromDays(value.toEpochDay.toInt)
  }

  private object DurationSparkCodec extends SparkCodec[Duration, java.time.Duration] {
    def encode(value: Duration): java.time.Duration = SparkIntervalUtils.microsToDuration(value.toMicros)
    def decode(value: java.time.Duration): Duration =
      Duration.fromMicros(SparkIntervalUtils.durationToMicros(value))
  }

  private class NullableSparkCodec[F, T](inner: SparkCodec[F, T]) extends SparkCodec[Option[F], Option[T]] {
    def encode(value: Option[F]): Option[T] = value.map(inner.encode)
    def decode(value: Option[T]): Option[F] = value.map(inner.decode)
  }

  private def makeNullable[T, F](
      transforming: TransformingEncoder[T, F]
  ): TransformingEncoder[Option[T], Option[F]] =
    TransformingEncoder(
      clsTag = classTag[Option[T]],
      transformed = OptionEncoder(transforming.transformed),
      codecProvider = () => NullableSparkCodec(transforming.codecProvider()),
      nullable = false
    )

  private[spark] def toAgnostic[T](codec: Codec[T]): AgnosticEncoder[T] =
    codec match {
      case Codec.Int => AgnosticEncoders.PrimitiveIntEncoder
      case Codec.Byte => AgnosticEncoders.PrimitiveByteEncoder
      case Codec.Short => AgnosticEncoders.PrimitiveShortEncoder
      case Codec.Long => AgnosticEncoders.PrimitiveLongEncoder
      case Codec.Float => AgnosticEncoders.PrimitiveFloatEncoder
      case Codec.Double => AgnosticEncoders.PrimitiveDoubleEncoder
      case Codec.Boolean => AgnosticEncoders.PrimitiveBooleanEncoder
      case Codec.String => AgnosticEncoders.StringEncoder
      case Codec.Bytes => AgnosticEncoders.BinaryEncoder
      case Codec.Decimal(precision, scale) =>
        // TYPE SAFETY: Cast is safe since tyda Decimal type is an opaque type for BigDecimal
        AgnosticEncoders.ScalaDecimalEncoder(DecimalType(precision, scale)).asInstanceOf[AgnosticEncoder[T]]
      // To get the logical type encoded correctly in parquet files we need to use the Spark build in
      // timestamp encoders. Maybe we should try upstream some way of encoding Long directly to timestamp
      // type.
      case Codec.TimestampMicros => TransformingEncoder(
          clsTag = codec.classTag,
          transformed = AgnosticEncoders.InstantEncoder(false),
          codecProvider = () => TimestampSparkCodec,
          nullable = false
        )
      case Codec.Date => TransformingEncoder(
          clsTag = codec.classTag,
          transformed = AgnosticEncoders.LocalDateEncoder(false),
          codecProvider = () => DateSparkCodec,
          nullable = false
        )
      case Codec.DurationMicros => TransformingEncoder(
          clsTag = codec.classTag,
          transformed = AgnosticEncoders.DayTimeIntervalEncoder,
          codecProvider = () => DurationSparkCodec,
          nullable = false
        )

      case Codec.Option(element @ Codec.Option(_)) => nestedOptionEncoder(element)

      case Codec.Option(element) => toAgnostic(element) match {
          // This is a workaround for an upstream issue. If this PR
          // https://github.com/apache/spark/pull/54506 gets merged we can remove this case
          case transforming: TransformingEncoder[?, ?] => makeNullable(transforming)
          case other => OptionEncoder(other)
        }

      case it: Codec.Iterable[?, ?] if sparkSupportedCollections.contains(it.classTag.runtimeClass) =>
        AgnosticEncoders.IterableEncoder(
          clsTag = it.classTag,
          element = toAgnostic(it.element),
          containsNull = nullable(it.element),
          false
        )

      case seq: Codec.Seq[?] => AgnosticEncoders.IterableEncoder(
          clsTag = seq.classTag,
          element = toAgnostic(seq.element),
          containsNull = nullable(seq.element),
          false
        )

      case map: Codec.Map[?, ?] => AgnosticEncoders.MapEncoder(
          clsTag = map.classTag,
          keyEncoder = toAgnostic(map.key),
          valueEncoder = toAgnostic(map.value),
          valueContainsNull = nullable(map.value)
        )

      case prod @ Codec.Product(_, _, Some(singleton)) => TransformingEncoder(
          clsTag = prod.classTag,
          transformed = AgnosticEncoders.RowEncoder(Seq(
            EncoderField(Forbidden.column, AgnosticEncoders.NullEncoder, true, Metadata.empty)
          )),
          codecProvider = () =>
            new SparkCodec[T, Row] {
              def encode(value: T): Row = new GenericRow(Array[Any](null))
              def decode(value: Row): T = singleton
            },
          nullable = false
        )

      case prod: Codec.Product[T] if allFieldsHaveJavaAccessor(prod) =>
        val fields = encoderFields(prod.fields.mapConst[Field[?]]([t] => identity(_)))
        AgnosticEncoders.ProductEncoder(clsTag = prod.classTag, fields = fields, outerPointerGetter = None)

      // This fallback handles products where the fields are not valid java accssors. For example NamedTuples
      // or other custom products.
      case prod @ Codec.Product(_, fields, _) =>
        val fieldsSeq = fields.mapConst[Field[?]]([t] => identity(_))
        val (nonTransformingEncoderFields, sparkCodecs, rawNullToOption) = encoderFields(fieldsSeq)
          .map {
            // This is a workaround for SPARK-55742 if https://github.com/apache/spark/pull/54539
            // is merged we should be able to remove this.
            case field @ EncoderField(_, TransformingEncoder(_, transformed, codecProvider, _), _, _, _, _) =>
              // TYPE SAFETY: When we use the codecs in ProductToRowEncoder we only call the codec for the
              // given index.
              val erasedCodec = codecProvider().asInstanceOf[SparkCodec[Any, Any]]
              transformed match {
                case OptionEncoder(inner) => (field.copy(enc = inner), Some(erasedCodec), true)
                case other => (field.copy(enc = other), Some(erasedCodec), false)
              }
            /* Because of the logic
             * https://github.com/apache/spark/blob/05b4d81f3f938ff140886d6f66ad66d08c66d5b2/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/DeserializerBuildHelper.scala#L462-L467
             * for RowEncoder Spark will return raw nulls instead of using the encoder. So we manually add the
             * option handling. */
            case field @ EncoderField(_, AgnosticEncoders.OptionEncoder(inner), _, _, _, _) =>
              (field.copy(enc = inner), None, true)
            case other => (other, None, false)
          }
          .unzip3

        TransformingEncoder(
          clsTag = codec.classTag,
          transformed = AgnosticEncoders.RowEncoder(nonTransformingEncoderFields),
          codecProvider = () => ProductToRowEncoder[T](prod, sparkCodecs.toArray, rawNullToOption.toArray),
          nullable = true
        )

      case sum: Codec.Sum[T, ?] => TransformingEncoder(
          clsTag = codec.classTag,
          transformed = AgnosticEncoders.RowEncoder(encoderFields(sum.reprFields)),
          codecProvider = () => SumToRowEncoder[T](sum),
          nullable = false
        )
      case Codec.FromInjection(inj, to) => TransformingEncoder(
          clsTag = codec.classTag,
          transformed = toAgnostic(to),
          codecProvider = () => injectionToSparkCodec(inj),
          nullable = nullable(to)
        )
    }

  private def allFieldsHaveJavaAccessor[T](prod: Codec.Product[T]): Boolean =
    prod
      .fields
      .foldLeft0(true)([t] =>
        (acc, field) => acc && isValidJavaAccessor(prod.classTag.runtimeClass, field.name)
      )

  final case class Wrapper[T](value: Option[T])
  private def nestedOptionEncoder[T](element: Codec.Option[T]): AgnosticEncoder[Option[Option[T]]] = {
    /* Because of https://github.com/apache/spark/pull/54366 we need to have an extra Option on the output
     * instead of directly encoding to the nullable value. */
    given Codec[Option[T]] = element
    TransformingEncoder(
      clsTag = classTag[Option[Option[T]]],
      transformed = toAgnostic(Codec[Option[Wrapper[T]]]),
      codecProvider = () =>
        new SparkCodec[Option[Option[T]], Option[Wrapper[T]]] {
          def encode(value: Option[Option[T]]): Option[Wrapper[T]] = value.map(Wrapper(_))
          def decode(value: Option[Wrapper[T]]): Option[Option[T]] = value.map(_._1)
        },
      nullable = true
    )
  }

  private def encoderFields(fields: Seq[Field[?]]): Seq[EncoderField] =
    fields.map(f => EncoderField(f.name, toAgnostic(f.codec), nullable(f.codec), Metadata.empty))

  private def injectionToSparkCodec[From, To](inj: Injection[From, To]): SparkCodec[From, To] =
    new SparkCodec[From, To] {
      def encode(value: From): To = inj(value)
      def decode(value: To): From = inj.invert(value)
    }

  /* Conservative List of collections directly supported by Spark. This is needed because Spark does not
   * correctly handle collections that takes an implicit parameter to their newBuilder method. */
  private val sparkSupportedCollections = Seq(classOf[List[?]], classOf[Set[?]], classOf[Seq[?]])
}
