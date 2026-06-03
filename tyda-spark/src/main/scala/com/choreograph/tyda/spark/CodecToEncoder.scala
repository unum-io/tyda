package com.choreograph.tyda.spark

import java.time.Duration

import scala.collection.Factory
import scala.deriving.Mirror
import scala.reflect.ClassTag

import javax.lang.model.SourceVersion
import org.apache.commons.lang3.reflect.ConstructorUtils
import org.apache.commons.lang3.reflect.MethodUtils
import org.apache.spark.sql.Encoder
import org.apache.spark.sql.Row
import org.apache.spark.sql.catalyst.DeserializerBuildHelper
import org.apache.spark.sql.catalyst.DeserializerBuildHelper.expressionWithNullSafety
import org.apache.spark.sql.catalyst.SerializerBuildHelper
import org.apache.spark.sql.catalyst.WalkedTypePath
import org.apache.spark.sql.catalyst.analysis.GetColumnByOrdinal
import org.apache.spark.sql.catalyst.analysis.UnresolvedExtractValue
import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
import org.apache.spark.sql.catalyst.expressions.*
import org.apache.spark.sql.catalyst.expressions.objects.AssertNotNull
import org.apache.spark.sql.catalyst.expressions.objects.CreateExternalRow
import org.apache.spark.sql.catalyst.expressions.objects.ExternalMapToCatalyst
import org.apache.spark.sql.catalyst.expressions.objects.GetExternalRowField
import org.apache.spark.sql.catalyst.expressions.objects.Invoke
import org.apache.spark.sql.catalyst.expressions.objects.MapObjects
import org.apache.spark.sql.catalyst.expressions.objects.NewInstance
import org.apache.spark.sql.catalyst.expressions.objects.StaticInvoke
import org.apache.spark.sql.catalyst.expressions.objects.UnresolvedCatalystToExternalMap
import org.apache.spark.sql.catalyst.expressions.objects.UnresolvedMapObjects
import org.apache.spark.sql.catalyst.expressions.objects.UnwrapOption
import org.apache.spark.sql.catalyst.expressions.objects.ValidateExternalType
import org.apache.spark.sql.catalyst.expressions.objects.WrapOption
import org.apache.spark.sql.catalyst.util.IntervalUtils
import org.apache.spark.sql.types.*

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Field
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.Injection
import com.choreograph.tyda.Variant
import com.choreograph.tyda.shapeless3extras.mapConst

/* This object currently contains code vendored from Spark 3.5.4 mainly from the files
 * https://github.com/apache/spark/blob/v3.5.4/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/DeserializerBuildHelper.scala
 * https://github.com/apache/spark/blob/v3.5.4/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/SerializerBuildHelper.scala
 *
 * The reason we need to vendor this code is that we need to hook into the type traversal so that we can
 * provide our own encoders for types like enums. When we are using Spark > 4.0.0 we should instead be able
 * just to convert of `Codec[T]` class into an `AgnosticEncoder[T]` but we will need the
 * `TransformingEncoder[T]` in order to support custom types in that approach. */
object CodecToEncoder {
  given convert[T: Codec]: Encoder[T] = convertInternal[T]

  private[spark] def convertInternal[T: Codec]: ExpressionEncoder[T] =
    new ExpressionEncoder[T](createSerializer(Codec[T]), createDeserializer(Codec[T]), Codec[T].classTag)

  private def jvmType[T](codec: Codec[T]): DataType =
    codec match {
      case Codec.Byte => ByteType
      case Codec.Short => ShortType
      case Codec.Int => IntegerType
      case Codec.Long => LongType
      case Codec.Float => FloatType
      case Codec.Double => DoubleType
      case Codec.Boolean => BooleanType
      case Codec.Bytes => BinaryType
      case Codec.TimestampMicros => LongType
      case Codec.DurationMicros => LongType
      case Codec.Date => IntegerType
      case Codec.String | Codec.Seq(_) | Codec.Map(_, _) | Codec.Option(_) | Codec.Decimal(_, _) | Codec
            .Product(_, _, _) | Codec.FromInjection(_, _) => ObjectType(codec.classTag.runtimeClass)
    }

  private[tyda] def catalystType[T](codec: Codec[T]): DataType =
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

  private def sumSchema[T](sum: Codec.Sum[T, ?]): StructType = structFromFields(sum.reprFields)

  private def structFromFields(fields: Seq[Field[?]]): StructType =
    StructType(fields.map(field => StructField(field.name, catalystType(field.codec), nullable(field.codec))))

  /* This implement to match Spark view of nullability where anything that can have a `null` in the jvm is
   * considered nullable. In practice we only want types inside an Option to be nullable. */
  private[spark] def nullable[T](codec: Codec[T]): Boolean =
    codec match {
      case _: Codec.Sum[T, ?] => false
      case _: Codec.Decimal[?, ?] => false
      case _ => jvmType(codec).isInstanceOf[ObjectType]
    }

  private def createSerializer[T](codec: Codec[T]): Expression = {
    val input = BoundReference(0, jvmType(codec), nullable = nullable(codec))
    createSerializer(codec, input)
  }

  private def createSerializer[T](codec: Codec[T], input: Expression): Expression =
    codec match {
      case Codec.Byte => input
      case Codec.Short => input
      case Codec.Int => input
      case Codec.Long => input
      case Codec.Float => input
      case Codec.Double => input
      case Codec.Boolean => input
      case Codec.Bytes => input
      case Codec.TimestampMicros => MicrosToTimestamp(input)
      case Codec.DurationMicros =>
        val duration = StaticInvoke(
          IntervalUtils.getClass,
          ObjectType(classOf[Duration]),
          "microsToDuration",
          input :: Nil,
          returnNullable = false
        )
        SerializerBuildHelper.createSerializerForJavaDuration(duration)
      case Codec.Date => DateFromUnixDate(input)
      case Codec.String => SerializerBuildHelper.createSerializerForString(input)
      case Codec.Decimal(precision, scale) =>
        /* We inline part of SerializerBuildHelper.createSerializerForBigDecimal here since we do not need an
         * overflow check */
        /* Since our decimal type is fixed since it can not overflow. This can be changed back to call
         * createSerializerForBigDecimal */
        // when using Spark 4.0.0 where ansi behavior is the default or if enable it globally.
        StaticInvoke(Decimal.getClass, catalystType(codec), "apply", input :: Nil)

      case Codec.Seq(element) => createSerializerForIterable(element, codec.classTag, input)
      case Codec.Iterable(tag, element) => createSerializerForIterable(element, tag, input)

      case Codec.Map(key, value) => ExternalMapToCatalyst(
          input,
          ObjectType(classOf[AnyRef]),
          validateAndSerializeElement(key),
          nullable(key),
          ObjectType(classOf[AnyRef]),
          validateAndSerializeElement(value),
          nullable(value)
        )

      case Codec.Option(inner @ Codec.Option(_)) =>
        val unwrapped = UnwrapOption(ObjectType(classOf[Option[?]]), input)
        createSerializerForStruct(unwrapped, Seq("value" -> createSerializer(inner, unwrapped)))
      case Codec.Option(valueCodec) => createSerializer(valueCodec, UnwrapOption(jvmType(valueCodec), input))

      case Codec.Product(_, _, Some(_)) =>
        val nullLiteral = Literal.create(null, NullType)
        // Spark cheks that all serializer has at least one BoundReference.
        val dummyConditionToUseInput = If(IsNull(input), nullLiteral, nullLiteral)
        createSerializerForStruct(input, Seq(Forbidden.column -> dummyConditionToUseInput))

      case prod: Codec.Product[?] =>
        /* To support NamedTuples we use productElement accessor if a accessor with the field name does not
         * exists. Based on the discussion in [0] it seems like one should not assume such an accessor exists.
         * And based on the comment in [1] it seems like using productElement is the correct way to access the
         * fields. We still use the accessor if it exists since that is more efficient and follows the
         * existing way that spark works.
         *
         * [0] https://github.com/scala/scala3/issues/15398
         *
         * [1] https://github.com/scala/scala3/issues/22382#issuecomment-2613187822 */

        val cls = prod.classTag.runtimeClass
        def isValidJavaAccessor(cls: Class[?], name: String): Boolean =
          !SourceVersion.isKeyword(name) && SourceVersion.isIdentifier(name) &&
            MethodUtils.getMatchingAccessibleMethod(cls, name) != null
        def nameAndSerializer(field: Field[?], index: Int): (String, Expression) = {
          def invoke(functionName: String, arguments: Expression*): Invoke =
            Invoke(
              KnownNotNull(input),
              functionName,
              jvmType(field.codec),
              arguments,
              returnNullable = nullable(field.codec)
            )
          val getter =
            if isValidJavaAccessor(cls, field.name) then invoke(field.name)
            else invoke("productElement", Literal(index))
          field.name -> createSerializer(field.codec, getter)
        }
        val nameAndSerializedFields = prod
          .fields
          .mapConst([t] => identity(_))
          .zipWithIndex
          .map(nameAndSerializer(_, _))
        createSerializerForStruct(input, nameAndSerializedFields)

      case sum: Codec.Sum[T, ?] =>
        val rowEncoder = Literal.create(SumToRowEncoder(sum), ObjectType(classOf[SumToRowEncoder[?]]))
        val asRow = Invoke(rowEncoder, "encode", ObjectType(classOf[Row]), Seq(input))
        val rowSerializer = createRowSerializer(sum.reprFields, KnownNotNull(asRow))
        if (input.nullable) If(IsNull(asRow), Literal.create(null, catalystType(sum)), rowSerializer)
        else rowSerializer

      case Codec.FromInjection(inj, innerCodec) =>
        val obj = Literal.create(inj, ObjectType(classOf[Injection[?, ?]]))
        createSerializer(innerCodec, Invoke(obj, "apply", jvmType(innerCodec), Seq(input)))
    }

  private def createSerializerForIterable[T, C <: Iterable[T]](
      element: Codec[T],
      tag: ClassTag[C],
      input: Expression
  ): Expression = {

    /* Based on Spark:
     * https://github.com/apache/spark/blob/v3.5.4/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/SerializerBuildHelper.scala#L327 */
    val asSeq =
      if (!classOf[scala.collection.Seq[?]].isAssignableFrom(tag.runtimeClass)) {
        Invoke(input, "toSeq", ObjectType(classOf[scala.collection.Seq[?]]))
      } else { input }
    MapObjects(validateAndSerializeElement(element), asSeq, ObjectType(classOf[AnyRef]))
  }

  private def createSerializerForStruct(input: Expression, fields: Seq[(String, Expression)]): Expression = {
    val struct = CreateNamedStruct(fields.flatMap { case (name, field) => List(Literal(name), field) })
    if (input.nullable) { If(IsNull(input), Literal.create(null, struct.dataType), struct) } else { struct }
  }

  private def createRowSerializer(fields: Seq[Field[?]], input: Expression): Expression = {
    val serializedFields = fields
      .zipWithIndex
      .map { case (field, index) =>
        val fieldValue = createSerializer(
          field.codec,
          ValidateExternalType(
            GetExternalRowField(input, index, field.name),
            catalystType(field.codec),
            jvmType(field.codec)
          )
        )

        val convertedField =
          if (nullable(field.codec)) {
            If(
              Invoke(input, "isNullAt", BooleanType, Literal(index) :: Nil),
              Literal.create(null, fieldValue.dataType),
              fieldValue
            )
          } else AssertNotNull(fieldValue)
        field.name -> convertedField
      }
    createSerializerForStruct(input, serializedFields)
  }
  /* We encode to a Row here using scala code. This way it easier to move this to the Spark 4.0.0 approach
   * where we will use this inside a TransformingEncoder. */
  trait SumToRowEncoder[S] extends Serializable {
    def encode(s: S): Row
    def decode(row: Row): S
  }

  private[spark] object SumToRowEncoder {
    def apply[T](s: Codec.Sum[T, ?]): SumToRowEncoder[T] = {
      val reprSize = s.reprFields.size
      val ordinalToSingleton = s
        .variants
        .mapConst[Option[T]]([t <: T] => Some(_).collect { case Variant.Singleton(value = v) => v })
        .toArray
      val ordinalToProductOfAllNone = s.variants.mapConst[scala.Option[T]]([t <: T] => _.allNone).toArray
      val ordinalToIndex = s
        .variants
        .mapConst[Boolean]([t] => _.isInstanceOf[Variant.Singleton[?]])
        .scanLeft(1)((index, isSingleton) => if (isSingleton) { index } else { index + 1 })
        .dropRight(1)
        .toArray
      val variantNames = s.variants.mapConst[String]([t] => _.name)
      val discriminantToOrdinal = variantNames.zipWithIndex.toMap
      val ordinalToDiscriminant = variantNames.toArray
      new SumToRowEncoder[T] {
        def encode(in: T): Row = {
          val values = new Array[Any](reprSize)
          val ordinal = s.ordinal(in)
          values(0) = ordinalToDiscriminant(ordinal)
          if (ordinalToSingleton(ordinal).isEmpty) values(ordinalToIndex(ordinal)) = Some(in)
          new GenericRow(values)
        }
        def decode(out: Row): T = {
          val discriminant = out.getString(0)
          val ordinal = discriminantToOrdinal(discriminant)
          ordinalToSingleton(ordinal)
            /* When reading old data where a singleton has been changed to a product spark can return null
             * here. */
            .orElse(Option(out.getAs[Option[T]](ordinalToIndex(ordinal))).flatten)
            .orElse(ordinalToProductOfAllNone(ordinal))
            .getOrElse {
              throw new RuntimeException(
                s"Expected non-null value for variant $discriminant but found null in row $out"
              )
            }
        }
      }
    }
  }

  private def createDeserializer[T](codec: Codec[T]): Expression = {
    val walkedTypePath = WalkedTypePath().recordRoot(codec.classTag.runtimeClass.getName)
    val input = GetColumnByOrdinal(0, jvmType(codec))
    val deserializer = createDeserializer(codec, input, walkedTypePath, topRow = true)
    expressionWithNullSafety(deserializer, nullable(codec), walkedTypePath)
  }

  private def createDeserializer[T](
      codec: Codec[T],
      path: Expression,
      walkedTypePath: WalkedTypePath,
      topRow: Boolean
  ): Expression =
    codec match {
      case Codec.Byte => path
      case Codec.Short => path
      case Codec.Int => path
      case Codec.Long => path
      case Codec.Float => path
      case Codec.Double => path
      case Codec.Boolean => path
      case Codec.Bytes => path
      case Codec.TimestampMicros => UnixMicros(path)
      case Codec.DurationMicros =>
        val duration = DeserializerBuildHelper.createDeserializerForDuration(path)
        StaticInvoke(
          IntervalUtils.getClass,
          LongType,
          "durationToMicros",
          duration :: Nil,
          returnNullable = false
        )
      case Codec.Date => UnixDate(path)
      case Codec.String => DeserializerBuildHelper.createDeserializerForString(path, returnNullable = false)
      case Codec.Decimal(precision, scale) =>
        DeserializerBuildHelper.createDeserializerForScalaBigDecimal(path, returnNullable = false)

      case seq: Codec.Seq[t] => createDeserializerForIterable(
          seq.element,
          seq.classTag,
          IndexedSeq.iterableFactory[t],
          path,
          walkedTypePath
        )
      /* The cast are safe because of contraints in Iterable, but seems compiler is no able to extract GDAT
       * constraints here. If https://github.com/scala/scala3/issues/24557 is fixed we should be able to
       * remove the casts. */
      case it: Codec.Iterable[?, ?] => createDeserializerForIterable(
          it.element,
          // TYPE SAFETY: see above.
          it.classTag.asInstanceOf,
          // TYPE SAFETY: see above.
          it.factory.asInstanceOf,
          path,
          walkedTypePath
        )

      case map: Codec.Map[?, ?] =>
        val newTypePath = walkedTypePath.recordMap(
          map.key.classTag.runtimeClass.getName,
          map.value.classTag.runtimeClass.getName
        )
        UnresolvedCatalystToExternalMap(
          path,
          createDeserializer(map.key, _, newTypePath, topRow = false),
          createDeserializer(map.value, _, newTypePath, topRow = false),
          map.classTag.runtimeClass
        )

      case Codec.Option(valueCodec) =>
        val newTypePath = walkedTypePath.recordOption(valueCodec.classTag.runtimeClass.getName)
        valueCodec match {
          case Codec.Option(_) =>
            val innerValue = UnresolvedExtractValue(path, Literal("value"))
            If(
              IsNull(path),
              Literal.create(None, ObjectType(classOf[Option[?]])),
              NewInstance(
                classOf[Some[?]],
                Seq(createDeserializer(valueCodec, innerValue, newTypePath, topRow = false)),
                ObjectType(classOf[Option[?]]),
                propagateNull = false
              )
            )
          case _ =>
            WrapOption(createDeserializer(valueCodec, path, newTypePath, topRow = false), jvmType(valueCodec))
        }

      case Codec.Product(_, _, Some(singleton)) =>
        val newInstance = Literal.create(singleton, jvmType(codec))
        if topRow then newInstance else If(IsNull(path), Literal.create(null, jvmType(codec)), newInstance)

      case prod: Codec.Product[?] =>
        val classArgs = prod.fields.mapConst([t] => _.codec.classTag.runtimeClass)
        val cls = prod.classTag.runtimeClass
        val newInstance =
          /* For large tuples or custom mirrors the corresponding constructor may not exists and we should
           * construct using the mirror instead. */
          if ConstructorUtils.getMatchingAccessibleConstructor(cls, classArgs*) != null then {
            def fieldDeserializer[T](field: Field[T]): Expression = {
              val newTypePath =
                walkedTypePath.recordField(field.codec.classTag.runtimeClass.getName, field.name)
              val getter =
                DeserializerBuildHelper.addToPath(path, field.name, catalystType(field.codec), newTypePath)
              expressionWithNullSafety(
                createDeserializer(field.codec, getter, newTypePath, topRow = false),
                nullable(field.codec),
                newTypePath
              )
            }
            val arguments = prod.fields.mapConst([t] => fieldDeserializer(_))
            NewInstance(cls, arguments, Nil, propagateNull = false, jvmType(codec), None)
          } else {
            val fields = prod.fields.mapConst([t] => identity(_))
            val row =
              createRowDeserializer(path, fields, walkedTypePath, structFromFields(fields), topRow = topRow)
            val clsRowProduct = classOf[RowProduct]
            val rowProduct =
              NewInstance(clsRowProduct, Seq(row), ObjectType(clsRowProduct), propagateNull = false)
            val mirror = Literal.create(prod.mirror, ObjectType(classOf[Mirror.Product]))
            Invoke(mirror, "fromProduct", jvmType(prod), Seq(rowProduct))
          }
        If(IsNull(path), Literal.create(null, jvmType(codec)), newInstance)

      case sum: Codec.Sum[T, ?] => createSumDeserializer(path, sum, walkedTypePath, topRow)

      case inj: Codec.FromInjection[T, ?] =>
        val obj = Literal.create(inj.inj, ObjectType(classOf[Injection[?, ?]]))
        Invoke(obj, "invert", jvmType(inj), Seq(createDeserializer(inj.to, path, walkedTypePath, topRow)))
    }

  private def createSumDeserializer[T](
      path: Expression,
      sum: Codec.Sum[T, ?],
      walkedTypePath: WalkedTypePath,
      topRow: Boolean
  ): Expression = {
    val row = createRowDeserializer(path, sum.reprFields, walkedTypePath, sumSchema(sum), topRow)
    val rowEncoder = Literal.create(SumToRowEncoder(sum), ObjectType(classOf[SumToRowEncoder[?]]))
    Invoke(rowEncoder, "decode", jvmType(sum), Seq(row))
  }

  /* Because we use the row encoder for sum type we need some special handling when it the top level object.
   * Spark also has this for the RowEncoder: */
  /* https://github.com/apache/spark/blob/7c29c664cdc9321205a98a14858aaf8daaa19db2/sql/catalyst/src/main/scala/org/apache/spark/sql/catalyst/DeserializerBuildHelper.scala#L218-L223 */
  private def createRowDeserializer(
      path: Expression,
      fields: Seq[Field[?]],
      walkedTypePath: WalkedTypePath,
      schema: StructType,
      topRow: Boolean
  ): Expression =
    if topRow then createRowDeserializerTopRow(path, fields, walkedTypePath, schema)
    else createRowDeserializerInner(path, fields, walkedTypePath, schema)

  private def createRowDeserializerTopRow(
      path: Expression,
      fields: Seq[Field[?]],
      walkedTypePath: WalkedTypePath,
      schema: StructType
  ): Expression = {
    val convertedFields = fields
      .zipWithIndex
      .map { case (Field(name, codec), i) =>
        val newTypePath = walkedTypePath.recordField(codec.classTag.runtimeClass.getName, name)
        createDeserializer(codec, GetStructField(path, i), newTypePath, topRow = false)
      }
    CreateExternalRow(convertedFields, schema)
  }

  private def createRowDeserializerInner(
      path: Expression,
      fields: Seq[Field[?]],
      walkedTypePath: WalkedTypePath,
      schema: StructType
  ): Expression = {
    val convertedFields = fields
      .zipWithIndex
      .map { case (Field(name, codec), i) =>
        val newTypePath = walkedTypePath.recordField(codec.classTag.runtimeClass.getName, name)
        If(
          Invoke(path, "isNullAt", BooleanType, Literal(i) :: Nil),
          Literal.create(null, jvmType(codec)),
          createDeserializer(codec, GetStructField(path, i), newTypePath, topRow = false)
        )
      }
    If(
      IsNull(path),
      Literal.create(null, ObjectType(classOf[Row])),
      CreateExternalRow(convertedFields, schema)
    )
  }

  /* Conservative List of collections directly supported by Spark. This is needed because Spark does not
   * correctly handle collections that takes an implicit parameter to their newBuilder method. */
  private val sparkSupportedCollections = Seq(classOf[List[?]], classOf[Set[?]], classOf[Seq[?]])

  private def createDeserializerForIterable[T, C <: Iterable[T]](
      elementCodec: Codec[T],
      tag: ClassTag[C],
      factory: Factory[T, C],
      path: Expression,
      walkedTypePath: WalkedTypePath
  ): Expression = {
    val newTypePath = walkedTypePath.recordArray(elementCodec.classTag.runtimeClass.getName)
    val mapFunction: Expression => Expression = element =>
      // upcast the array element to the data type the encoder expects.
      DeserializerBuildHelper.deserializerForWithNullSafetyAndUpcast(
        element,
        catalystType(elementCodec),
        nullable = nullable(elementCodec),
        newTypePath,
        createDeserializer(elementCodec, _, newTypePath, topRow = false)
      )
    if (sparkSupportedCollections.contains(tag.runtimeClass)) {
      UnresolvedMapObjects(mapFunction, path, Some(tag.runtimeClass))
    } else {
      val arrayData = UnresolvedMapObjects(mapFunction, path)
      val asSeq = Invoke(
        arrayData,
        "toSeq",
        ObjectType(classOf[IndexedSeq[?]]),
        Seq(Literal(jvmType(elementCodec), ObjectType(classOf[DataType])))
      )
      /* This will create an unnecessary copy, we could consider specializing for cases like ArraySeq and use
       * unsafeWrapArray to avoid the copy. */
      Invoke(
        Literal.create(factory, ObjectType(classOf[Factory[?, ?]])),
        "fromSpecific",
        ObjectType(tag.runtimeClass),
        Seq(asSeq)
      )
    }

  }

  private def validateAndSerializeElement(codec: Codec[?]): Expression => Expression = { input =>
    val expected = codec match {
      case _: Codec.Option[?] => jvmType(codec)
      case codec => catalystType(codec)
    }

    expressionWithNullSafety(
      createSerializer(codec, ValidateExternalType(input, expected, jvmType(codec))),
      nullable(codec),
      WalkedTypePath()
    )
  }
}
