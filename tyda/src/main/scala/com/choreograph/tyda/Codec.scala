package com.choreograph.tyda

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple
import scala.annotation.targetName
import scala.collection.Factory
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.reflect.classTag

import shapeless3.deriving.Ap
import shapeless3.deriving.K0
import shapeless3.deriving.Labelling
import shapeless3.deriving.MapF
import shapeless3.deriving.Pure

import com.choreograph.tyda.Decimal as TydaDecimal
import com.choreograph.tyda.shapeless3extras.WrappedCoproductInstances
import com.choreograph.tyda.shapeless3extras.WrappedProductInstances
import com.choreograph.tyda.shapeless3extras.labelled
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances

sealed trait Codec[T] {
  def classTag: ClassTag[T]
}

private[tyda] final case class Field[T](name: String, codec: Codec[T])
private[tyda] enum Variant[T] {
  def name: String

  case Product(name: String, codec: Codec.Product[T])
  case Singleton(name: String, value: T, codec: Codec.Product[T])

  /** Try to construct an instance of T where all fields are None.
    *
    * If any field is not an Option, returns None. This is used when reading old
    * data where a singleton has been extended with new optional fields.
    */
  def allNone: scala.Option[T] =
    this match {
      case Variant.Singleton(_, value, _) => None
      case Variant.Product(_, codec) => codec
          .fields
          .constructA([t] =>
            _ match {
              case Field(_, Codec.Option(_)) => Some(None)
              case _ => None
            }
          )(Variant.optionPure, Variant.optionMap, Variant.optionAp)
    }

  def toField: scala.Option[Field[Option[T]]] =
    Some(this).collect { case Variant.Product(name, codec) => Field(name, Codec.Option(codec)) }
}

object Variant {
  private val optionPure: Pure[scala.Option] = [a] => (a: a) => Some(a)
  private val optionMap: MapF[scala.Option] = [a, b] => (fa: scala.Option[a], f: a => b) => fa.map(f)
  private val optionAp: Ap[scala.Option] = [a, b] =>
    (ff: scala.Option[a => b], fa: scala.Option[a]) => ff.flatMap(f => fa.map(f))

  def apply[T](name: String, codec: Codec.Product[T]): Variant[T] =
    val camelCaseName = pascalCaseToCamelCase(name)
    codec match {
      case Codec.Product(_, _, Some(value)) => Singleton(camelCaseName, codec.fromProduct(EmptyTuple), codec)
      case _ => Product(camelCaseName, codec)
    }
}

object Codec {
  def apply[T](using c: Codec[T]): Codec[T] = c

  /** Base trait for codecs that are always leaf nodes and do not contain other
    * codecs.
    */
  private[tyda] sealed trait Primitive[T: ClassTag] extends Codec[T] {
    def classTag: ClassTag[T] = summon
  }

  private[tyda] case object Byte extends Primitive[scala.Byte]
  private[tyda] case object Short extends Primitive[scala.Short]
  private[tyda] case object Int extends Primitive[scala.Int]
  private[tyda] case object Long extends Primitive[scala.Long]
  private[tyda] case object Float extends Primitive[scala.Float]
  private[tyda] case object Double extends Primitive[scala.Double]
  private[tyda] case object Boolean extends Primitive[scala.Boolean]
  private[tyda] case object String extends Primitive[Predef.String]

  private[tyda] case object TimestampMicros extends Primitive[com.choreograph.tyda.Timestamp]
  private[tyda] case object DurationMicros extends Primitive[com.choreograph.tyda.Duration]
  private[tyda] case object Date extends Primitive[com.choreograph.tyda.Date]

  private[tyda] final case class Seq[T](element: Codec[T]) extends Codec[scala.Seq[T]] {
    def classTag: ClassTag[scala.Seq[T]] = summon
  }

  private[tyda] final case class Iterable[T, C <: scala.Iterable[T]](
      classTag: ClassTag[C],
      element: Codec[T]
  )(using Factory[T, C])
      extends FromInjection[C, scala.Seq[T]] {
    def factory: Factory[T, C] = summon
    def to: Codec[scala.Seq[T]] = Codec.Seq(element)
    def inj: Injection[C, scala.Seq[T]] = IterableInjection(factory)
  }

  private final case class IterableInjection[T, C <: scala.Iterable[T]](factory: Factory[T, C])
      extends Injection[C, scala.Seq[T]] {
    def apply(from: C): scala.Seq[T] = from.toSeq
    def invert(to: scala.Seq[T]): C = factory.fromSpecific(to)
  }

  private[tyda] final case class Map[K, V](key: Codec[K], value: Codec[V]) extends Codec[Predef.Map[K, V]] {
    def classTag: ClassTag[Predef.Map[K, V]] = summon
  }

  private[tyda] final case class Option[T](element: Codec[T]) extends Codec[scala.Option[T]] {
    def classTag: ClassTag[scala.Option[T]] = summon
  }

  private[tyda] final case class Decimal[P <: Int, S <: Int](precision: P, scale: S)(using
      val valid: TydaDecimal.Valid[P, S]
  ) extends Primitive[TydaDecimal[P, S]]

  final case class Product[T: Mirror.ProductOf](
      classTag: ClassTag[T],
      wrappedFields: WrappedProductInstances[Field, T],
      singleton: scala.Option[T]
  ) extends Codec[T] {
    def fields: K0.ProductInstances[Field, T] = wrappedFields.value
    def fromProduct(p: scala.Product): T = mirror.fromProduct(p)
    def mirror: Mirror.ProductOf[T] = summon[Mirror.ProductOf[T]]
  }
  object Product {
    def unapply[T](
        codec: Codec.Product[T]
    ): Some[(ClassTag[T], K0.ProductInstances[Field, T], scala.Option[T])] =
      Some((codec.classTag, codec.fields, codec.singleton))
  }

  private[tyda] final case class Sum[T: Mirror.SumOf, Repr <: AnyNamedTuple & scala.Product](
      classTag: ClassTag[T],
      wrappedVariants: WrappedCoproductInstances[Variant, T]
  ) extends FromInjection[T, Repr] with Codec[T] {
    // TYPE SAFETY: The representation can be seen as a NamedTuple with the fields defined in reprFields
    def reprCodec: Codec.Product[Repr] = unsafeNamedTuple(reprFields).asInstanceOf[Codec.Product[Repr]]
    def variants: K0.CoproductInstances[Variant, T] = wrappedVariants.value
    def ordinal(t: T): Int = summon[Mirror.SumOf[T]].ordinal(t)
    def reprFields: NonEmpty[scala.Seq[Field[?]]] =
      NonEmpty(
        Field(Codec.Sum.discriminant, Codec.String),
        variants.mapConst[scala.Option[Field[?]]]([t] => _.toField).flatten*
      )

    def to: Codec.Product[Repr] = reprCodec
    def inj: Injection[T, Repr] = SumAsReprInjection(this)
  }

  private trait SumAsReprInjection[T, Repr] extends Injection[T, Repr]

  private object SumAsReprInjection {
    def apply[T, Repr <: AnyNamedTuple & scala.Product](
        s: Codec.Sum[T, Repr]
    ): SumAsReprInjection[T, Repr] = {
      val reprSize = s.reprFields.size
      val ordinalToSingleton = s
        .variants
        .mapConst[scala.Option[T]]([t <: T] => Some(_).collect { case Variant.Singleton(value = v) => v })
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
      new SumAsReprInjection[T, Repr] {
        def apply(in: T): Repr = {
          val values = Array.fill[Any](reprSize)(None)
          val ordinal = s.ordinal(in)
          values(0) = ordinalToDiscriminant(ordinal)
          if (ordinalToSingleton(ordinal).isEmpty) values(ordinalToIndex(ordinal)) = Some(in)
          // TYPE SAFETY: At runtime NamedTuples are tuples and we contruct according to the representation.
          Tuple.fromArray(values).asInstanceOf[Repr]
        }
        def invert(out: Repr): T = {
          // TYPE SAFETY: The first element is always the discriminant
          val discriminant = out.productElement(0).asInstanceOf[String]
          val ordinal = discriminantToOrdinal(discriminant)
          ordinalToSingleton(ordinal) match {
            case Some(singleton) => singleton
            case None =>
              /* TYPE SAFETY: We know that the element at ordinalToIndex(ordinal) is Option[T] because of the
               * descriminant */
              val valueFromRepr = out.productElement(ordinalToIndex(ordinal)).asInstanceOf[scala.Option[T]]
              valueFromRepr
                .orElse(ordinalToProductOfAllNone(ordinal))
                .getOrElse {
                  throw new RuntimeException(
                    s"Unable to decode field with discriminant $discriminant from $out"
                  )
                }
          }
        }
      }
    }
  }

  private[tyda] object Sum {
    def unapply[T, Repr <: AnyNamedTuple & scala.Product](
        codec: Codec.Sum[T, Repr]
    ): Some[(ClassTag[T], K0.CoproductInstances[Variant, T])] = Some((codec.classTag, codec.variants))
    val discriminant: String = "discriminant"
  }

  /** A codec that encodes an Enum as a String.
    *
    * This can be used for enums that only contains singletons and will never be
    * extended to contain products. To use this codec an enum can opt in in the
    * derives class as follows:
    * ```
    * enum MyEnum derives Codec.EnumAsString {
    *  case A
    * }
    * ```
    *
    * Note: This codec is not compatible with the default codec for enums. So
    * both changing to and from this to the default is a breaking change.
    */
  sealed trait EnumAsString[T] extends Codec[T] {
    def values: scala.Seq[T]
  }

  object EnumAsString {
    inline def derived[T: ClassTag: AllSingletons as singletons]: Codec.EnumAsString[T] = {
      val tag = summon[ClassTag[T]]
      checkStableHashCode(tag)
      SumAsString[T](summon, singletons.values)
    }
  }

  private final case class SumAsStringInjection[T](values: scala.Seq[T], encodedValues: scala.Seq[String])
      extends Injection[T, String] {
    private val decode = encodedValues.zip(values).toMap
    private val encode = values.zip(encodedValues).toMap
    def apply(from: T): String = encode(from)
    def invert(to: String): T = decode(to)
  }

  private[tyda] final case class SumAsString[T](override val classTag: ClassTag[T], values: scala.Seq[T])
      extends EnumAsString[T], FromInjection[T, String] {
    def encodedValues: scala.Seq[String] = values.map(s => pascalCaseToCamelCase(s.toString))
    def to: Codec[String] = Codec.String
    def inj: Injection[T, String] = SumAsStringInjection(values, encodedValues)
  }

  /** A codec that encodes an [[Array[Byte]]] as a binary type.
    *
    * Note: This is distinct from encoding a [[scala.Seq[Byte]]], which is
    * represented as an array where each element is a byte.
    *
    * In Spark, for example: An [[Array[Byte]]] is encoded as a `binary` type:
    * ```
    * scala> spark.createDataset(Seq(Array[Byte]())).printSchema()
    * root
    *  |-- value: binary (nullable = true)
    * ```
    * While a [[Seq[Byte]]] is encoded as an `array` of bytes:
    * ```
    * scala> spark.createDataset(Seq(Seq[Byte]())).printSchema()
    * root
    *  |-- value: array (nullable = true)
    *  |    |-- element: byte (containsNull = false)
    * ```
    *
    * This distinction has important implications for schema evolution. For a
    * [[scala.Seq[Byte]]], it is possible to evolve the type to a
    * [[scala.Seq[Int]]] by leveraging integer promotion rules. For instance:
    * ```
    * scala> spark.createDataset(Seq(Seq[Byte](1))).as[Seq[Int]].collect()
    * val res2: Array[Seq[Int]] = Array(List(1))
    * ```
    * However, such evolution is not supported for the special `binary` type
    * used by [[Array[Byte]]]. Attempting to read `binary` data as a wider
    * integer type will result in an error:
    * ```
    * scala> spark.createDataset(Seq(Array[Byte]())).as[Seq[Int]]
    * org.apache.spark.sql.AnalysisException: [UNSUPPORTED_DESERIALIZER.DATA_TYPE_MISMATCH]
    * The deserializer is not supported: need a(n) "ARRAY" field but got "BINARY".
    * ```
    */
  private[tyda] case object Bytes extends Primitive[Array[Byte]]

  private[tyda] sealed trait FromInjection[From, To] extends Codec[From] {
    def inj: Injection[From, To]
    def to: Codec[To]
  }

  object FromInjection {
    def unapply[From, To](codec: Codec.FromInjection[From, To]): (Injection[From, To], Codec[To]) =
      (codec.inj, codec.to)
  }

  private[tyda] def fromInjection[From: ClassTag, To](
      withInjection: Injection[From, To],
      withTo: Codec[To]
  ): Codec[From] =
    new FromInjection[From, To] {
      def inj: Injection[From, To] = withInjection
      def to: Codec[To] = withTo
      def classTag: ClassTag[From] = summon
    }

  private object BigIntAsBytes extends Injection[scala.BigInt, Array[Byte]] {
    def apply(from: scala.BigInt): Array[Byte] = from.toByteArray
    def invert(to: Array[Byte]): scala.BigInt = scala.BigInt(to)
  }

  private def checkStableHashCode[S](tag: ClassTag[S]): Unit = {
    val isEnum = classOf[scala.reflect.Enum].isAssignableFrom(tag.runtimeClass)
    val hasWorkaround = classOf[EnumStableHashCode].isAssignableFrom(tag.runtimeClass)
    if isEnum && !hasWorkaround then
      throw new RuntimeException(
        s"Enum ${tag
            .runtimeClass
            .getName} might not have stable hashCode due to https://github.com/scala/scala3/issues/19177." +
          " Please mixing the trait EnumStableHashCode to make the hashCode stable."
      )
  }

  given byte: Codec[scala.Byte] = Byte
  given short: Codec[scala.Short] = Short
  given int: Codec[scala.Int] = Int
  given long: Codec[scala.Long] = Long
  given float: Codec[scala.Float] = Float
  given double: Codec[scala.Double] = Double
  given boolean: Codec[scala.Boolean] = Boolean
  given string: Codec[Predef.String] = String

  given date: Codec[com.choreograph.tyda.Date] = Date
  given timestamp: Codec[com.choreograph.tyda.Timestamp] = TimestampMicros
  given duration: Codec[com.choreograph.tyda.Duration] = DurationMicros

  given decimal[P <: Int, S <: Int](using valid: TydaDecimal.Valid[P, S]): Codec[TydaDecimal[P, S]] =
    Decimal[P, S](valid.precision, valid.scale)

  /** Encodes a [[BigInt]] as an array of bytes.
    *
    * Note: This is different from Spark that will store BigInt as a fixed size
    * array of 16 bytes. This instead uses a variable length array and can
    * therefore support all values of [[BigInt]].
    */
  given bigInt: Codec[BigInt] = fromInjection(BigIntAsBytes, Codec.Bytes)

  given seq[T: Codec, C <: scala.Seq[T]: ClassTag as tag](using Factory[T, C]): Codec[C] = {
    val seqTag = classTag[scala.Seq[T]]
    tag match {
      case `seqTag` => Seq(Codec[T])
      case _ => Iterable[T, C](summon[ClassTag[C]], Codec[T])
    }
  }

  given map[K: Codec, V: Codec]: Codec[Predef.Map[K, V]] = Codec.Map(Codec[K], Codec[V])

  given option[T: Codec]: Codec[scala.Option[T]] = Option(summon[Codec[T]])

  given product[T: ClassTag: Mirror.ProductOf as m: Labelling](using
      inst: K0.ProductInstances[Codec, T]
  ): Codec.Product[T] = {
    assert(
      ClassTag[T] != ClassTag[scala.Product],
      "Incorrect ClassTag supplied, possibly due to https://github.com/scala/scala3/issues/23195"
    )

    Product(
      summon[ClassTag[T]],
      WrappedProductInstances(inst.labelled.mapK[Field]([t] => Field[t].tupled(_))),
      scala.Option.when(inst.arity == 0)(m.fromProduct(EmptyTuple))
    )
  }

  /** Creates a default codec for a sum type (sealed trait or enum).
    *
    * For example, consider the following enum:
    *
    * ```scala sc-name: Status.scala
    * enum Status:
    *   case Active(userId: Int)
    *   case Inactive
    *   case Pending(reason: String)
    * ```
    *
    * This enum is encoded as if it was first converted to the following case
    * class:
    *
    * ```scala sc-compile-with: Status.scala
    * case class StatusRepr(
    *     discriminant: String,
    *     active: Option[Status.Active],
    *     pending: Option[Status.Pending]
    * )
    * ```
    *
    * In this representation, at most one element, except for `discriminant`, is
    * populated. The `discriminant` field contains the case label in camelCase
    * and can be used to determine which case is populated or if the written
    * case is a singleton. For instance, `Status.Inactive` would map to
    * `StatusRepr("inactive", None, None)`, and `Status.Active(0)` would map to
    * `StatusRepr("active", Status.Active(0), None)`.
    */
  given sum[T: ClassTag: Mirror.SumOf: Labelling](using
      inst: K0.CoproductInstances[Codec.Product, T]
  ): Codec[T] =

    val wrappedVariants = WrappedCoproductInstances(inst.labelled.mapK[Variant]([t] => Variant[t].tupled(_)))
    val sum = Sum(summon[ClassTag[T]], wrappedVariants)
    if sum.variants.mapConst([t] => _.name).distinct.size != sum.variants.arity then
      throw new RuntimeException(
        s"Camel cased variant names of ${summon[ClassTag[T]].runtimeClass.getName} are not unique"
      )
    if sum.variants.mapConst([t] => _.isInstanceOf[Variant.Singleton[t]]).exists(identity) then
      checkStableHashCode(summon[ClassTag[T]])
    sum

  inline def derived[T: ClassTag](using m: Mirror.Of[T]): Codec[T] =
    inline m match {
      case given Mirror.SumOf[T] => sum
      case given Mirror.ProductOf[T] => product
    }

  private[tyda] def iterable[T: Codec, C <: scala.Iterable[T]: ClassTag](using Factory[T, C]): Codec[C] =
    Iterable[T, C](summon[ClassTag[C]], Codec[T])

  /** Iterator with all Codecs that are used inside a Codec[T].
    */
  private[tyda] def iterate[T](codec: Codec[T]): Iterator[Codec[?]] =
    val inner = codec match {
      case _: Codec.Primitive[?] => Iterator.empty
      case Codec.Seq(element) => iterate(element)
      case Codec.Map(key, value) => iterate(key) ++ iterate(value)
      case Codec.Option(element) => iterate(element)
      case Codec.Product(_, fields, _) =>
        fields.foldLeft0(Iterator.empty[Codec[?]])([t] => (acc, f) => acc ++ iterate(f.codec))
      case Codec.FromInjection(_, to) => iterate(to)
    }
    Iterator(codec) ++ inner

  private[tyda] def tupleClassTag[T](arity: Int): ClassTag[T] =
    /* TYPE SAFETY: Before Scala 3 there was a limit of 22 elements, because each tuple arity used a generated
     * type. In Scala 3 this limit is removed by using a single type `scala.runtime.TupleXXL` for all tuples
     * with more than 22 elements. */
    if arity > 22 then summon[ClassTag[scala.runtime.TupleXXL]].asInstanceOf
    else if arity > 0 then ClassTag(Class.forName(s"scala.Tuple$arity"))
    else ClassTag(EmptyTuple.getClass)

  /** Create a codec for a tuple given codecs for each element.
    */
  private[tyda] def tuple[T <: Tuple](instances: K0.ProductInstances[Codec, T]): Codec.Product[T] = {
    val labelling: Labelling[T] =
      Labelling[T](s"Tuple${instances.arity}", (0 until instances.arity).map(i => s"_${i + 1}"))
    // TYPE SAFETY: The labelling matches the output structure of a normal tuple
    unsafeTupleCodec(instances, labelling).asInstanceOf[Codec.Product[T]]
  }

  /** Create a Codec for a named tuple given codecs for each value.
    */
  private[tyda] def namedTuple[NT <: AnyNamedTuple](
      instances: K0.ProductInstances[Codec, NamedTuple.DropNames[NT]]
  )(using StringLiterals[NamedTuple.Names[NT]]): Codec.Product[NT] =
    val labelling = Labelling[NamedTuple.DropNames[NT]]("NamedTuple", StringLiterals[NamedTuple.Names[NT]])
    // TYPE SAFETY: The labelling matches the output structure of a named tuple with names N
    unsafeTupleCodec(instances, labelling).asInstanceOf[Codec.Product[NT]]

  /** Create a codec for a named tuple corresponding to the structure of a given
    * product.
    */
  private[tyda] def namedTuple[T: Mirror.ProductOf as m](
      codec: Codec.Product[T]
  ): Codec.Product[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes]] =
    given Mirror.ProductOf[T] = codec.mirror
    val instances = tupleInstances(codec.fields.toTuple)
    // TYPE SAFETY: The fields match the structure of T
    namedTuple(instances).asInstanceOf[Codec.Product[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes]]]

  /** Dynamically create a codec for a named tuple given fields for each
    * element.
    */
  private def namedTuple[T <: Tuple](
      instances: K0.ProductInstances[Field, T]
  ): Codec.Product[NamedTuple[Tuple, T]] =
    val labelling: Labelling[T] = Labelling("NamedTuple", instances.mapConst([t] => _.name).toIndexedSeq)
    // TYPE SAFETY: The labelling matches the output structure of a named tuple with names given by fields
    unsafeTupleCodec(instances.mapK([t] => _.codec), labelling).asInstanceOf[Codec.Product[
      NamedTuple[Tuple, T]
    ]]

  /** Creates a codec for a tuple given codecs for each element and a labelling
    * to describe the field names.
    *
    * Used to avoid triggering derivation for cases where we know the codecs for
    * each element.
    *
    * NOTE: This is unsafe since the caller must ensure that the labelling
    * matches the desired output structure.
    */
  private def unsafeTupleCodec[T <: Tuple](
      instances: K0.ProductInstances[Codec, T],
      labelling: Labelling[T]
  ): Codec.Product[?] = {
    val tag = tupleClassTag[T](instances.arity)
    // TYPE SAFETY: T is a subtype of Tuple with the arity of instances
    val mirror = new scala.runtime.TupleMirror(instances.arity).asInstanceOf[Mirror.ProductOf[T]]
    product(using tag, mirror, labelling, instances)
  }

  private[tyda] def unsafeNamedTuple(fields: scala.Seq[Field[?]]): Codec.Product[NamedTuple[Tuple, Tuple]] =
    namedTuple(fieldTupleInstances(fields))

  /** Create a new Product codec by concatenating two existing Product codecs.
    *
    * The resulting codec will have the fields of `left` followed by the fields
    * of `right`.
    */
  private[tyda] def concatProduct[A, B](
      left: Codec.Product[A],
      right: Codec.Product[B]
  ): Codec.Product[NamedTuple[Tuple, Tuple]] = {
    def fields[T](prod: Codec.Product[T]): scala.Seq[Field[?]] =
      prod.fields.mapConst[Field[?]]([t] => identity(_))
    val combinedFields = fields(left) ++ fields(right)
    unsafeNamedTuple(combinedFields)
  }

  private[tyda] def concat[A <: AnyNamedTuple, B <: AnyNamedTuple](
      left: Codec[A],
      right: Codec[B]
  ): Codec.Product[NamedTuple.Concat[A, B]] =
    (left, right) match {
      case (left @ Codec.Product(_, _, _), right @ Codec.Product(_, _, _)) =>
        // TYPE SAFETY: The fields of the combined codec match the structure of the concatenated named tuple
        concatProduct(left, right).asInstanceOf[Codec.Product[NamedTuple.Concat[A, B]]]
      case _ => unreachable("NamedTuples are always encoded using Codec.Product")
    }

  private def fieldTupleInstances(t: scala.Seq[Field[?]]): K0.ProductInstances[Field, Tuple] =
    // TYPE SAFETY: All of the values of type Field[?]
    tupleInstances(Tuple.fromArray(t.toArray).asInstanceOf[Tuple.Map[Tuple, Field]])

  extension [T](codec: Codec[scala.Option[T]]) {
    @targetName("optionElementCodec")
    private[tyda] def element: Codec[T] =
      // TYPE SAFETY: Codec[Option[T]] is always Codec.Option[T]
      codec.asInstanceOf[Codec.Option[T]].element
  }

  extension [T, C <: scala.Iterable[T]](codec: Codec[C]) {
    private[tyda] def element: Codec[T] =
      codec match {
        case Codec.Iterable(_, codec) =>
          // TYPE SAFETY: The constraints in Codec.Iterable only give the compiler Codec[T$1 :> T],
          // but I believe that us having a Factory will also give us T$1 <: T.
          // TODO: Capture this better in the type system and avoid the cast here.
          codec.asInstanceOf[Codec[T]]
        case Codec.Seq(element) =>
          // TYPE SAFETY: Same as above
          element.asInstanceOf[Codec[T]]
        case Codec.Map(given Codec[k], given Codec[v]) =>
          // TYPE SAFETY: if Codec[C] is Codec.Map, then the pair is Codec[T]
          Codec[(k, v)].asInstanceOf[Codec[T]]
        case codec => unreachable(s"Expected Iterable or Map codec but got $codec")
      }
  }

  extension [T <: Tuple](codec: Codec[T]) {
    private[tyda] def elements: Tuple.Map[T, Codec] =
      // TYPE SAFETY: Codec[T] of a tuple with is always Codec.Product[T]
      codec.asInstanceOf[Codec.Product[T]].fields.mapK([t] => _.codec).toTuple
  }
}
