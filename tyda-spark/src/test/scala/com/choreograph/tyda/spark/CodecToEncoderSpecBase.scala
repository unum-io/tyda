package com.choreograph.tyda.spark
import scala.collection.immutable.ArraySeq
import scala.compiletime.ops.int.>
import scala.reflect.ClassTag

import org.apache.spark.sql.types.ArrayType
import org.apache.spark.sql.types.BinaryType
import org.apache.spark.sql.types.BooleanType
import org.apache.spark.sql.types.ByteType
import org.apache.spark.sql.types.DataType
import org.apache.spark.sql.types.DateType
import org.apache.spark.sql.types.DayTimeIntervalType
import org.apache.spark.sql.types.DoubleType
import org.apache.spark.sql.types.FloatType
import org.apache.spark.sql.types.IntegerType
import org.apache.spark.sql.types.LongType
import org.apache.spark.sql.types.MapType
import org.apache.spark.sql.types.ShortType
import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.types.StructField
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.types.TimestampType
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.Ord
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.spark.CodecToCatalystType.nullable
import com.choreograph.tyda.staticAssert

object CodecToEncoderSpecBase {
  final case class Person(name: String, age: Int, alias: Option[String] = None) derives Codec, Ord
  final case class Employee(person: Person, salary: Double) derives Codec, Ord

  enum Location extends EnumStableHashCode derives Codec, Ord {
    case Null extends Location
    case City(name: String) extends Location
    case Country(name: String) extends Location
    case Undefined extends Location
    case Missing extends Location
    case Region(name: Int) extends Location
  }
  enum OnlySingletons extends EnumStableHashCode derives Codec, Ord {
    case A
    case B
  }

  enum MyOption[+T] extends EnumStableHashCode derives Codec, Ord {
    case Some(x: T)
    case None
  }

  enum SameShape derives Codec, Ord {
    case A(a: Int, b: Long)
    case B(a: Int, b: Long)
  }

  opaque type MyString = String

  object MyString {
    given Codec[MyString] = Codec.string
    given Ordering[MyString] = Ordering[String]
    given Arbitrary[MyString] = Arbitrary.string
  }

  enum NestedEnum derives Codec, Ord {
    case Case(city: Location.City)
    case Enum(location: Location)
  }

  enum Generic extends EnumStableHashCode derives Codec, Ord {
    case A
  }

  enum GenericEmptyVariants[+T <: Generic] extends EnumStableHashCode {
    case Empty1, Empty2
  }
  object GenericEmptyVariants {
    given Codec[GenericEmptyVariants[Nothing]] = Codec.sum[GenericEmptyVariants[Nothing]]
    given Ordering[GenericEmptyVariants[Nothing]] = Ord.sum[GenericEmptyVariants[Nothing]]
  }

  enum EnumString extends EnumStableHashCode derives Codec.EnumAsString, Ord {
    case A
    case B
  }
  private final case class JavaKeyword(`class`: String) derives Codec, Ord
  private final case class JavaInvalidIdentifiers(`0`: String) derives Codec, Ord

  type BigNamedTuple = (
      f1: NestedEnum,
      f2: EnumString,
      f3: Timestamp,
      f4: Date,
      f5: Long,
      f6: String,
      f7: Short,
      f8: Byte,
      f9: Float,
      f10: Double,
      f11: Boolean,
      f12: Person,
      f13: Employee,
      f14: Seq[Int],
      f15: (int: Int, double: Double),
      f16: Int,
      f17: Int,
      f18: Int,
      f19: Int,
      f20: Int,
      f21: Int,
      f22: Int,
      f23: Int
  )

  staticAssert[NamedTuple.Size[BigNamedTuple] > 22, "BigNamedTuple should exceed 22 elements."]

  // To prefer equiv from Ord and avoid diverging givens
  given [T: Ord]: Equiv[T] = Ord[T]
  given [K, V]: Equiv[Map[K, V]] = Equiv.universal
}

trait CodecToEncoderSpecBase extends AnyFunSuite with SharedSparkSession {
  import CodecToEncoder.convert
  import CodecToEncoderSpecBase.{given, *}

  private def roundtripAndCheck[T: {Codec, Equiv as equiv}](values: Seq[T]): Unit = {
    val ds = spark.createDataset(values).map(identity)

    // Check that we are actually going serializing and deserializing the value
    val planStr = ds.queryExecution.executedPlan.toString
    assert(planStr.contains("SerializeFromObject"))
    assert(planStr.contains("DeserializeToObject"))

    val result = ds.collect().toSeq
    assert(result.size == values.size, s"Expected ${values.size} values but got ${result.size}")
    result
      .zip(values)
      .foreach { case (a, b) => assert(equiv.equiv(a, b), s"Expected value $b did not match result $a") }
  }

  def test[T: {Arbitrary, Codec, Equiv as equiv, TypeName}](supportsNull: Boolean = true): Unit = {
    test(s"encoder for ${TypeName.name[T]} round trip")(roundtripAndCheck(Seq.fill(10)(Arbitrary[T]())))
    if (supportsNull) {
      test(s"encoder for ${TypeName.name[T]} supports null") {
        // Top level null is not supported so we need to wrap `T` in another struct
        given Equiv[Tuple1[T]] = Equiv.universal
        // TYPE SAFETY: If supportsNull is true, then T >: Null
        roundtripAndCheck[Tuple1[T]](Seq(Tuple1(null.asInstanceOf[T])))
      }
    }
  }

  test[Byte](supportsNull = false)
  test[Int](supportsNull = false)
  test[Short](supportsNull = false)
  test[Long](supportsNull = false)
  test[Float](supportsNull = false)
  test[Double](supportsNull = false)
  test[Boolean](supportsNull = false)
  test[Timestamp](supportsNull = false)
  test[Duration](supportsNull = false)
  test[Date](supportsNull = false)
  test[Option[Date]](supportsNull = false)
  test[String]()
  test[Decimal[5, 2]](supportsNull = false)
  test[Decimal[15, 2]](supportsNull = false)
  test[Decimal[38, 24]](supportsNull = false)
  // For Options `null` will be turned into `None`
  test[Option[Int]](supportsNull = false)
  test[Option[Option[Int]]](supportsNull = false)
  test[Tuple1[Option[Option[Int]]]](supportsNull = true)
  test[Option[Option[Option[String]]]](supportsNull = false)
  test[Option[Option[Option[Option[Person]]]]](supportsNull = false)
  test[(Int, Double)]()
  test[Seq[(Int, Double)]]()
  test[ArraySeq[Int]]()
  test[ArraySeq[Person]]()
  test[Map[Int, Int]]()
  test[Map[Person, String]]()
  test[Person]()
  test[Employee]()
  test[(Int, Option[Int])]()
  test[Location](supportsNull = false)
  test[Option[Location]](supportsNull = false)
  test[Tuple1[Location]]()
  test[List[Location]]()
  test[OnlySingletons](supportsNull = false)
  test[MyOption[Int]](supportsNull = false)
  test[Option[MyOption[Int]]](supportsNull = false)
  test[Tuple1[MyOption[String]]]()
  test[Tuple1[SameShape]]()
  test[MyString]()
  test[NestedEnum](supportsNull = false)
  test[GenericEmptyVariants[Generic.A.type]](supportsNull = false)
  test[BigInt](supportsNull = false)
  test[EnumString](supportsNull = false)
  test[(name: String, age: Int)](supportsNull = false)
  test[BigNamedTuple](supportsNull = false)
  test[(Int, BigNamedTuple)](supportsNull = false)
  test[Tuple1[(namedTupleWithOption: Option[Int])]](supportsNull = false)
  test[Tuple1[(namedTupleWithOptionTimestamp: Option[Timestamp])]](supportsNull = false)
  test[OnlySingletons.A.type](supportsNull = false)
  test[Option[OnlySingletons.A.type]](supportsNull = false)
  test[Tuple1[OnlySingletons.A.type]]()
  {
    type S = Seq[OnlySingletons.A.type]
    // Explicit givens needed due to https://github.com/scala/scala3/issues/24416
    given Arbitrary[S] = Arbitrary.iterable[OnlySingletons.A.type, S]
    given Codec[S] = Codec.iterable[OnlySingletons.A.type, S]
    test[S]()
  }
  test[Seq[OnlySingletons]]()
  test[JavaKeyword]()
  test[JavaInvalidIdentifiers]()

  def schemaTest[T: Codec: TypeName](expectedType: DataType): Unit =
    test(s"schema test for ${TypeName.name[T]}") {
      val expected = expectedType match {
        case s: StructType => s
        case other => StructType(Seq(StructField("value", other, nullable = nullable(Codec[T]))))
      }

      val fromCodec = convert[T].schema
      assert(fromCodec == expected, s"\n$fromCodec\ndid not equal\n$expected")
    }

  schemaTest[Byte](ByteType)
  schemaTest[Short](ShortType)
  schemaTest[Int](IntegerType)
  schemaTest[Long](LongType)
  schemaTest[Float](FloatType)
  schemaTest[Double](DoubleType)
  schemaTest[Boolean](BooleanType)
  schemaTest[BigInt](BinaryType)
  schemaTest[Timestamp](TimestampType)
  schemaTest[Duration](DayTimeIntervalType())
  schemaTest[Date](DateType)
  schemaTest[Seq[Int]](ArrayType(IntegerType, false))
  schemaTest[Map[Int, Int]](MapType(IntegerType, IntegerType, false))
  schemaTest[Map[Int, Option[Int]]](MapType(IntegerType, IntegerType, true))

  val expectedPerson = StructType(Seq(
    StructField("name", StringType, true),
    StructField("age", IntegerType, false),
    StructField("alias", StringType, true)
  ))
  schemaTest[Person](expectedPerson)
  val expectedEmployee =
    StructType(Seq(StructField("person", expectedPerson, true), StructField("salary", DoubleType, false)))
  schemaTest[Employee](expectedEmployee)
  schemaTest[Seq[Employee]](ArrayType(expectedEmployee))
  schemaTest[Map[Int, Employee]](MapType(IntegerType, expectedEmployee, true))
  schemaTest[Map[Employee, Int]](MapType(expectedEmployee, IntegerType, false))
  schemaTest[(name: String, age: Int)](StructType(
    Seq(StructField("name", StringType, true), StructField("age", IntegerType, false))
  ))

  val expectedLocationRepr = StructType(Seq(
    StructField(Codec.Sum.discriminant, StringType, true),
    StructField("city", StructType(Seq(StructField("name", StringType, true))), true),
    StructField("country", StructType(Seq(StructField("name", StringType, true))), true),
    StructField("region", StructType(Seq(StructField("name", IntegerType, false))), true)
  ))

  schemaTest[Location](expectedLocationRepr)
  schemaTest[Tuple2[Location, Int]](StructType(
    Seq(StructField("_1", expectedLocationRepr, false), StructField("_2", IntegerType, false))
  ))
  schemaTest[Tuple2[Option[Location], Int]](StructType(
    Seq(StructField("_1", expectedLocationRepr, true), StructField("_2", IntegerType, false))
  ))
  schemaTest[OnlySingletons](StructType(Seq(StructField(Codec.Sum.discriminant, StringType, true))))
  schemaTest[MyOption[Int]](StructType(Seq(
    StructField(Codec.Sum.discriminant, StringType, true),
    StructField("some", StructType(Seq(StructField("x", IntegerType, false))))
  )))
  schemaTest[Option[Int]](StructType(Seq(StructField("value", IntegerType, true))))

  schemaTest[NestedEnum](StructType(Seq(
    StructField(Codec.Sum.discriminant, StringType, true),
    StructField(
      "case",
      StructType(Seq(StructField("city", StructType(Seq(StructField("name", StringType, true))), true)))
    ),
    StructField("enum", StructType(Seq(StructField("location", expectedLocationRepr, false))), true)
  )))

  schemaTest[GenericEmptyVariants[Generic.A.type]](StructType(
    Seq(StructField(Codec.Sum.discriminant, StringType, true))
  ))

  schemaTest[EnumString](StringType)
}
