package com.choreograph.tyda.spark

import org.apache.spark.sql.types.*
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Field
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName

class StructTypeToCodecSpec extends AnyFunSuite {
  private def roundTrip[T: {Codec, TypeName}]: Unit = {
    test(s"StructTypeToCodec round-trip for ${TypeName.name[T]}") {
      val schema = CodecToEncoder.convert[T].schema
      val approx = StructTypeToCodec(schema)
      assert(approx.isExact, s"Codec for schema $schema was not exact")
      assert(
        approx.codec == Codec[T],
        s"Round trip codec did not match for schema $schema\nReconstructed: ${approx
            .codec}\nOriginal: ${Codec[T]}"
      )
    }
  }

  // Note: Since we follow Sparks nullability in CodecToEncoder all Strings, collections and products needs to
  // be wrapped in Option for them to round trip.
  roundTrip[(a: Boolean)]
  roundTrip[(a: Byte)]
  roundTrip[(a: Short)]
  roundTrip[(a: Int)]
  roundTrip[(a: Float)]
  roundTrip[(a: Double)]
  roundTrip[(a: Option[Decimal[10, 2]])]
  roundTrip[(a: Option[Binary])]
  roundTrip[(a: Option[Timestamp])]
  roundTrip[(a: Option[Date])]
  roundTrip[(a: Option[Int])]
  roundTrip[(a: Option[Seq[Int]])]
  roundTrip[(a: Option[Seq[Option[(c: Int, d: Int)]]])]
  roundTrip[(a: Option[Seq[Option[String]]])]
  roundTrip[(a: Option[Map[Int, Int]])]
  roundTrip[(a: Option[(b: Long, c: Option[String])])]

  test("skips unsupported types") {
    val schema =
      StructType(Seq(StructField("valid", IntegerType, false), StructField("invalid", NullType, true)))
    val approx = StructTypeToCodec(schema)
    assert(!approx.isExact)
    assert(approx.codec == Codec.unsafeNamedTuple(Seq(Field("valid", Codec.Int))))
  }

  test("skips array with unsupported element type") {
    val schema = StructType(Seq(StructField("invalid_array", ArrayType(NullType), false)))
    val approx = StructTypeToCodec(schema)
    assert(!approx.isExact)
    assert(approx.codec == Codec.unsafeNamedTuple(Seq.empty))
  }

  test("skips map with unsupported key type") {
    val schema = StructType(Seq(StructField("invalid_map", MapType(NullType, IntegerType), false)))
    val approx = StructTypeToCodec(schema)
    assert(!approx.isExact)
    assert(approx.codec == Codec.unsafeNamedTuple(Seq.empty))
  }

  test("skips map with unsupported value type") {
    val schema = StructType(Seq(StructField("invalid_map", MapType(IntegerType, NullType), false)))
    val approx = StructTypeToCodec(schema)
    assert(!approx.isExact)
    assert(approx.codec == Codec.unsafeNamedTuple(Seq.empty))
  }
}
