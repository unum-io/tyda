package com.choreograph.tyda.sparksql

import org.apache.spark.sql.types.ArrayType
import org.apache.spark.sql.types.DataType
import org.apache.spark.sql.types.MapType
import org.apache.spark.sql.types.StructType
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.spark.CodecToEncoder
import com.choreograph.tyda.spark.withAllNullable
import com.choreograph.tyda.sql.DdlDialect
import com.choreograph.tyda.sql.ToDdl

object ToDdlSparkSpec {
  private final case class Fields(name: Option[String], age: Int) derives Codec
  private final case class NestedFields(optionalFields: Option[Fields]) derives Codec
  private final case class NotOptionalFields(name: String, age: Int) derives Codec
  private final case class NestedNotOptionalFields(notOptionalFields: NotOptionalFields) derives Codec
  private enum Enum extends EnumStableHashCode derives Codec {
    case Singleton
    case Product1(a: Int, b: String)
    case Product2(c: Int, d: Long)
  }
  private enum EnumString extends EnumStableHashCode derives Codec.EnumAsString {
    case A
    case B
  }
}

class ToDdlSparkSpec extends AnyFunSuite {
  import ToDdlSparkSpec.*

  extension (ddlNullable: Boolean) {
    private def moreStrictThan(codecToEncoderNullable: Boolean): Boolean = {
      (ddlNullable == codecToEncoderNullable) || codecToEncoderNullable
    }
  }

  private def ddlIsMoreStrict(typeFromDdl: DataType, typeFromCodecToEncoder: DataType): Boolean = {
    (typeFromDdl, typeFromCodecToEncoder) match {
      case (StructType(ddlFields), StructType(codecToEncoderFields)) => ddlFields.size ==
          codecToEncoderFields.size &&
          ddlFields
            .zip(codecToEncoderFields)
            .forall { (ddlField, codecToEncoderField) =>
              ddlField.nullable.moreStrictThan(codecToEncoderField.nullable) &&
              ddlIsMoreStrict(ddlField.dataType, codecToEncoderField.dataType)
            }

      case (ArrayType(ddlType, ddlNullable), ArrayType(codecToEncoderType, codecToEncoderNullable)) =>
        ddlNullable.moreStrictThan(codecToEncoderNullable) && ddlIsMoreStrict(ddlType, codecToEncoderType)
      case (
            MapType(ddlKey, ddlValue, ddlNullable),
            MapType(codecToEncoderKey, codecToEncoderValue, codecToEncoderNullable)
          ) => ddlNullable.moreStrictThan(codecToEncoderNullable) &&
        ddlIsMoreStrict(ddlKey, codecToEncoderKey) && ddlIsMoreStrict(ddlValue, codecToEncoderValue)
      case _ => true
    }
  }

  private def sameAsCodecToEncoder[T: Codec: TypeName]: Unit = {
    test(s"same ddl as CodecToEncoder for ${TypeName.name}") {
      val codecToEncoderSchema = CodecToEncoder.convert.schema
      val ddlSchema = StructType.fromDDL(ToDdl.toDdl(Codec[T], DdlDialect.Spark))
      assert(codecToEncoderSchema == ddlSchema)
    }
  }

  private def sameAsCodecToEncoderButMoreStrict[T: Codec: TypeName]: Unit = {
    test(s"same ddl as CodecToEncoder but more strict ${TypeName.name}") {
      val codecToEncoderSchema = CodecToEncoder.convert.schema
      val ddlSchema = StructType.fromDDL(ToDdl.toDdl(Codec[T], DdlDialect.Spark))
      assert(codecToEncoderSchema != ddlSchema)
      assert(ddlIsMoreStrict(ddlSchema, codecToEncoderSchema))
    }
  }

  private def sameAsCodecToEncoderExceptNullability[T: Codec: TypeName]: Unit = {
    test(s"same ddl as CodecToEncoder except nullability for ${TypeName.name}") {
      val codecToEncoderSchema = CodecToEncoder.convert.schema
      val ddlSchema = StructType.fromDDL(ToDdl.toDdl(Codec[T], DdlDialect.Spark))
      assert(codecToEncoderSchema != ddlSchema)
      assert(!ddlIsMoreStrict(ddlSchema, codecToEncoderSchema))
      assert(withAllNullable(codecToEncoderSchema) == withAllNullable(ddlSchema))
    }
  }

  sameAsCodecToEncoder[Byte]
  sameAsCodecToEncoder[Option[Byte]]
  sameAsCodecToEncoder[Short]
  sameAsCodecToEncoder[Option[Short]]
  sameAsCodecToEncoder[Int]
  sameAsCodecToEncoder[Option[Int]]
  sameAsCodecToEncoder[Long]
  sameAsCodecToEncoder[Option[Long]]
  sameAsCodecToEncoder[Float]
  sameAsCodecToEncoder[Option[Float]]
  sameAsCodecToEncoder[Double]
  sameAsCodecToEncoder[Option[Double]]
  sameAsCodecToEncoder[Boolean]
  sameAsCodecToEncoder[Option[Boolean]]
  sameAsCodecToEncoder[Timestamp]
  sameAsCodecToEncoder[Option[Timestamp]]
  sameAsCodecToEncoder[Duration]
  sameAsCodecToEncoder[Option[Duration]]
  sameAsCodecToEncoder[Option[String]]
  sameAsCodecToEncoder[Option[Option[String]]]
  sameAsCodecToEncoder[Option[Option[Option[String]]]]
  sameAsCodecToEncoder[Option[Option[Option[Option[Int]]]]]
  sameAsCodecToEncoder[Option[List[Option[Int]]]]
  sameAsCodecToEncoder[Option[Map[Option[Int], Option[Byte]]]]
  sameAsCodecToEncoder[Fields]
  sameAsCodecToEncoder[Option[Fields]]
  sameAsCodecToEncoder[NestedFields]
  sameAsCodecToEncoder[(select: Int, `with space`: Long)]
  sameAsCodecToEncoder[Option[EnumString]]
  sameAsCodecToEncoder[Option[Decimal[10, 2]]]

  sameAsCodecToEncoderButMoreStrict[String]
  sameAsCodecToEncoderButMoreStrict[Decimal[10, 2]]
  sameAsCodecToEncoderButMoreStrict[Binary]
  sameAsCodecToEncoderButMoreStrict[Enum]
  sameAsCodecToEncoderButMoreStrict[List[Option[Int]]]
  sameAsCodecToEncoderButMoreStrict[List[Option[List[Option[Int]]]]]
  sameAsCodecToEncoderButMoreStrict[List[List[Option[Int]]]]
  sameAsCodecToEncoderButMoreStrict[Map[List[String], List[Option[Int]]]]
  sameAsCodecToEncoderButMoreStrict[Map[Int, Option[Byte]]]
  sameAsCodecToEncoderButMoreStrict[Map[Option[Int], Option[Byte]]]
  sameAsCodecToEncoderButMoreStrict[NotOptionalFields]
  sameAsCodecToEncoderButMoreStrict[NestedNotOptionalFields]
  sameAsCodecToEncoderButMoreStrict[EnumString]

  /* These ought to be exactly the same, but spark column definition DDL doesn't currently support the NOT
   * NULL qualifier in the ARRAY element or MAP value positions */
  sameAsCodecToEncoderExceptNullability[List[List[Int]]]
  sameAsCodecToEncoderExceptNullability[Map[List[String], List[Int]]]
  sameAsCodecToEncoderExceptNullability[List[Int]]
  sameAsCodecToEncoderExceptNullability[Option[List[Int]]]
  sameAsCodecToEncoderExceptNullability[Map[Option[Int], Byte]]
  sameAsCodecToEncoderExceptNullability[Option[Map[Option[Int], Byte]]]
  sameAsCodecToEncoderExceptNullability[Map[Int, Byte]]
  sameAsCodecToEncoderExceptNullability[Option[Map[Int, Byte]]]

}
