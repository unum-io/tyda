package com.choreograph.tyda.sql

import scala.reflect.ClassTag

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.SimpleTypeName
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.sql.DdlDialect.MapSupport
import com.choreograph.tyda.sql.ast.DdlWriter

object ToDdlSpec {
  final case class Person(name: String, age: Int) derives Codec
  final case class Parent(name: String, age: Int, child: Person) derives Codec
  final case class Primitives(
      byte: Byte,
      short: Short,
      int: Int,
      long: Long,
      float: Float,
      double: Double,
      boolean: Boolean,
      string: String
  ) derives Codec
  enum Enum extends EnumStableHashCode derives Codec {
    case Singleton
    case Product1(a: Int, b: String)
    case Product2(c: Int, d: Option[Long])
    case Product3(c: Int, d: Product1)
  }

  enum EnumString extends EnumStableHashCode derives Codec.EnumAsString {
    case A
    case B
  }

  final case class Containers(arr: List[Person], map: Map[String, List[Person]], opt: Option[List[Person]])
      derives Codec

  val DialectNoNotNull = DdlDialect(
    decimal = DdlDialect.DecimalSupport.Decimal128,
    duration = DdlDialect.DurationSupport.Native("INTERVAL DAY TO SECOND"),
    map = DdlDialect.MapSupport.Native(supportsNotNullKey = false, supportsNotNullValue = false),
    emptyStructFieldType = "void",
    supportsNotNullColumn = false,
    supportsNotNullArrayElement = false
  )
}

class ToDdlSpec extends AnyFunSuite {
  import ToDdlSpec.*

  private def toBigQueryDecimal(codec: Codec[?], parameterized: Boolean, pretty: Boolean = false): String = {
    val dialect = DdlDialect.Spark.copy(decimal = DdlDialect.DecimalSupport.BigQuery(parameterized))
    ToDdl.toDdl(codec, dialect, pretty)
  }

  private def toCustomFloatDdl(codec: Codec[?], pretty: Boolean = true): String = {
    val dialect = DdlDialect.Spark.copy(floatType = "FLOAT64", doubleType = "FLOAT64")
    ToDdl.toDdl(codec, dialect, pretty)
  }

  private def toAllNullableDdl(codec: Codec[?], pretty: Boolean = true): String = {
    ToDdl.toDdl(codec, DialectNoNotNull, pretty)
  }

  private def toSparkDdl(codec: Codec[?], pretty: Boolean = true): String =
    ToDdl.toDdl(codec, DdlDialect.Spark, pretty)

  private def toSparkDdlType(codec: Codec[?], pretty: Boolean = false): String = {
    val ddl = ToDdl.toDdlType(codec, DdlDialect.Spark)
    val writer = new java.io.StringWriter()
    DdlWriter(writer, pretty = pretty).write(ddl, 0)
    writer.toString
  }

  test("toDdl Person") {
    val ddl = toSparkDdl(Codec[Person])
    assert(ddl == """
                    |name STRING NOT NULL,
                    |age INT NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Person no not null support") {
    val ddl = toAllNullableDdl(Codec[Person])
    assert(ddl == """
                    |name STRING /*NOT NULL*/,
                    |age INT /*NOT NULL*/
                    |""".stripMargin)
  }

  test("toDdl Person no not null support pretty=false") {
    val ddl = toAllNullableDdl(Codec[Person], pretty = false)
    assert(ddl == "name STRING,age INT")
  }

  test("toDdl Parent") {
    val ddl = toSparkDdl(Codec[Parent])
    assert(ddl == """
                    |name STRING NOT NULL,
                    |age INT NOT NULL,
                    |child STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Parent no not null support") {
    val ddl = toAllNullableDdl(Codec[Parent])
    assert(ddl == """
                    |name STRING /*NOT NULL*/,
                    |age INT /*NOT NULL*/,
                    |child STRUCT<
                    |  name STRING /*NOT NULL*/,
                    |  age INT /*NOT NULL*/
                    |> /*NOT NULL*/
                    |""".stripMargin)
  }

  test("toDdl Primitives") {
    val ddl = toSparkDdl(Codec[Primitives])
    assert(ddl == """
                    |byte TINYINT NOT NULL,
                    |short SMALLINT NOT NULL,
                    |int INT NOT NULL,
                    |long BIGINT NOT NULL,
                    |float FLOAT NOT NULL,
                    |double DOUBLE NOT NULL,
                    |boolean BOOLEAN NOT NULL,
                    |string STRING NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Primitives custom float type") {
    val ddl = toCustomFloatDdl(Codec[Primitives])
    assert(ddl == """
                    |byte TINYINT NOT NULL,
                    |short SMALLINT NOT NULL,
                    |int INT NOT NULL,
                    |long BIGINT NOT NULL,
                    |float FLOAT64 NOT NULL,
                    |double FLOAT64 NOT NULL,
                    |boolean BOOLEAN NOT NULL,
                    |string STRING NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Timestamp") {
    val ddl = toSparkDdl(Codec[Timestamp])
    assert(ddl == """
                    |value TIMESTAMP NOT NULL
                    |""".stripMargin)
  }

  test("toDdl duration") {
    val ddl = toSparkDdl(Codec[Duration])
    assert(ddl == """
                    |value BIGINT NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Date") {
    val ddl = toSparkDdl(Codec.Date)
    assert(ddl == """
                    |value DATE NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Binary") {
    val ddl = toSparkDdl(Codec[Binary])
    assert(ddl == """
                    |value BINARY NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Array") {
    val ddl = toSparkDdl(Codec[List[Person]])
    assert(ddl == """
                    |value ARRAY<STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> /*NOT NULL*/> NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Array pretty=false") {
    val ddl = toSparkDdl(Codec[List[Person]], pretty = false)
    assert(ddl == "value ARRAY<STRUCT<name STRING NOT NULL,age INT NOT NULL>> NOT NULL")
  }

  test("toDdl Map") {
    val ddl = toSparkDdl(Codec[Map[String, Person]])
    assert(ddl == """
                    |value MAP<STRING /*NOT NULL*/, STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> /*NOT NULL*/> NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Map no not null support pretty=false") {
    val ddl =
      ToDdl.toDdl(Codec[Map[String, Person]], DialectNoNotNull.copy(map = MapSupport.Array), pretty = false)
    assert(ddl == "value ARRAY<STRUCT<key STRING,value STRUCT<name STRING,age INT>>>")
  }

  test("toDdl Seq[Seq[Int]] supportsArrayAsArrayElement=true") {
    val ddl = ToDdl.toDdl(Codec[Seq[Seq[Int]]], DialectNoNotNull, pretty = false)
    assert(ddl == "value ARRAY<ARRAY<INT>>")
  }

  test("toDdl Seq[Seq[Int]] supportsArrayAsArrayElement=false") {
    val ddl = ToDdl.toDdl(
      Codec[Seq[Seq[Int]]],
      DialectNoNotNull.copy(supportsArrayAsArrayElement = false),
      pretty = false
    )
    assert(ddl == "value ARRAY<STRUCT<value ARRAY<INT>>>")
  }

  test("toDdl Option") {
    val ddl = toSparkDdl(Codec[Option[Person]])
    assert(ddl == """
                    |value STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |>
                    |""".stripMargin)
  }

  test("toDdl Enum") {
    val ddl = toSparkDdl(Codec[Enum])
    assert(ddl == """
                    |discriminant STRING NOT NULL /*one of: singleton, product1, product2, product3*/,
                    |product1 STRUCT<
                    |  a INT NOT NULL,
                    |  b STRING NOT NULL
                    |>,
                    |product2 STRUCT<
                    |  c INT NOT NULL,
                    |  d BIGINT
                    |>,
                    |product3 STRUCT<
                    |  c INT NOT NULL,
                    |  d STRUCT<
                    |    a INT NOT NULL,
                    |    b STRING NOT NULL
                    |  > NOT NULL
                    |>
                    |""".stripMargin)
  }

  test("toDdl Enum no not null support pretty=false") {
    val ddl = toAllNullableDdl(Codec[Enum], pretty = false)
    assert(
      ddl ==
        "discriminant STRING,product1 STRUCT<a INT,b STRING>,product2 STRUCT<c INT,d BIGINT>,product3 STRUCT<c INT,d STRUCT<a INT,b STRING>>"
    )
  }

  test("toDdl Containers") {
    val ddl = toSparkDdl(Codec[Containers])
    assert(ddl == """
                    |arr ARRAY<STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> /*NOT NULL*/> NOT NULL,
                    |map MAP<STRING /*NOT NULL*/, ARRAY<STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> /*NOT NULL*/> /*NOT NULL*/> NOT NULL,
                    |opt ARRAY<STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> /*NOT NULL*/>
                    |""".stripMargin)
  }

  test("toDdl Option inside List") {
    val ddl = toSparkDdl(Codec[List[Option[Person]]])
    assert(ddl == """
                    |value ARRAY<STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |>> NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Option inside Map key") {
    val ddl = toSparkDdl(Codec[Map[Option[String], Person]])
    assert(ddl == """
                    |value MAP<STRING, STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |> /*NOT NULL*/> NOT NULL
                    |""".stripMargin)
  }

  test("toDdl Option inside Map value") {
    val ddl = toSparkDdl(Codec[Map[String, Option[Person]]])
    assert(ddl == """
                    |value MAP<STRING /*NOT NULL*/, STRUCT<
                    |  name STRING NOT NULL,
                    |  age INT NOT NULL
                    |>> NOT NULL
                    |""".stripMargin)
  }

  test("toDdl for Option inside Option") {
    val ddl = toSparkDdl(Codec[Option[Option[String]]])
    assert(ddl == """
                    |value STRUCT<
                    |  value STRING
                    |>
                    |""".stripMargin)
  }

  test("toDdl for deeply nested Options") {
    val ddl = toSparkDdl(Codec[Option[Option[Option[Option[String]]]]])
    assert(ddl == """
                    |value STRUCT<
                    |  value STRUCT<
                    |    value STRUCT<
                    |      value STRING
                    |    >
                    |  >
                    |>
                    |""".stripMargin)
  }

  test("toDdl for enum deriving Codec.EnumAsString") {
    val ddl = toSparkDdl(Codec[EnumString])
    assert(ddl == """
                    |value STRING NOT NULL /*one of: a, b*/
                    |""".stripMargin)
  }

  test("toDdl for Option of enum deriving Codec.EnumAsString") {
    val ddl = toSparkDdl(Codec[Option[EnumString]])
    assert(ddl == """
                    |value STRING /*one of: a, b*/
                    |""".stripMargin)
  }

  test("toDdl for Decimal") {
    val ddl = toSparkDdl(Codec[Decimal[10, 2]])
    assert(ddl == """
                    |value DECIMAL(10,2) NOT NULL
                    |""".stripMargin)
  }

  test("toDdl for Option[Decimal]") {
    val ddl = toSparkDdl(Codec[Option[Decimal[10, 2]]])
    assert(ddl == """
                    |value DECIMAL(10,2)
                    |""".stripMargin)
  }

  test("toDdl escape identifiers when needed") {
    val ddl = toSparkDdl(Codec[(select: Int, `with space`: Long)])
    assert(ddl == """
                    |`select` INT NOT NULL,
                    |`with space` BIGINT NOT NULL
                    |""".stripMargin)
  }

  test("toDdl for Decimal bigquery") {
    assert(toBigQueryDecimal(Codec[Decimal[10, 2]], parameterized = true) == "value DECIMAL(10,2) NOT NULL")
    assert(toBigQueryDecimal(Codec[Decimal[10, 2]], parameterized = false) == "value DECIMAL NOT NULL")
    assert(
      toBigQueryDecimal(Codec[Decimal[38, 0]], parameterized = true) == "value BIGDECIMAL(38,0) NOT NULL"
    )
    assert(toBigQueryDecimal(Codec[Decimal[38, 0]], parameterized = false) == "value BIGDECIMAL NOT NULL")
  }

  def testSparkDdlType[T: Codec: SimpleTypeName](expected: String) =
    test(s"toDdlType ${SimpleTypeName[T]}") {
      val ddl = toSparkDdlType(Codec[T])
      assert(ddl == expected)
    }

  testSparkDdlType[Int]("INT")
  testSparkDdlType[Option[Int]]("INT")
  testSparkDdlType[Option[Option[Int]]]("STRUCT<value INT>")
  testSparkDdlType[Seq[Int]]("ARRAY<INT>")
  testSparkDdlType[Seq[Option[Int]]]("ARRAY<INT>")
  testSparkDdlType[(value: Int)]("STRUCT<value INT NOT NULL>")
}
