package com.choreograph.tyda.parquet

import java.nio.file.Files

import scala.reflect.Typeable
import scala.util.Using

import org.apache.commons.io.FileUtils
import org.apache.hadoop.fs.Path
import org.apache.parquet.hadoop.ParquetReader
import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Ord
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName

object ParquetRoundTripSuite {
  private final case class MyProduct(a: Option[Int], b: String, c: (Int, Option[Int]))
      derives Arbitrary, Codec
  private enum MyEnum derives Arbitrary, Codec {
    case A, B
    case C(a: Int, b: Option[Int])
    case D(a: Int, b: Option[Int])
    case E
    case F(prod: MyProduct)
  }

  private enum SimpleEnum derives Arbitrary, Codec.EnumAsString {
    case X, Y, Z
  }

  final case class TmpDir(path: java.nio.file.Path)
  object TmpDir {
    def apply(): TmpDir = new TmpDir(Files.createTempDirectory("parquet-roundtrip-"))
    given Using.Releasable[TmpDir] = tmp => FileUtils.deleteDirectory(tmp.path.toFile)
  }

  def equalityFromOrd[T: Ord: Typeable]: Equality[T] =
    new Equality[T] {
      override def areEqual(a: T, b: Any): Boolean =
        b match {
          case b: T => Ord[T].equiv(a, b)
          case _ => false
        }
    }

  given Equality[Float] = equalityFromOrd[Float]
  given Equality[Double] = equalityFromOrd[Double]
}

class ParquetRoundTripSuite extends AnyFunSuite {
  import ParquetRoundTripSuite.{MyProduct, MyEnum, SimpleEnum, TmpDir, given}

  def testRoundTrip[T: {Arbitrary, Codec, TypeName, Equality}] =
    test(s"roundtrip parquet for ${TypeName.name[T]}") {
      val values = Vector.fill(100)(Arbitrary[T]())
      val deserialized = Using.resource(TmpDir())(tmpDir => {
        val hadoopPath = Path(tmpDir.path.toString(), "data.parquet")
        Using.resource(CodecParquetWriter[T](hadoopPath))(writer => values.foreach(writer.write))
        val reader = ParquetReader.builder(CodecReadSupport[T](), hadoopPath).build()
        val read = Iterator.continually(reader.read()).takeWhile(_ != null).toIndexedSeq
        reader.close()
        read
      })
      assert(deserialized.size == values.size)
      deserialized
        .zip(values)
        .zipWithIndex
        .map { case ((value, expected), index) =>
          assert(value === expected, s"Found value $value expected $expected at index $index")
        }
    }

  testRoundTrip[Boolean]
  testRoundTrip[Byte]
  testRoundTrip[Short]
  testRoundTrip[Int]
  testRoundTrip[Long]
  testRoundTrip[Float]
  testRoundTrip[Double]
  testRoundTrip[String]
  testRoundTrip[Decimal[3, 0]]
  testRoundTrip[Decimal[15, 5]]
  testRoundTrip[Decimal[38, 9]]
  testRoundTrip[Seq[Decimal[15, 5]]]
  testRoundTrip[Option[Decimal[38, 3]]]
  testRoundTrip[(Decimal[15, 5], Decimal[3, 0])]
  testRoundTrip[Binary]
  testRoundTrip[Date]
  testRoundTrip[Timestamp]
  testRoundTrip[Duration]
  testRoundTrip[Option[Int]]
  testRoundTrip[Option[Option[Int]]]
  testRoundTrip[Tuple1[Option[Option[Int]]]]
  testRoundTrip[Seq[Int]]
  testRoundTrip[Option[Seq[Int]]]
  testRoundTrip[Seq[Seq[Int]]]
  testRoundTrip[Seq[(Int, Option[Int])]]
  testRoundTrip[Map[String, Int]]
  testRoundTrip[Option[Map[String, Int]]]
  testRoundTrip[Map[(String, Int), (String, Option[Int])]]
  testRoundTrip[Map[Seq[String], Seq[Int]]]
  testRoundTrip[(Int, Option[Int])]
  testRoundTrip[MyProduct]
  testRoundTrip[Option[MyProduct]]
  testRoundTrip[MyEnum]
  testRoundTrip[Option[MyEnum]]
  testRoundTrip[SimpleEnum]
  testRoundTrip[Option[SimpleEnum]]

}
