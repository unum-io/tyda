package com.choreograph.tyda.arrow

import scala.reflect.Typeable
import scala.util.Using

import org.apache.arrow.memory.RootAllocator
import org.scalactic.Equality
import org.scalatest.BeforeAndAfterAll
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

object ArrowRoundTripSuite {
  private final case class MyProduct(a: Option[Int], b: String, c: (Int, Option[Int]))
      derives Arbitrary, Codec
  private enum MyEnum extends EnumStableHashCode derives Arbitrary, Codec {
    case A, B
    case C(a: Int, b: Option[Int])
    case D(a: Int, b: Option[Int])
    case E
    case F(prod: MyProduct)
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

class ArrowRoundTripSuite extends AnyFunSuite, BeforeAndAfterAll {
  import ArrowRoundTripSuite.{MyProduct, MyEnum, given}

  val rootAllocator = new RootAllocator()

  override def afterAll(): Unit = rootAllocator.close()

  def testRoundTrip[T: {Arbitrary, Codec, TypeName, Equality}] = {
    test(s"roundtrip vector for ${TypeName.name[T]}") {
      val values = Vector.fill(100)(Arbitrary[T]())
      val allocator = rootAllocator.newChildAllocator(TypeName.name[T], 0, Long.MaxValue)
      val deserialized = Using.resource(allocator) { allocator =>
        Using.resource(ArrowSerializer.toVector[T](values.iterator, allocator)) { serialized =>
          ArrowDeserializer.fromVector[T](serialized).toSeq
        }
      }
      assert(deserialized.size == values.size)
      deserialized
        .zip(values)
        .zipWithIndex
        .map { case ((value, expected), index) =>
          assert(value === expected, s"Found value $value expected $expected at index $index")
        }
    }
    test(s"roundtrip IPC for ${TypeName.name[T]}") {
      val values = Vector.fill(100)(Arbitrary[T]())
      val deserialized = Using.resource(new RootAllocator()) { allocator =>
        val ipcBytes = ArrowSerializer.toIpcBytes[T](values, allocator)
        ArrowDeserializer.fromIpcBytes[T](ipcBytes, allocator)(_.toIndexedSeq)
      }
      assert(deserialized.size == values.size)
      deserialized
        .zip(values)
        .zipWithIndex
        .map { case ((value, expected), index) =>
          assert(value === expected, s"Found value $value expected $expected at index $index")
        }
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
  testRoundTrip[Decimal[38, 9]]
  testRoundTrip[Date]
  testRoundTrip[Timestamp]
  testRoundTrip[Duration]
  testRoundTrip[Option[Int]]
  testRoundTrip[Option[Option[Int]]]
  testRoundTrip[Seq[Int]]
  testRoundTrip[Seq[Seq[Int]]]
  testRoundTrip[Seq[(Int, Option[Int])]]
  testRoundTrip[Map[String, Int]]
  testRoundTrip[Map[(String, Int), (String, Option[Int])]]
  testRoundTrip[Map[Seq[String], Seq[Int]]]
  testRoundTrip[(Int, Option[Int])]
  testRoundTrip[MyProduct]
  testRoundTrip[MyEnum]
}
