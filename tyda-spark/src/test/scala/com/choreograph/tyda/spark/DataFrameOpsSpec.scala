package com.choreograph.tyda.spark
import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.*

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.spark.CodecToEncoder.convert

object DataFrameOpsSpec {
  final case class Wide(a: Int, b: String, c: String, d: String, e: String)
  final case class WideUnpivot(a: Int, b: String, column: String, value: String)
}

class DataFrameOpsSpec extends AnyFunSuite, SharedSparkSession {
  import DataFrameOpsSpec.{Wide, WideUnpivot}
  import DataFrameOps.*

  test("unpivotAs support basic unpivot") {
    val input = Arbitrary[Wide]()
    val tmp = WideUnpivot(input.a, input.b, "", "")
    val expected = Seq(
      tmp.copy(column = "c", value = input.c),
      tmp.copy(column = "d", value = input.d),
      tmp.copy(column = "e", value = input.e)
    )
    val output = spark.createDataset(Seq(input)).toDF().unpivotAs[WideUnpivot]().collect().toList
    output must contain theSameElementsAs (expected)
  }

  test("unpivotAs support pivot with 0 id columns") {
    val input = Arbitrary[(String, String, String, String, String)]()
    val expected = input.productElementNames.zip(input.productIterator.map(_.toString)).toSeq
    val output = spark.createDataset(Seq(input)).toDF().unpivotAs[(String, String)]().collect().toList
    output must contain theSameElementsAs (expected)
  }

  test("unpivotAs should allow cast of numeric types to String") {
    val input = Arbitrary[(String, String, Int, Long, Float)]()
    val expected = input.productElementNames.zip(input.productIterator.map(_.toString)).toSeq
    val output = spark.createDataset(Seq(input)).toDF().unpivotAs[(String, String)]().collect().toList
    output must contain theSameElementsAs (expected)
  }

  test("unpivotAs should allow cast widing casts of integers") {
    val input = Arbitrary[(Long, Int, Short, Byte)]()
    val expected = Seq(("_1", input._1), ("_2", input._2), ("_3", input._3), ("_4", input._4))
    val output = spark.createDataset(Seq(input)).toDF().unpivotAs[(String, Long)]().collect().toList
    output must contain theSameElementsAs (expected)
  }

  test("unpivotAs should allow cast widing casts of floating points") {
    val input = Arbitrary[(Double, Float)]()
    val expected = Seq(("_1", input._1), ("_2", input._2.toDouble))
    val output = spark.createDataset(Seq(input)).toDF().unpivotAs[(String, Double)]().collect().toList
    given Equality[(String, Double)] =
      new Equality[(String, Double)] {
        override def areEqual(a: (String, Double), b: Any): Boolean =
          b match {

            case (s: String, d: Double) => Ordering[(String, Double)].equiv(a, (s, d))
            case _ => false
          }
      }
    output must contain theSameElementsAs (expected)
  }

  test("unpivotAs should give error on unsupported cast String->Int") {
    val input = Arbitrary[String]()
    val df = spark.createDataset(Seq(input)).toDF()
    assertThrows[RuntimeException] { df.unpivotAs[(String, Int)]() }
  }

  test("unpivotAs should give error on unsupported cast Struct->String") {
    val input = Arbitrary[Tuple1[(String, String)]]()
    val df = spark.createDataset(Seq(input)).toDF()
    assertThrows[RuntimeException] { df.unpivotAs[(String, String)]() }
  }

  test("unpivotAs should give error on unsupported cast Option[Int]->Int") {
    val input = Arbitrary[Option[Int]]()
    val df = spark.createDataset(Seq(input)).toDF()
    assertThrows[RuntimeException] { df.unpivotAs[(String, Int)]() }
  }
}
