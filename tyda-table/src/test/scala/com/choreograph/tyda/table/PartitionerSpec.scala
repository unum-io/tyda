package com.choreograph.tyda.table

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec

object PartitionerSpec {
  enum EnumPartitionValue derives Codec.EnumAsString {
    case A
    case B
  }
  final case class MyPartitions(p1: String, p2: String, p3: Int)
}

class PartitionerSuite extends AnyFunSuite {
  import PartitionerSpec.{EnumPartitionValue, MyPartitions}

  test("None support empty path") {
    val partitioner = Partitioner.None
    assert(partitioner.path("/base/path") == "/base/path")
  }

  private val partitionValue = MyPartitions("v1", "v2", 3)
  private val basePath = "/base/path"
  private val expectedEncoded = s"$basePath/p1=v1/p2=v2/p3=3/"

  private val decoder = summon[Partitioner.Determinator[MyPartitions, Partitioner.Hive[MyPartitions]]]

  test("Hive support encoding fully fixed") {
    val partitioner = Partitioner.Hive.fromValue(partitionValue)
    assert(partitioner.path(basePath) == expectedEncoded)
  }

  test("Hive support encoding partially fixed") {
    val partitioner = Partitioner.Hive.fromSeqs[MyPartitions](Seq("v1", "v2"), Seq(), Seq(3))
    assert(partitioner.path(basePath) == basePath + "/p1={v1,v2}/p2=*/p3=3/")
  }

  test("Hive support trailing / in basePath") {
    val partitioner = Partitioner.Hive.fromValue(partitionValue)
    assert(partitioner.path(basePath + "/") == expectedEncoded)
  }

  test("Hive support decoding") { assert(decoder.decode(expectedEncoded) == partitionValue) }

  test("Hive support / in value using url encoding") {
    val partitioner = Partitioner.Hive.fromValue(MyPartitions("v1/v2", "v3", 3))
    assert(partitioner.path(basePath) == s"$basePath/p1=v1%2Fv2/p2=v3/p3=3/")
  }

  test("Hive fail on incomplete path") {
    def assertThrowsContains(path: String, expected: String): Unit = {
      val e = intercept[RuntimeException] { decoder.decode(path) }
      assert(e.getMessage.contains(expected)): Unit
    }
    assertThrowsContains(basePath, "hive partition")
    assertThrowsContains(basePath + "/p1=v1", "hive partition")
    assertThrowsContains(basePath + "/p1=v1/p2=v2", "hive partition")
    assertThrowsContains(basePath + "/p1=v1/p2=v2/p4=4", "hive partition")
    assertThrowsContains(basePath + "/p1=v1/p2=v2/p3=abc", "abc")
  }

  test("Hive support empty product") {
    val partitioner = Partitioner.Hive.fromValue(EmptyTuple)
    assert(partitioner.path(basePath) == basePath)
    assert(
      summon[Partitioner.Determinator[EmptyTuple, Partitioner.Hive[EmptyTuple]]].decode("/base/path/") ==
        EmptyTuple
    )
  }

  test("Hive support enums") {
    type Partition = Tuple1[EnumPartitionValue]
    val value = Arbitrary[Partition]()
    val partitioner = Partitioner.Hive.fromValue(value)
    val encoded = s"$basePath/_1=${value._1.toString}/"
    assert(partitioner.path(basePath) == encoded)
    assert(summon[Partitioner.Determinator[Partition, Partitioner.Hive[Partition]]].decode(encoded) == value)
  }

  test("Hive should roundtrip") {
    (0 to 1000).foreach { _ =>
      val value = Arbitrary[MyPartitions]()
      val partitioner = Partitioner.Hive.fromValue(value)
      assert(decoder.decode(partitioner.path(basePath)) == value)
    }
  }
}
