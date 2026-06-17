package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

class BinarySpec extends AnyFunSuite {
  test("equal binaries compare as 0") {
    val binary1 = Binary.fromArray(Array[Byte](1, 2, 3))
    val binary2 = Binary.fromArray(Array[Byte](1, 2, 3))
    assert(Ordering[Binary].compare(binary1, binary2) == 0)
  }

  test("shorter prefix is less than longer") {
    val shorter = Binary.fromArray(Array[Byte](1))
    val longer = Binary.fromArray(Array[Byte](1, 2))
    assert(Ordering[Binary].compare(shorter, longer) < 0)
  }

  test("unsigned byte ordering: 0xff > 0x01") {
    val binary1 = Binary.fromArray(Array[Byte](0xff.toByte))
    val binary2 = Binary.fromArray(Array[Byte](1))
    assert(Ordering[Binary].compare(binary1, binary2) > 0)
  }

  test("unsigned byte ordering: 0x80 > 0x7f") {
    val binary1 = Binary.fromArray(Array[Byte](0x80.toByte))
    val binary2 = Binary.fromArray(Array[Byte](0x7f.toByte))
    assert(Ordering[Binary].compare(binary1, binary2) > 0)
  }

  test("empty binary is least") {
    val empty = Binary.empty
    val nonEmpty = Binary.fromArray(Array[Byte](0))
    assert(Ordering[Binary].compare(empty, nonEmpty) < 0)
  }

  test("lexicographic: first differing byte determines order") {
    val binary1 = Binary.fromArray(Array[Byte](1, 3))
    val binary2 = Binary.fromArray(Array[Byte](1, 2))
    assert(Ordering[Binary].compare(binary1, binary2) > 0)
  }
}
