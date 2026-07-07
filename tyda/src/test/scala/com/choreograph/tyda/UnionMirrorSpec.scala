package com.choreograph.tyda

import scala.annotation.nowarn
import scala.deriving.Mirror

import org.scalatest.funsuite.AnyFunSuite

object UnionMirrorSpec {
  opaque type MyInt = Int
}

class UnionMirrorSpec extends AnyFunSuite {
  import UnionMirrorSpec.MyInt
  import UnionMirror.given

  test("mirror for Int | String should work") {
    val mirror = summon[Mirror.SumOf[Int | String]]
    summon[mirror.MirroredMonoType =:= (Int | String)]
    summon[mirror.MirroredLabel =:= "Int | String"]
    summon[mirror.MirroredElemTypes =:= String *: Int *: EmptyTuple]
    summon[mirror.MirroredElemLabels =:= "String" *: "Int" *: EmptyTuple]
    (0 to 100).foreach(_ => assert(mirror.ordinal(Arbitrary[String]()) == 0))
    (0 to 100).foreach(_ => assert(mirror.ordinal(Arbitrary[Int]()) == 1))
  }

  test("mirror for List[Int] | Int should work") {
    val mirror = summon[Mirror.SumOf[List[Int] | Int]]
    summon[mirror.MirroredMonoType =:= (List[Int] | Int)]
    summon[mirror.MirroredLabel =:= "List[Int] | Int"]
    summon[mirror.MirroredElemTypes =:= Int *: List[Int] *: EmptyTuple]
    summon[mirror.MirroredElemLabels =:= "Int" *: "List[Int]" *: EmptyTuple]
  }

  test("remove duplicate cases") {
    val mirror1 = summon[Mirror.SumOf[Int | String | Int]]
    val mirror2 = summon[Mirror.SumOf[Int | Int | String]]
    val mirror3 = summon[Mirror.SumOf[String | Int | Int]]
    summon[mirror1.MirroredMonoType =:= (Int | String)]
    summon[mirror1.MirroredLabel =:= "Int | String"]
    summon[mirror1.MirroredElemTypes =:= String *: Int *: EmptyTuple]
    summon[mirror1.MirroredElemLabels =:= "String" *: "Int" *: EmptyTuple]
    summon[mirror1.MirroredMonoType =:= mirror2.MirroredMonoType]
    summon[mirror1.MirroredElemTypes =:= mirror2.MirroredElemTypes]
    summon[mirror1.MirroredElemLabels =:= mirror2.MirroredElemLabels]
    summon[mirror1.MirroredMonoType =:= mirror3.MirroredMonoType]
    summon[mirror1.MirroredElemTypes =:= mirror3.MirroredElemTypes]
    summon[mirror1.MirroredElemLabels =:= mirror3.MirroredElemLabels]
    assert(mirror1.ordinal(Arbitrary[String]()) == 0)
    assert(mirror1.ordinal(Arbitrary[Int]()) == 1)
    assert(mirror2.ordinal(Arbitrary[String]()) == 0)
    assert(mirror2.ordinal(Arbitrary[Int]()) == 1)
    assert(mirror3.ordinal(Arbitrary[String]()) == 0)
    assert(mirror3.ordinal(Arbitrary[Int]()) == 1)
  }

  test("mirror should be independent of case order") {
    val mirror1 = summon[Mirror.SumOf[Int | String]]
    val mirror2 = summon[Mirror.SumOf[String | Int]]
    summon[mirror1.MirroredMonoType =:= mirror2.MirroredMonoType]
    summon[mirror1.MirroredElemTypes =:= mirror2.MirroredElemTypes]
    summon[mirror1.MirroredElemLabels =:= mirror2.MirroredElemLabels]
    summon[mirror1.MirroredLabel =:= "Int | String"]
    summon[mirror2.MirroredLabel =:= "Int | String"]
    assert(mirror1.ordinal(Arbitrary[Int]()) == mirror2.ordinal(Arbitrary[Int]()))
    assert(mirror1.ordinal(Arbitrary[String]()) == mirror2.ordinal(Arbitrary[String]()))
  }

  test("mirror array of primitive") {
    val mirror = summon[Mirror.SumOf[Array[Int] | Array[Long]]]
    summon[mirror.MirroredMonoType =:= (Array[Int] | Array[Long])]
    summon[mirror.MirroredLabel =:= "Array[Long] | Array[Int]"]
    summon[mirror.MirroredElemTypes =:= Array[Int] *: Array[Long] *: EmptyTuple]
    summon[mirror.MirroredElemLabels =:= "Array[Int]" *: "Array[Long]" *: EmptyTuple]
    assert(mirror.ordinal(Array(1)) == 0)
    assert(mirror.ordinal(Array(2L)) == 1)
  }

  test("should not be derivable for non union type") { assertDoesNotCompile("UnionMirror.derived[Int]") }

  /* The test here is that a warning is produced. This works because nowarn that silences nothing generates a
   * separate warning. */
  test("mirror should warn where ordinal implementation does not work") {
    val mirror = (summon[Mirror.SumOf[List[Int] | List[String]]]: @nowarn("id=E092"))
    summon[mirror.MirroredLabel =:= "List[String] | List[Int]"]
  }

  test("mirror should warn for opaque types same erasure") {
    // This could never work since MyInt and Int are indistinguishable at runtime.
    val mirror = (summon[Mirror.SumOf[MyInt | Int]]: @nowarn("id=E092"))
    summon[mirror.MirroredLabel =:= "Int | MyInt"]
  }

  test("mirror should warn for opaque types different erasure") {
    // This could in theory be supported, but it unclear how to implement the ordinal method.
    val mirror = (summon[Mirror.SumOf[MyInt | Double]]: @nowarn("id=E092"))
    summon[mirror.MirroredLabel =:= "Double | MyInt"]
  }
}
