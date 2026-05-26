package com.choreograph.tyda

import scala.annotation.unused

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

object ProjectorSpec {
  final case class Full(x: Int, y: Int, s: IndexedSeq[Int]) derives Arbitrary
  final case class Partial(x: Int, s: IndexedSeq[Int])
  final case class PartialWrong(x: Long, z: String)
}

class ProjectorSpec extends AnyFunSuite {
  import ProjectorSpec.{Full, Partial, PartialWrong}

  test("Projector for Full -> Partial") {
    val projector = summon[Projector[Full, Partial]]
    val full = Arbitrary[Full]()
    val expected = Partial(full.x, full.s)
    assert(projector(full) == expected)
  }

  test("Projector support named tuples") {
    val projector = summon[Projector[Full, (x: Int, y: Int)]]
    val full = Arbitrary[Full]()
    val expected = (full.x, full.y)
    assert(projector(full) == expected)
  }

  /* The fast part is not check explicitly but if the implemenation is very slow it should be noticed early
   * but covering cases with large classes here. */
  test("support and be fast to compile for big products") {
    val projector = summon[Projector[BigNamedTuple, (i100: Int)]]
    val big = Arbitrary[BigNamedTuple]()
    val expected = Tuple1(big.i100)
    assert(projector(big) == expected)
  }

  test("report all missing fields and type mismatches") {
    assertCompileTimeError(
      "summon[Projector[Full, PartialWrong]]",
      s"Cannot derive Projector from ${TypeName.name[Full]} to ${TypeName.name[PartialWrong]}",
      "field 'x' has type Int in Full but Long in PartialWrong",
      "no field named 'z' in Full"
    )
  }

  test("report truncate large NamedTuple in per field error messages") {
    // Unused needed due to https://github.com/scala/scala3/issues/21805
    @unused
    type Target = (i10: Int, i100: String, i1000: Int, i1001: Int)
    val trucatedBig = "BigNamedTuple"
    assertCompileTimeError(
      "summon[Projector[BigNamedTuple, Target]]",
      "Cannot derive Projector from com.choreograph.tyda.BigNamedTuple",
      s"no field named 'i1000' in $trucatedBig",
      s"no field named 'i1001' in $trucatedBig",
      s"field 'i100' has type Int in $trucatedBig but String in (i10: Int, i100: String, i1000: Int, i1001: Int)"
    )
  }
}
