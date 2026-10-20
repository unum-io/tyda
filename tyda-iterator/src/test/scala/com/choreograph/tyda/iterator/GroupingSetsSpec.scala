package com.choreograph.tyda.iterator

import scala.annotation.nowarn

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.aggregates.sum
import com.choreograph.tyda.compiletimeextras.assertCompileTimeError
import com.choreograph.tyda.functions.cube
import com.choreograph.tyda.functions.rollup

class GroupingSetsSpec extends AnyFunSuite {
  private val ds: Dataset[(a1: Int, a2: String, a3: Boolean, v: Long)] =
    Dataset.from(Seq((1, "x", true, 1L), (1, "y", false, 2L), (2, "x", true, 4L)))

  extension [T](ds: Dataset[T]) { private def compute: Seq[T] = IteratorRunner.collect(ds) }

  test("cube groups by all subsets") {
    val result = ds.groupBy(r => cube((a1 = r.a1, a2 = r.a2))).aggregate(r => (v = sum(r.v))).compute
    assert(
      result.toSet == Set(
        (Some(1), Some("x"), 1L),
        (Some(1), Some("y"), 2L),
        (Some(2), Some("x"), 4L),
        (Some(1), None, 3L),
        (Some(2), None, 4L),
        (None, Some("x"), 5L),
        (None, Some("y"), 2L),
        (None, None, 7L)
      )
    )
    assert(result.size == 8)
  }

  test("rollup groups by all prefixes") {
    val result = ds.groupBy(r => rollup((a1 = r.a1, a2 = r.a2))).aggregate(r => (v = sum(r.v))).compute
    assert(
      result.toSet == Set(
        (Some(1), Some("x"), 1L),
        (Some(1), Some("y"), 2L),
        (Some(2), Some("x"), 4L),
        (Some(1), None, 3L),
        (Some(2), None, 4L),
        (None, None, 7L)
      )
    )
    assert(result.size == 6)
  }

  test("cube concatenated with a regular key") {
    val result = ds.groupBy(r => cube((a1 = r.a1)) ++ (a3 = r.a3)).aggregate(_ => (n = count())).compute
    assert(
      result.toSet == Set(
        (Some(1), true, 1L),
        (Some(1), false, 1L),
        (Some(2), true, 1L),
        (None, true, 2L),
        (None, false, 1L)
      )
    )
  }

  test("cube concatenated with cube is the cartesian product of the grouping sets") {
    val result = ds
      .groupBy(r => cube((a1 = r.a1, a2 = r.a2)) ++ cube((a3 = r.a3)))
      .aggregate(_ => (n = count()))
      .compute
    assert(result.size == 18)
    assert(result.contains((None, None, None, 3L)))
    assert(result.contains((None, None, Some(true), 2L)))
    assert(result.contains((Some(1), Some("y"), None, 1L)))
  }

  test("cube of optional fields distinguishes None values from grouped away fields") {
    val optional: Dataset[(a: Option[Int], v: Long)] = Dataset.from(Seq((None, 1L), (Some(1), 2L)))
    val result = optional.groupBy(r => cube((a = r.a))).aggregate(r => (v = sum(r.v))).compute
    assert(result.toSet == Set((Some(None), 1L), (Some(Some(1)), 2L), (None, 3L)))
  }

  test("cube keys keep their names") {
    val result: Dataset[(a1: Option[Int], a2: Option[String], a3: Boolean, n: Long)] = ds
      .groupBy(r => cube((a1 = r.a1, a2 = r.a2)) ++ (a3 = r.a3))
      .aggregate(_ => (n = count()))
    assert(result.compute.nonEmpty)
  }

  test("cube can not be used outside of groupBy") {
    @nowarn("msg=unused")
    val local = ds
    assertCompileTimeError("local.select(r => cube((a1 = r.a1)))", "")
    assertCompileTimeError("local.where(r => cube((a1 = r.a1)) == cube((a1 = r.a1)))", "")
    assertCompileTimeError("local.groupBy(r => cube((a1 = cube((a2 = r.a2)))))", "")
  }

  test("cube can not reuse field names") {
    @nowarn("msg=unused")
    val local = ds
    assertCompileTimeError("local.groupBy(r => cube((a1 = r.a1)) ++ (a1 = r.a2))", "")
  }
}
