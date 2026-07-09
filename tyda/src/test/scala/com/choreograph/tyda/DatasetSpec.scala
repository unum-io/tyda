package com.choreograph.tyda

import scala.annotation.nowarn
import scala.deriving.Mirror

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.aggregates.sum
import com.choreograph.tyda.compiletimeextras.assertCompileTimeError
import com.choreograph.tyda.functions.explode
import com.choreograph.tyda.functions.seq

class DatasetSpec extends AnyFunSuite {
  test("support where") {
    val ds = Dataset.FromSeq(Seq((1, true), (2, false)))
    ds.where(_._2) match {
      case Dataset.Filter(`ds`, compiled) =>
        assert(compiled.expr == ExprNode.Select[(Int, Boolean), Boolean](compiled.arg, "_2"))
      case _ => fail("Expected Dataset.Filter")
    }
  }

  test("support map") {
    val ds: Dataset[(Int, Int)] = Dataset.FromSeq(Seq((1, 1)))
    // The test here is that the following should compile without and type hints
    ds.map(i => (i._1, (i._2, 1)))
  }

  test("support select") {
    val ds: Dataset[(Int, Int)] = Dataset.FromSeq(Seq((1, 1)))
    ds.select(_._1, _._2)
  }

  test("support Expr extractor in select") {
    val ds: Dataset[(Int, Int)] = Dataset.FromSeq(Seq((1, 1)))
    ds.select { case Expr(a, _) => a }
  }

  test("support limit") {
    val ds: Dataset[Int] = Dataset.FromSeq(Seq(1, 2, 3))
    ds.limit(5) match {
      case Dataset.Limit(`ds`, 5) => ()
      case _ => fail("Expected Dataset.Limit")
    }
  }

  test("reject negative limit") {
    val ds: Dataset[Int] = Dataset.FromSeq(Seq(1, 2, 3))
    val exception = intercept[IllegalArgumentException] { ds.limit(-1) }
    assert(exception.getMessage.contains("non-negative"))
  }

  test("Do not allow Map in groupBy") {
    @nowarn("msg=unused")
    val ds: Dataset[(Map[Int, Int], Int)] = Dataset.FromSeq(Seq((Map(1 -> 1), 1)))
    assertCompileTimeError("ds.groupBy(r => (g = r._1))", "Map[Int, Int] is not groupable")
    assertCompileTimeError("ds.grouped", "Map[Int, Int] is not groupable")
    assertCompileTimeError("ds.groupByKey(identity)", "Map[Int, Int] is not groupable")
  }

  test("Do not allow Set in groupBy") {
    given Codec[Set[Int]] = Codec.Iterable[Int, Set[Int]](summon, summon)
    @nowarn("msg=unused")
    val ds: Dataset[(Set[Int], Int)] = Dataset.FromSeq(Seq((Set(1, 2), 1)))
    assertCompileTimeError("ds.groupBy(r => (g = r._1))", "Set[Int] is not groupable")
    assertCompileTimeError("ds.grouped", "Set[Int] is not groupable")
    assertCompileTimeError("ds.groupByKey(identity)", "Set[Int] is not groupable")
  }

  test("Do not allow sum of unsupported types") {
    val ds: Dataset[(String, Int)] = Dataset.FromSeq(Seq(("a", 1)))
    // Sanity check
    val _ = ds.aggregate(sum(_._2))
    assertCompileTimeError("ds.aggregate(sum(_._1))", "Sum is not supported for type String")
  }

  test("reject nested explode inside struct") {
    val ds: Dataset[(Seq[(String, Int)], Seq[(String, Int)])] =
      Dataset.FromSeq(Seq((Seq(("a", 1)), Seq(("a", 1)))))
    val _ = ds.select(x => explode(seq(x)))
    assertCompileTimeError(
      "ds.select(x => (explode(Tuple1(explode(x)))))",
      "No given instance of type",
      "tyda.Expr.AsExpr"
    )
  }

  test("reject nested explode") {
    val ds: Dataset[Seq[Seq[Int]]] = Dataset.FromSeq(Seq(Seq(Seq(1))))
    val _ = ds
    assertCompileTimeError(
      "ds.select(x => explode(explode(x)))",
      "No given instance of type",
      "tyda.Expr.AsExpr"
    )
  }

  /* We are not using the public api here since different since for value equality the references in compiled
   * expression will make the differ. If we and some semantic equality we could use that instead. */
  private val ref = ExprNode.Reference[(Int, Int)]()
  private def pred(limit: Int) =
    CompiledExpr[(Int, Int), Boolean](ref, (ref: ExprNode[(Int, Int)])._1 > limit)
  private val statement = CompiledExpr[(Int, Int), Int](_._2)
  private def compute(ds: Dataset[(Int, Int)], limit: Int = 0): Dataset[Int] =
    Dataset.Select(Dataset.Filter(ds, pred(limit)), statement)

  test("Support TreeApi") {
    val ds = compute(Dataset.from(Seq((1, 1))))
    val expected = compute(Dataset.empty)
    val rewrite: [t] => Dataset[t] => Continue[Dataset[t]] = [t] =>
      _ match {
        case Dataset.FromSeq(_, codec) => Continue(Dataset.FromSeq(Seq.empty, codec))
        case other => Continue(other)
      }
    assert(ds.transformUp(rewrite) == expected)
    assert(ds.transformDown(rewrite) == expected)
  }

  test("Support TreeApi for Dataset on Action") {
    def write(ds: Dataset[(Int, Int)]): Dataset.Action = compute(ds).writeToPath("/tmp", Format.Parquet)
    val action = write(Dataset.from(Seq((1, 1))))
    val expected = write(Dataset.empty)
    val rewrite: [t] => Dataset[t] => Continue[Dataset[t]] = [t] =>
      _ match {
        case Dataset.FromSeq(_, codec) => Continue(Dataset.FromSeq(Seq.empty, codec))
        case other => Continue(other)
      }
    assert(action.transformUp(rewrite) == expected)
    assert(action.transformDown(rewrite) == expected)
  }

  test("Support TreeApi for ExprNode on Action") {
    def write(limit: Int): Dataset.Action =
      compute(Dataset.from(Seq((1, 2))), limit).writeToPath("/tmp", Format.Parquet)
    val action = write(0)
    val expected = write(10)
    val rewrite: [t] => ExprNode[t] => Continue[ExprNode[t]] = [t] =>
      _ match {
        case ExprNode.Literal(_, Codec.Int) => Continue(ExprNode.Literal(10, Codec.Int))
        case other => Continue(other)
      }
    assert(action.transformUpExprs(rewrite) == expected)
    assert(action.transformDownExprs(rewrite) == expected)
  }
  test("Dataset should be a GADT") { summon[Mirror.SumOf[Dataset[?]]] }

  test("Dataset.single should be convertable to Expr") { val _ = Dataset.single(42).value }
}
