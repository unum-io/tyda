package com.choreograph.tyda.meta

import org.scalatest.funsuite.AnyFunSuite
import shapeless3.deriving.Labelling

import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.aggregates.countIf
import com.choreograph.tyda.aggregates.max
import com.choreograph.tyda.compiletimeextras.assertCompileTimeError
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.functions.none
import com.choreograph.tyda.functions.tuple
import com.choreograph.tyda.iterator.AggregateExprEvaluation
import com.choreograph.tyda.iterator.ExprEvaluation

object K0ExprSpec {
  enum Enum {
    case A, B, C
  }

  trait AnyNone[T] extends ExprFn[T, Boolean]
  object AnyNone {
    def apply[T: AnyNone as instance]: AnyNone[T] = instance

    given int: AnyNone[Int] = _ => lit(false)
    given option[T: AnyNone as inner]: AnyNone[Option[T]] = expr => expr.isEmpty || expr.exists(inner(_))
    given product[T](using instances: K0Expr.ProductInstances[AnyNone, T]): AnyNone[T] =
      expr => instances.foldLeft(expr)(lit(false))([t] => (acc, anyNone, field) => acc || anyNone(field))
  }

  type Path = Seq[String]
  object Path {
    def empty = Seq.empty
  }

  trait CountNone[T] {
    def inner: Seq[(Path, ExprFn[T, Boolean])]
    def isNone: ExprFn[T, Boolean]
    final def apply(e: Expr[T]): AggregateExpr[Seq[(Path, Long)]] = {
      val aggs = inner.map((path, isNone) => AggregateExpr.tuple(path, countIf(isNone)(e)))
      AggregateExpr.seq(aggs)
    }
  }
  object CountNone {
    def apply[T: CountNone as instance]: CountNone[T] = instance

    given int: CountNone[Int] {
      def inner = Seq.empty
      def isNone = _ => lit(false)
    }
    given option: [T: CountNone] => CountNone[Option[T]] {
      def inner = CountNone[T].inner.map((path, isNone) => (path, _.exists(isNone)))
      def isNone = _.isEmpty
    }
    given product: [T: K0Expr.ProductInstancesOf[CountNone] as inst: Labelling] => CountNone[T] {
      def isNone = _ => lit(false)
      def inner =
        inst
          .labelled
          .foldLeft(Seq.empty[(Path, ExprFn[T, Boolean])])([t] =>
            (acc, labelAndCountNone, extractor) =>
              val (label, countNone) = labelAndCountNone
              acc ++ Seq((Seq(label), extractor.andThen(countNone.isNone))) ++
                countNone.inner.map((path, isNone) => (label +: path, extractor.andThen(isNone)))
          )
    }
  }

  trait Blank[T] extends ExprFn0[T]
  object Blank {
    def apply[T: Blank as instance]: Blank[T] = instance

    given option[T: Codec]: Blank[Option[T]] = () => none
    given product[T](using instances: K0Expr.ProductInstances[Blank, T]): Blank[T] =
      () => instances.construct([t] => _())
  }

  /** Compute the maximum per field of a product type */
  trait Max[T] extends AggregateFn[T, T]
  object Max {
    def apply[T: Max as instance]: Max[T] = instance

    given int: Max[Int] = max(_)
    given long: Max[Long] = max(_)
    given product[T](using instances: K0Expr.ProductInstances[Max, T]): Max[T] =
      expr => instances.mapAggregate(expr)([t] => (max, e) => max(e))
  }

  /** Compare two values doing fields-wise less-than for product types */
  trait AllLessThan[T] extends ExprFn2[T, T, Boolean]
  object AllLessThan {
    def apply[T: AllLessThan as instance]: AllLessThan[T] = instance

    given int: AllLessThan[Int] = (a, b) => a < b
    given string: AllLessThan[String] = (a, b) => a < b
    given option[T: AllLessThan as inner]: AllLessThan[Option[T]] =
      (a, b) => (a.isEmpty && !b.isEmpty) || a.exists(aInner => b.exists(bInner => inner(aInner, bInner)))
    given product[T](using instances: K0Expr.ProductInstances[AllLessThan, T]): AllLessThan[T] =
      (e1, e2) =>
        instances.foldLeft2(e1, e2)(lit(true))([t] => (acc, instance, a, b) => acc && instance(a, b))
  }
}

class K0ExprSpec extends AnyFunSuite {
  import K0ExprSpec.{AnyNone, CountNone, Blank, AllLessThan, Max}

  def checkSameBehavior[T: Arbitrary: Codec, R](f: Expr[T] => Expr[R], reference: T => R) = {
    val fEval = ExprEvaluation.lambda(f)
    (0 to 100).foreach { _ =>
      val value = Arbitrary[T]()
      assert(fEval(value) == reference(value), s"Failed for input: $value")
    }
  }

  def checkSameBehaviorAggregate[T: Arbitrary: Codec, R](
      f: Expr[T] => AggregateExpr[R],
      reference: Seq[T] => Option[R]
  ) = {
    val fEval = AggregateExprEvaluation.lambda(f)
    (0 to 100).foreach { _ =>
      val value = Arbitrary[Seq[T]]()
      assert(fEval(value) == reference(value))
    }
  }

  test("anyNone for product type") {
    type T = (Int, Option[Int], Option[(Int, Int)])
    checkSameBehavior(AnyNone[T](_), t => t._2.isEmpty || t._3.isEmpty)
  }

  test("anyNone for nested product type") {
    type Inner = (a: Int, b: Option[Int])
    type Outer = (x: Int, y: Option[Int], z: Option[Inner])
    checkSameBehavior(AnyNone[Outer](_), t => t.y.isEmpty || t.z.isEmpty || t.z.exists(_.b.isEmpty))
  }

  test("countNone for nested product type") {
    type T = (Int, Option[Int], Option[(Int, Option[Int])])
    checkSameBehaviorAggregate(
      CountNone[T](_),
      values => {
        Option.when(!values.isEmpty)(Seq(
          Seq("_1") -> 0L,
          Seq("_2") -> values.count(_._2.isEmpty),
          Seq("_3") -> values.count(_._3.isEmpty),
          Seq("_3", "_1") -> 0,
          Seq("_3", "_2") -> values.count(_._3.exists(_._2.isEmpty))
        ))
      }
    )
  }

  test("lessThan for nested product type") {
    type Outer = (x: Int, y: Option[Int], z: Option[String])
    val ordInt: Ordering[Option[Int]] = summon
    val ordString: Ordering[Option[String]] = summon
    import ordInt.mkOrderingOps
    import ordString.mkOrderingOps
    checkSameBehavior[(Outer, Outer), Boolean](
      e => AllLessThan[Outer](e._1, e._2),
      (a, b) => a.x < b.x && a.y < b.y && a.z < b.z
    )
  }

  test("blank for product only Option") {
    type T = (Option[Int], Option[String])
    given Codec[T] = Codec.product
    val expr = Blank[T]()
    val expected = tuple(lit[Option[Int]](None), lit[Option[String]](None))
    assert(expr.node == expected.node)
  }

  test("max for nested product") {
    type Inner = (Int, Long)
    type Outer = (Int, Long, Inner)
    checkSameBehaviorAggregate(
      Max[Outer](_),
      arg =>
        Option.when(arg.nonEmpty)(
          (arg.map(_._1).max, arg.map(_._2).max, (arg.map(_._3._1).max, arg.map(_._3._2).max))
        )
    )
  }

  test("fail for coproduct") {
    assertCompileTimeError(
      """summon[K0Expr.CoproductInstances[AnyNone, K0ExprSpec.Enum]]""",
      "not supported for K0Expr"
    )
  }
}
