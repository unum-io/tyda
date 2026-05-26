package com.choreograph.tyda

import scala.collection.mutable

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.TreeApi.Stop
import com.choreograph.tyda.TreeApi.StopOrContinue
import com.choreograph.tyda.TreeApiSpec.ToyDataset

object TreeApiSpec {
  sealed trait ToyExpr[T]
  object ToyExpr {
    final case class Literal[T](value: T) extends ToyExpr[T]
    final case class Select[U, T](value: ToyExpr[U], field: String) extends ToyExpr[T]
    final case class And(rhs: ToyExpr[Boolean], lhs: ToyExpr[Boolean]) extends ToyExpr[Boolean]
    final case class Equal[T](left: ToyExpr[T], right: ToyExpr[T]) extends ToyExpr[Boolean]
    final case class Add[T](left: ToyExpr[T], right: ToyExpr[T]) extends ToyExpr[T]
    final case class LessThan[T](ord: Ordering[T], left: ToyExpr[T], right: ToyExpr[T]) extends ToyExpr[T]
    final case class MakeArray[T](elements: Seq[ToyExpr[T]]) extends ToyExpr[Seq[T]]
    final case class Coalesce[T](elements: Seq[ToyExpr[Option[T]]]) extends ToyExpr[Option[T]]
    final case class MakeTuple[T <: Tuple](value: Tuple.Map[T, ToyExpr]) extends ToyExpr[T]
    final case class KnownNotNull[T](expr: ToyExpr[Option[T]]) extends ToyExpr[T]
    final case class Subquery[T](dataset: ToyDataset[T]) extends ToyExpr[T]

    given api[T]: TreeApi[ToyExpr[T], ToyExpr] = {
      given [T]: TreeApi[ToyExpr.Literal[T], ToyExpr] = TreeApi.leaf
      given TreeApi[String, ToyExpr] = TreeApi.leaf
      given [T]: TreeApi[Ordering[T], ToyExpr] = TreeApi.leaf
      given TreeApi[Tuple.Map[Tuple, ToyExpr], ToyExpr] = TreeApi.mappedTuple([t] => () => api)
      given [T]: TreeApi[ToyDataset[T], ToyExpr] = TreeApi.coproductContainer
      TreeApi.coproduct
    }
  }
  enum ToyDataset[T] {
    case Empty()
    case Filter(dataset: ToyDataset[T], condition: ToyExpr[T])
    case Join[T, U](left: ToyDataset[T], right: ToyDataset[U], on: ToyExpr[Boolean])
        extends ToyDataset[(T, U)]
  }
}

class TreeApiSpec extends AnyFunSuite {
  import TreeApiSpec.ToyExpr

  private val basicAddExpr =
    ToyExpr.Add(ToyExpr.Literal(1), ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)))

  test("basic transform up") {
    val visited = mutable.ArrayBuffer[ToyExpr[?]]()
    val _ = ToyExpr.api.transformUp(basicAddExpr, [t] => (v: ToyExpr[t]) => { visited += v; Continue(v) })
    val expected = Seq(
      ToyExpr.Literal(1),
      ToyExpr.Literal(2),
      ToyExpr.Literal(3),
      ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)),
      basicAddExpr
    )
    assert(visited == expected)
  }

  test("basic transform down") {
    val visited = mutable.ArrayBuffer[ToyExpr[?]]()
    val _ = ToyExpr.api.transformDown(basicAddExpr, [t] => (v: ToyExpr[t]) => { visited += v; Continue(v) })
    val expected = Seq(
      basicAddExpr,
      ToyExpr.Literal(1),
      ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)),
      ToyExpr.Literal(2),
      ToyExpr.Literal(3)
    )
    assert(visited == expected)
  }

  test("skip on transform down") {
    val visited = mutable.ArrayBuffer[ToyExpr[?]]()
    val _ = ToyExpr
      .api
      .transformDown(
        basicAddExpr,
        [t] =>
          (v: ToyExpr[t]) => {
            visited += v
            if v == ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)) then Skip(v) else Continue(v)
          }
      )
    val expected = Seq(basicAddExpr, ToyExpr.Literal(1), ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)))
    assert(visited == expected)
  }

  test("stop on transform up") {
    val visited = mutable.ArrayBuffer[ToyExpr[?]]()
    val _ = ToyExpr
      .api
      .transformUp(
        basicAddExpr,
        [t] =>
          (v: ToyExpr[t]) => {
            visited += v
            v match {
              case lit @ ToyExpr.Literal(v: Int) if v >= 2 => Stop(lit)
              case other => Continue(other)
            }
          }
      )
    val expected = Seq(ToyExpr.Literal(1), ToyExpr.Literal(2), ToyExpr.Literal(3))
    assert(visited == expected)
  }

  private val stopOnLiteral2 = [t] =>
    (v: ToyExpr[t]) =>
      v match {
        case lit @ ToyExpr.Literal(2) => Stop(lit)
        case other => Continue(other)
      }: StopOrContinue[ToyExpr[t]]

  private def countingTrasformUp[T](
      expr: ToyExpr[T],
      f: [t] => ToyExpr[t] => StopOrContinue[ToyExpr[t]]
  ): (ToyExpr[T], Int) = {
    var visited = 0
    val result = ToyExpr
      .api
      .transformUp(
        expr,
        [t] =>
          v => {
            visited += 1
            f(v)
          }
      )
    (result, visited)
  }

  private def countingTrasformDown[T](
      expr: ToyExpr[T],
      f: [t] => ToyExpr[t] => StopOrContinue[ToyExpr[t]]
  ): (ToyExpr[T], Int) = {
    var visited = 0
    val result = ToyExpr
      .api
      .transformDown(
        expr,
        [t] =>
          v => {
            visited += 1
            f(v)
          }
      )
    (result, visited)
  }

  test("stop on transform MakeTuple should work") {
    val expr =
      ToyExpr.MakeTuple[(Int, Int, Int)]((ToyExpr.Literal(1), ToyExpr.Literal(2), ToyExpr.Literal(3)))

    val (resultUp, visitedUp) = countingTrasformUp(expr, stopOnLiteral2)
    assert(resultUp == expr)
    assert(visitedUp == 3)

    val (resultDown, visitedDown) = countingTrasformDown(expr, stopOnLiteral2)
    assert(resultDown == expr)
    assert(visitedDown == 4)
  }

  test("stop on transform MakeArray should work") {
    val expr = ToyExpr.MakeArray(Seq(ToyExpr.Literal(1), ToyExpr.Literal(2), ToyExpr.Literal(3)))

    val (resultUp, visitedUp) = countingTrasformUp(expr, stopOnLiteral2)
    assert(resultUp == expr)
    assert(visitedUp == 3)

    val (resultDown, visitedDown) = countingTrasformDown(expr, stopOnLiteral2)
    assert(resultDown == expr)
    assert(visitedDown == 4)
  }

  test("stop on transform Product expr should work") {
    val expr = ToyExpr.Add(ToyExpr.Literal(1), ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)))

    val (resultUp, visitedUp) = countingTrasformUp(expr, stopOnLiteral2)
    assert(resultUp == expr)
    assert(visitedUp == 3)

    val (resultDown, visitedDown) = countingTrasformDown(expr, stopOnLiteral2)
    assert(resultDown == expr)
    assert(visitedDown == 5)
  }

  test("stop on transform Subquery should work") {
    val dataset = ToyDataset.Filter(
      ToyDataset.Empty[Boolean](),
      ToyExpr.Equal(ToyExpr.Add(ToyExpr.Literal(1), ToyExpr.Literal(2)), ToyExpr.Literal(3))
    )
    val expr = ToyExpr.Subquery(dataset)

    val (resultUp, visitedUp) = countingTrasformUp(expr, stopOnLiteral2)
    assert(resultUp == expr)
    assert(visitedUp == 3)

    val (resultDown, visitedDown) = countingTrasformDown(expr, stopOnLiteral2)
    assert(resultDown == expr)
    assert(visitedDown == 6)
  }

  val simplifyBooleanExpr = [t] =>
    (v: ToyExpr[t]) =>
      Continue(v match {
        case ToyExpr.And(lhs, ToyExpr.Literal(true)) => lhs
        case ToyExpr.And(ToyExpr.Literal(true), rhs) => rhs
        case ToyExpr.And(_, rhs @ ToyExpr.Literal(false)) => rhs
        case ToyExpr.And(lhs @ ToyExpr.Literal(false), _) => lhs
        case other => other
      }: ToyExpr[t])

  test("transform support Subquery") {
    val dataset = ToyDataset.Filter(
      ToyDataset.Empty[Boolean](),
      ToyExpr.And(ToyExpr.Literal(true), ToyExpr.Literal(false))
    )
    val expr = ToyExpr.Subquery(dataset)
    val expected = ToyExpr.Subquery(ToyDataset.Filter(ToyDataset.Empty[Boolean](), ToyExpr.Literal(false)))
    assert(expected == ToyExpr.api.transformUp(expr, simplifyBooleanExpr))
  }

  test("transform support MakeTuple") {
    val expr = ToyExpr.MakeTuple[(Int, Boolean, Int)](
      (ToyExpr.Literal(1), ToyExpr.And(ToyExpr.Literal(false), ToyExpr.Literal(true)), ToyExpr.Literal(3))
    )
    val expected =
      ToyExpr.MakeTuple[(Int, Boolean, Int)]((ToyExpr.Literal(1), ToyExpr.Literal(false), ToyExpr.Literal(3)))
    assert(expected == ToyExpr.api.transformUp(expr, simplifyBooleanExpr))
  }

  test("transform support MakeArray") {
    val expr = ToyExpr.MakeArray(Seq(
      ToyExpr.Literal(true),
      ToyExpr.And(ToyExpr.Literal(false), ToyExpr.Literal(true)),
      ToyExpr.Literal(false)
    ))
    val expected =
      ToyExpr.MakeArray(Seq(ToyExpr.Literal(true), ToyExpr.Literal(false), ToyExpr.Literal(false)))
    assert(expected == ToyExpr.api.transformUp(expr, simplifyBooleanExpr))
  }

  test("transform support skip subtree") {
    val dataset = ToyDataset.Filter(
      ToyDataset.Empty[Boolean](),
      ToyExpr.And(ToyExpr.Literal(true), ToyExpr.Literal(false))
    )
    val expr = ToyExpr.MakeArray(
      Seq(ToyExpr.Subquery(dataset), ToyExpr.And(ToyExpr.Literal(true), ToyExpr.Literal(false)))
    )
    val expected = ToyExpr.MakeArray(Seq(ToyExpr.Subquery(dataset), ToyExpr.Literal(false)))
    val simplifyWithouSubquery = [t] =>
      (v: ToyExpr[t]) =>
        v match {
          case subquery @ ToyExpr.Subquery(_) => Skip(subquery)
          case v => simplifyBooleanExpr(v)
        }
    assert(expected == ToyExpr.api.transformDown(expr, simplifyWithouSubquery))
  }

  test("basic transform accumulate up") {
    val (visited, result) = ToyExpr
      .api
      .transformAccumulateUp(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (visited, v) => { Continue(visited :+ v, v) }
      )
    val expected = Seq(
      ToyExpr.Literal(1),
      ToyExpr.Literal(2),
      ToyExpr.Literal(3),
      ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)),
      basicAddExpr
    )
    assert(visited == expected)
    assert(result == basicAddExpr)
  }

  test("stop on transform accumulate up") {
    val (visited, _) = ToyExpr
      .api
      .transformAccumulateUp(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (visited, v) => {
          if v == ToyExpr.Literal(2) then Stop(visited :+ v, v) else Continue(visited :+ v, v)
        }
      )
    val expectedVisited = Seq(ToyExpr.Literal(1), ToyExpr.Literal(2), ToyExpr.Literal(3))
    assert(visited == expectedVisited)
  }

  test("basic transform accumulate down") {
    val (visited, result) = ToyExpr
      .api
      .transformAccumulateDown(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (visited, v) => Continue(visited :+ v, v)
      )
    val expected = Seq(
      basicAddExpr,
      ToyExpr.Literal(1),
      ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)),
      ToyExpr.Literal(2),
      ToyExpr.Literal(3)
    )
    assert(visited == expected)
    assert(result == basicAddExpr)
  }

  test("skip on transform accumulate down") {
    val (visited, _) = ToyExpr
      .api
      .transformAccumulateDown(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (visited, v) => {
          if v == ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)) then Skip(visited :+ v, v)
          else Continue(visited :+ v, v)
        }
      )
    val expectedVisited =
      Seq(basicAddExpr, ToyExpr.Literal(1), ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)))
    assert(visited == expectedVisited)
  }

  test("transform up while accumulating") {
    val (adds, result) = ToyExpr
      .api
      .transformAccumulateUp(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (acc, v) =>
          v match {
            case ToyExpr.Add(left, right) => Continue(acc :+ v, ToyExpr.Add(right, left))
            case other => Continue(acc, other)
          }
      )
    // In post-order, f is called after children are transformed, so the outer Add's right child
    // is already the swapped inner Add when f records it.
    val expectedAdds = Seq(
      ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)),
      ToyExpr.Add(ToyExpr.Literal(1), ToyExpr.Add(ToyExpr.Literal(3), ToyExpr.Literal(2)))
    )
    assert(adds == expectedAdds)
    assert(result == ToyExpr.Add(ToyExpr.Add(ToyExpr.Literal(3), ToyExpr.Literal(2)), ToyExpr.Literal(1)))
  }

  test("transform down while accumulating") {
    val (adds, result) = ToyExpr
      .api
      .transformAccumulateDown(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (acc, v) =>
          v match {
            case ToyExpr.Add(left, right) => Continue(acc :+ v, ToyExpr.Add(right, left))
            case other => Continue(acc, other)
          }
      )
    // In pre-order, f is called before children are transformed, so accumulation records original nodes.
    val expectedAdds = Seq(basicAddExpr, ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)))
    assert(adds == expectedAdds)
    assert(result == ToyExpr.Add(ToyExpr.Add(ToyExpr.Literal(3), ToyExpr.Literal(2)), ToyExpr.Literal(1)))
  }

  test("stop on transform accumulate down") {
    val (visited, _) = ToyExpr
      .api
      .transformAccumulateDown(Seq.empty[ToyExpr[?]], basicAddExpr)([t] =>
        (visited, v) => {
          if v == ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)) then Stop(visited :+ v, v)
          else Continue(visited :+ v, v)
        }
      )
    val expectedVisited =
      Seq(basicAddExpr, ToyExpr.Literal(1), ToyExpr.Add(ToyExpr.Literal(2), ToyExpr.Literal(3)))
    assert(visited == expectedVisited)
  }

  def countingExists[T](expr: ToyExpr[T], f: [t] => ToyExpr[t] => Boolean): (Boolean, Int) = {
    var visited = 0
    val exists = ToyExpr
      .api
      .exists(
        expr,
        [t] =>
          v => {
            visited += 1
            f(v)
          }
      )
    (exists, visited)
  }

  test("exists") {
    val (exists, visited) = countingExists(
      basicAddExpr,
      [t] =>
        _ match {
          case ToyExpr.Literal(1) => true
          case _ => false
        }
    )
    assert(exists)
    assert(visited == 2)
  }

  def countingForall[T](expr: ToyExpr[T], f: [t] => ToyExpr[t] => Boolean): (Boolean, Int) = {
    var visited = 0
    val forall = ToyExpr
      .api
      .forall(
        expr,
        [t] =>
          v => {
            visited += 1
            f(v)
          }
      )
    (forall, visited)
  }

  test("forall") {
    val (forall, visited) = countingForall(
      basicAddExpr,
      [t] =>
        _ match {
          case ToyExpr.Add(_, _) => true
          case ToyExpr.Literal(1) => true
          case _ => false
        }
    )
    assert(!forall)
    assert(visited == 4)
  }

  test("forall true") {
    val (forall, visited) = countingForall(basicAddExpr, [t] => _ => true)
    assert(forall)
    assert(visited == 5)
  }

  test("fold support boolean And+Equal") {
    val expr = ToyExpr.And(
      ToyExpr.Equal(ToyExpr.Literal(1), ToyExpr.Literal(2)),
      ToyExpr.Equal(ToyExpr.Literal(3), ToyExpr.Literal(4))
    )
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(2) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 4)
  }

  test("fold support LessThan") {
    val expr = ToyExpr.LessThan(Ordering.Int, ToyExpr.Literal(1), ToyExpr.Literal(2))
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(1) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 2)
  }

  test("fold support MakeArray") {
    val expr = ToyExpr.MakeArray(Seq(ToyExpr.Literal(1), ToyExpr.Literal(2), ToyExpr.Literal(3)))
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(2) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 3)
  }

  test("fold support MakeTuple") {
    val expr =
      ToyExpr.MakeTuple[(Int, Int, Int)]((ToyExpr.Literal(1), ToyExpr.Literal(2), ToyExpr.Literal(3)))
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(2) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 3)
  }

  test("fold support Coalesce") {
    val expr =
      ToyExpr.Coalesce(Seq(ToyExpr.Literal(Some(1)), ToyExpr.Literal(None), ToyExpr.Literal(Some(2))))
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(None) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 3)
  }

  test("fold support KnownNotNull") {
    val expr = ToyExpr.KnownNotNull(ToyExpr.Literal(Some(1)))
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(Some(1)) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 2)
  }

  test("fold support Subquery") {
    val expr = ToyExpr.Subquery(
      ToyDataset.Filter(ToyDataset.Filter(ToyDataset.Empty(), ToyExpr.Literal(false)), ToyExpr.Literal(true))
    )
    val (exists, visited) = countingExists(
      expr,
      [t] =>
        v =>
          v match {
            case ToyExpr.Literal(true) => true
            case _ => false
          }
    )
    assert(exists)
    assert(visited == 3)
  }

  test("fold support skip Subquery") {
    val dataset = ToyDataset.Filter(
      ToyDataset.Filter(ToyDataset.Empty[Boolean](), ToyExpr.Literal(false)),
      ToyExpr.Literal(true)
    )
    val expr = ToyExpr.And(ToyExpr.Subquery(dataset), ToyExpr.Literal(true))
    val found = ToyExpr
      .api
      .fold(expr)(false)([t] =>
        (acc, value) =>
          value match {
            case ToyExpr.Subquery(_) => Skip(acc)
            case ToyExpr.Literal(false) => Stop(true)
            case _ => Continue(acc)
          }
      )
    assert(!found)
  }

  val and = ToyExpr.And(ToyExpr.Literal(false), ToyExpr.Literal(true))
  val trueLit = ToyExpr.Literal(true)

  def collectChildren[T](expr: ToyExpr[T]): Seq[ToyExpr[?]] = {
    ToyExpr.api.foldChildren(expr)(Seq.empty[ToyExpr[?]])([t] => (acc, value) => Continue(acc :+ value))
  }

  test("foldChildren support Subquery") {
    val expr = ToyExpr.Subquery(ToyDataset.Filter(ToyDataset.Filter(ToyDataset.Empty(), and), trueLit))
    assert(collectChildren(expr) == Seq(and, trueLit))
  }

  test("foldChildren support MakeTuple") {
    val expr = ToyExpr.MakeTuple[(Boolean, Boolean)](and, trueLit)
    assert(collectChildren(expr) == Seq(and, trueLit))
  }

  test("foldChildren support MakeArray") {
    val expr = ToyExpr.MakeArray(Seq(and, trueLit))
    assert(collectChildren(expr) == Seq(and, trueLit))
  }

  test("foldChildren support And") {
    val expr = ToyExpr.And(and, trueLit)
    assert(collectChildren(expr) == Seq(and, trueLit))
  }
}
