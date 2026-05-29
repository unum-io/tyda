package com.choreograph.tyda

import scala.compiletime.testing.typeChecks

import org.scalatest.funsuite.AnyFunSuite

object NonEmptySpec {
  // Cache instances that are used over and over again
  private given Arbitrary[Seq[Int]] = Arbitrary.iterable
  private given Arbitrary[Set[Int]] = Arbitrary.iterable
  private given Arbitrary[Map[Int, Int]] = Arbitrary.iterable
}

class NonEmptySpec extends AnyFunSuite {
  import NonEmptySpec.given

  test("constructor should not compile with empty") { assert(!typeChecks("NonEmpty[Set]()")) }

  test("constructor should support type constructor") {
    val c = NonEmpty[Seq](1, 2)
    assert(c.toSeq == Seq(1, 2))
  }

  test("constructor should support concrete type") {
    val c: NonEmpty[Set[Int]] = NonEmpty[Set](1, 2)
    assert(c.toSet == Set(1, 2))
  }

  test("from should reject empty") { assert(NonEmpty.from(Set()) == None) }

  test("from should accept non empty") { assert(NonEmpty.from(Set(1)) == Some(NonEmpty[Set](1))) }

  private def testSameBehavior[C <: Iterable[?]: TypeName: Arbitrary, R](
      opName: String,
      op: NonEmpty[C] => R,
      ref: C => R
  ) = {
    val values = Arbitrary[C].filter(!_.isEmpty)()
    val nonEmpty = NonEmpty.from(values).getOrElse(fail(s"Failed to create NonEmpty from ${values}"))
    test(s"${TypeName.name[C]}.$opName") { assert(op(nonEmpty) == ref(values)) }
  }
  private def testSameBehaviorAndPreservesNonEmpty[C <: Iterable[?]: TypeName: Arbitrary, R <: Iterable[?]](
      opName: String,
      op: NonEmpty[C] => NonEmpty[R],
      ref: C => R
  ) = testSameBehavior[C, R](opName + " preserves NonEmpty", op.andThen(identity), ref)

  testSameBehavior[Seq[Int], Int]("knownSize", _.knownSize, _.knownSize)
  testSameBehavior[IndexedSeq[Int], Int]("knownSize", _.knownSize, _.knownSize)
  testSameBehavior[Set[Int], Set[Int]]("filter", _.filter(_ > 0), _.filter(_ > 0))
  testSameBehavior[Set[Int], List[Int]]("iterator", _.iterator.toList, _.iterator.toList)
  testSameBehavior[Seq[Int], Seq[Int]](
    "flatMap",
    _.flatMap(x => Seq(x, x + 1)),
    values => values.flatMap(x => Seq(x, x + 1))
  )
  testSameBehavior[Seq[Int], Seq[(Int, Int)]]("zip", _.zip(Seq(10, 20, 30)), _.zip(Seq(10, 20, 30)))
  testSameBehavior[Seq[Int], Seq[Int]]("can be used as underlying directly", identity, identity)
  testSameBehavior[Seq[Int], Seq[Int]]("underlying", _.underlying, identity)

  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("map", _.map(_ + 1), _.map(_ + 1))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[(Int, Int)]](
    "zipWithIndex",
    _.zipWithIndex,
    _.zipWithIndex
  )
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]](
    "scanLeft",
    _.scanLeft(0)(_ + _),
    _.scanLeft(0)(_ + _)
  )
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("prepended", _.prepended(0), _.prepended(0))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("+:", 1 +: _, 1 +: _)
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("appended", _.appended(99), _.appended(99))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]](":+", _ :+ 0, _ :+ 0)
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("reverse", _.reverse, _.reverse)
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("distinct", _.distinct, _.distinct)
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]](
    "distinctBy",
    _.distinctBy(_ % 3),
    _.distinctBy(_ % 3)
  )
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("sorted", _.sorted, _.sorted)
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("sortBy", _.sortBy(-_), _.sortBy(-_))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("++", _ ++ List(1, 2, 3), _ ++ List(1, 2, 3))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[(Int, Int)]](
    "zip",
    _.zip(NonEmpty[List](10, 20, 30)),
    _.zip(NonEmpty[List](10, 20, 30))
  )

  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]](
    "concat",
    _.concat(List(4, 5)),
    _.concat(List(4, 5))
  )

  testSameBehavior[Seq[Int], Map[Int, NonEmpty[Seq[Int]]]](
    "groupBy",
    _.groupBy(_ % 2),
    _.groupBy(_ % 2)
      .view
      .mapValues(c => NonEmpty.from(c).getOrElse(fail("Expected at least one value")))
      .toMap
  )
  testSameBehavior[Seq[Int], Map[Int, NonEmpty[Seq[Int]]]](
    "groupMap",
    _.groupMap(_ % 2)(_ * 10),
    _.groupMap(_ % 2)(_ * 10)
      .view
      .mapValues(c => NonEmpty.from(c).getOrElse(fail("Expected at least one value")))
      .toMap
  )
  testSameBehavior[Seq[Int], Map[Int, Int]](
    "groupMapReduce",
    _.groupMapReduce(_ % 2)(_ * 10)(_ + _),
    _.groupMapReduce(_ % 2)(_ * 10)(_ + _)
  )

  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Seq[Int]]("to(Seq)", _.to(Seq), _.to(Seq))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], List[Int]]("to(List)", _.to(List), _.to(List))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Vector[Int]]("to(Vector)", _.to(Vector), _.to(Vector))
  testSameBehaviorAndPreservesNonEmpty[Seq[Int], Set[Int]]("to(Set)", _.to(Set), _.to(Set))
  testSameBehaviorAndPreservesNonEmpty[Seq[(Int, Int)], Map[Int, Int]]("to(Map)", _.to(Map), _.to(Map))

  testSameBehaviorAndPreservesNonEmpty[Map[Int, Int], Map[Int, Int]](
    "updated",
    _.updated(0, 1),
    _.updated(0, 1)
  )
  testSameBehaviorAndPreservesNonEmpty[Map[Int, Int], Map[Int, Int]]("+", _ + (0 -> 1), _ + (0 -> 1))
  testSameBehaviorAndPreservesNonEmpty[Map[Int, Int], Map[Int, Int]](
    "transform",
    _.transform((k, v) => v + k),
    _.transform((k, v) => v + k)
  )
  testSameBehaviorAndPreservesNonEmpty[Map[Int, Int], Map[Int, Int]](
    "++",
    _ ++ Map(0 -> 1, 3 -> 4),
    _ ++ Map(0 -> 1, 3 -> 4)
  )
  testSameBehaviorAndPreservesNonEmpty[Map[Int, Int], Set[Int]]("keySet", _.keySet, _.keySet)

  testSameBehaviorAndPreservesNonEmpty[List[Int], List[Int]]("::", 1 :: _, 1 :: _)
  testSameBehaviorAndPreservesNonEmpty[List[Seq[Int]], List[Iterable[Int]]](
    "::",
    Iterable(2) :: _,
    Iterable(2) :: _
  )
}
