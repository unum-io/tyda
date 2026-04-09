package com.choreograph.tyda.testsuites

import java.util.regex.Pattern

import scala.NamedTuple.NamedTuple
import scala.collection.Factory
import scala.collection.immutable.IndexedSeqOps
import scala.collection.immutable.StrictOptimizedSeqOps
import scala.collection.mutable
import scala.compiletime.ops.int.-
import scala.compiletime.ops.int.S
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.reflect.Typeable
import scala.util.matching.Regex

import org.scalactic.Equality
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.CanCast
import com.choreograph.tyda.CanTryCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Comparable
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.Expr
import com.choreograph.tyda.JsonArrayOrObject
import com.choreograph.tyda.Ord
import com.choreograph.tyda.Remover
import com.choreograph.tyda.Selector
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.functions.coalesce
import com.choreograph.tyda.functions.concat
import com.choreograph.tyda.functions.daysToDate
import com.choreograph.tyda.functions.endsWith
import com.choreograph.tyda.functions.fromJson
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.functions.makeMap
import com.choreograph.tyda.functions.microsToDuration
import com.choreograph.tyda.functions.microsToTimestamp
import com.choreograph.tyda.functions.namedTuple
import com.choreograph.tyda.functions.none
import com.choreograph.tyda.functions.raiseError
import com.choreograph.tyda.functions.range
import com.choreograph.tyda.functions.seq
import com.choreograph.tyda.functions.some
import com.choreograph.tyda.functions.startsWith
import com.choreograph.tyda.functions.toJson
import com.choreograph.tyda.functions.tuple
import com.choreograph.tyda.testsuites.FloatingPointEquality.given

object ExprEvaluationSuiteBase {
  private final case class Full(a: Int, b: String, c: Boolean)
  private final case class Projected(a: Int)

  private final case class Struct(a: Int, b: String, c: Boolean) derives Arbitrary, Codec
  private type NamedTupleAlias = (a: Int, b: String, c: Boolean)
  private type NamedTupleAliasWithUnused = (a: Int, b: String, c: Boolean, unused: String)

  private enum TestEnum extends EnumStableHashCode {
    case A, B
    case C(i: Int)
    case D(i: Int)
  }

  private sealed trait TestSealedTrait
  private object TestSealedTrait {
    case object A extends TestSealedTrait
    final case class B(i: Int) extends TestSealedTrait
  }

  private enum TestEnumString extends EnumStableHashCode derives Codec.EnumAsString {
    case A, B
  }

  private type NamedTuple1 = (a: String, b: Int)
  private type NamedTuple2 = (c: String, d: Long)

  private type NestedOption[Levels <: Int, Value] = Levels match {
    case 0 => Value
    case _ => NestedOption[Levels - 1, Option[Value]]
  }

  private final case class WithOptionField(a: Option[Int]) derives Arbitrary, Codec

  private final case class WithEmptyTuple(empty: EmptyTuple) derives Arbitrary, Codec
  private final case class WithNamedTupleEmpty(empty: NamedTuple.Empty) derives Arbitrary, Codec

  // Bounded recursive type
  private final case class Leaf[T](value: T) derives Codec
  private final case class Node[T, N <: Int](value: T, children: Seq[TreeBounded[T, N]])
  private type TreeBounded[T, N <: Int] = N match {
    case 0 => Leaf[T]
    case S[n] => Node[T, n]
  }

  final case class CustomIntSeq(ints: Array[Int])
      extends IndexedSeq[Int],
        IndexedSeqOps[Int, IndexedSeq, IndexedSeq[Int]],
        StrictOptimizedSeqOps[Int, IndexedSeq, IndexedSeq[Int]],
        Serializable {
    override def iterableFactory = IndexedSeq
    override def fromSpecific(it: IterableOnce[Int]): CustomIntSeq = CustomIntSeq(it.iterator.toArray)
    override def newSpecificBuilder = IndexedSeq.newBuilder
    override def empty = CustomIntSeq(Array.empty)
    def apply(idx: Int): Int = ints(idx)
    def length: Int = ints.length
  }
  object CustomIntSeq {
    given factory: Factory[Int, CustomIntSeq] {
      def fromSpecific(it: IterableOnce[Int]): CustomIntSeq = CustomIntSeq(it.iterator.toArray)
      def newBuilder: mutable.Builder[Int, CustomIntSeq] =
        mutable.ArrayBuilder.make[Int].mapResult(arr => CustomIntSeq(arr))
    }
  }

  val errorMessage = "My error message"

  // We run into https://issues.apache.org/jira/browse/SPARK-49311
  // We might want to condider not using DayTimeInterval for Duration in Spark since they do not provide
  // any good apis for extracting the underlying value.
  given Arbitrary[Duration] =
    Arbitrary[Duration].filter(d => d.toMicros < Long.MaxValue / 10 && d.toMicros > Long.MinValue / 10)
}

/** This contains behavior tests for expression evaluation.
  *
  * Note: This also used for the SQL unparser tests so all tests cases here
  * should create deterministic Exprs. Test that are not deterministic should go
  * into ExprEvaluationSuite instead.
  */
trait ExprEvaluationSuiteBase extends AnyFunSuite {
  import ExprEvaluationSuiteBase.{
    CustomIntSeq, Full, Projected, Struct, NestedOption, NamedTuple1, NamedTuple2, NamedTupleAlias,
    NamedTupleAliasWithUnused, TestEnum, TestSealedTrait, TestEnumString, WithEmptyTuple, WithNamedTupleEmpty,
    WithOptionField, TreeBounded, Leaf, Node, errorMessage, given
  }

  /** Evaluate the expression on a sequence of inputs.
    *
    * The returned values can be in any order. It up to the caller to order the
    * values if needed.
    */
  def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To]

  def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String

  def testHasSameBehavior[From: ClassTag: Codec: Arbitrary, To: Equality](
      name: String,
      expr: Expr[From] => Expr[To],
      expected: From => To
  ): Unit

  def testFailure[From: {Arbitrary, Codec}, To](
      name: String,
      expr: Expr[From] => Expr[To],
      messages: (String | Regex)*
  ): Unit

  testHasSameBehavior[Int, Int]("identity primitive", identity, identity)
  testHasSameBehavior[(Int, Int), (Int, Int)]("identity tuple", identity, identity)
  testHasSameBehavior[(Boolean, Boolean), Boolean]("boolean and", t => t._1 && t._2, t => t._1 && t._2)
  testHasSameBehavior[(Boolean, Boolean), Boolean]("boolean or", t => t._1 || t._2, t => t._1 || t._2)
  testHasSameBehavior[(Boolean, Boolean), Boolean](
    "boolean not and equals",
    t => !t._1 == !t._2,
    t => !t._1 == !t._2
  )

  testHasSameBehavior[(Int, Int), Boolean]("equals on primitive", t => t._1 == t._2, _ == _)
  testHasSameBehavior[(Int, Int), Boolean]("not equals on primitive", t => t._1 != t._2, _ != _)
  testHasSameBehavior[(Option[Int], Option[Int]), Boolean]("equals on Option", t => t._1 == t._2, _ == _)
  testHasSameBehavior[(Option[Option[Boolean]], Option[Option[Boolean]]), Boolean](
    "equals on nested Option",
    t => t._1 == t._2,
    _ == _
  )
  testHasSameBehavior[(Seq[Boolean], Seq[Boolean]), Boolean]("equals on Seq", t => t._1 == t._2, _ == _)
  testHasSameBehavior[(WithOptionField, WithOptionField), Boolean](
    "equals on struct with optional field",
    t => t._1 == t._2,
    _ == _
  )
  testHasSameBehavior[(Seq[(Int, Int)], Seq[(Int, Int)]), Boolean](
    "equals on Seq with product",
    t => t._1 == t._2,
    _ == _
  )
  testHasSameBehavior[(Seq[WithOptionField], Seq[WithOptionField]), Boolean](
    "equals on Seq with product optional field",
    t => t._1 == t._2,
    _ == _
  )
  testHasSameBehavior[Option[Int], Boolean]("equals to None", _ == None, _ == None)

  testHasSameBehavior[Option[Option[Int]], Option[Option[Int]]]("nested Option", identity, identity)
  testHasSameBehavior[NestedOption[7, Int], NestedOption[4, Boolean]](
    "nested Option deep",
    _.map(_.map(_.map(_.map(_ == None)))),
    _.map(_.map(_.map(_.map(_ == None))))
  )
  testHasSameBehavior[Option[Option[Int]], Option[Boolean]](
    "nested Option map",
    _.map(_.isEmpty),
    _.map(_.isEmpty)
  )
  testHasSameBehavior[Option[Option[Int]], Option[Int]](
    "nested Option getOrElse",
    _.getOrElse(None: Option[Int]),
    _.getOrElse(None)
  )
  testHasSameBehavior[Option[Int], Option[Option[Int]]](
    "nested Option MakeSome",
    _.map(v => Expr.when(v > 0, v)),
    _.map(v => Option.when(v > 0)(v))
  )

  testHasSameBehavior[Int, Int]("udf", _.udf(_ * 2), _ * 2)

  testHasSameBehavior[Option[Int], Boolean]("Option.isEmpty", _.isEmpty, _.isEmpty)
  testHasSameBehavior[Option[Map[Int, Int]], Boolean]("Option[Map].isEmpty", _.isEmpty, _.isEmpty)
  testHasSameBehavior[(Option[Int], Option[Int]), Option[Int]](
    "Option.orElse",
    t => t._1.orElse(t._2),
    _.orElse(_)
  )
  testHasSameBehavior[(Option[Int], Int), Int]("Option.getOrElse", t => t._1.getOrElse(t._2), _.getOrElse(_))

  testHasSameBehavior[Option[Int], Option[Boolean]]("Option.map", _.map(_ == 0), _.map(_ == 0))
  val byte0 = 0.toByte
  testHasSameBehavior[Option[Byte], Boolean]("Option.exists", _.exists(_ == byte0), _.exists(_ == byte0))
  testHasSameBehavior[Option[Byte], Boolean]("Option.forall", _.forall(_ == byte0), _.forall(_ == byte0))
  testHasSameBehavior[Option[Byte], Boolean]("Option.contains", _.contains(byte0), _.contains(byte0))

  testHasSameBehavior[Option[Int], Option[Boolean]]("map Option", _.map(_ == 0), _.map(_ == 0))
  testHasSameBehavior[Option[Option[Int]], Option[Int]]("flatMap Option", _.flatMap(identity), _.flatten)
  testHasSameBehavior[(Option[Boolean], Option[Boolean]), Option[Boolean]](
    "Option for comprehension",
    { case Expr(opt1, opt2) =>
      for {
        v1 <- opt1
        v2 <- opt2
      } yield v1 && v2
    },
    { case (opt1, opt2) =>
      for {
        v1 <- opt1
        v2 <- opt2
      } yield v1 && v2
    }
  )
  testHasSameBehavior[(Option[Int], Option[Int]), Option[(Int, Int)]](
    "Option zip",
    { case Expr(opt1, opt2) => opt1.zip(opt2) },
    { case (opt1, opt2) => opt1.zip(opt2) }
  )
  testHasSameBehavior[(Option[Int], Int), Option[Boolean]](
    "map Option using outer",
    pair => pair._1.map(_ == pair._2),
    (a, b) => a.map(_ == b)
  )

  testHasSameBehavior[(Option[Int], Option[Int]), Option[Boolean]](
    "map Option using nested outer",
    pair => pair._1.map(v => pair._2.map(_ == v) == Some(true)),
    (a, b) => a.map(v => b.map(_ == v) == Some(true))
  )

  testHasSameBehavior[(Option[Int], Option[Int]), Option[Int]](
    "coalesce",
    t => coalesce(t._1, t._2),
    t => t._1.orElse(t._2)
  )

  testHasSameBehavior[Full, Projected]("project", _.project[Projected], f => Projected(f.a))

  testHasSameBehavior[(Full, Projected), Full](
    "copy using projection",
    { case Expr(full, proj) => full.copy(proj) },
    { case (full, proj) => full.copy(proj.a) }
  )
  testHasSameBehavior[(Full, Projected), Full](
    "copy using expr and literal",
    { case Expr(full, proj) => full.copy(a = proj.a, b = "bar") },
    { case (full, proj) => full.copy(a = proj.a, b = "bar") }
  )
  testHasSameBehavior[(Struct, NamedTupleAlias), Struct](
    "copy case class with named tuple",
    { case Expr(struct, nt) => struct.copy(nt) },
    { case ((struct, nt)) => struct.copy(nt.a, nt.b, nt.c) }
  )

  testHasSameBehavior[(Full, Full), (b: String, c: Boolean, a: Int)](
    "update replacing field with same type",
    { case Expr(full, other) => full.update((a = other.a)) },
    { case (full, other) => (b = full.b, c = full.c, a = other.a) }
  )
  testHasSameBehavior[Full, (b: String, c: Boolean, a: String)](
    "update replacing field with different type",
    full => full.update((a = full.b)),
    full => (b = full.b, c = full.c, a = full.b)
  )
  testHasSameBehavior[Full, (a: Int, b: String, c: Boolean, d: Boolean)](
    "update adding new field",
    full => full.update((d = true)),
    full => (a = full.a, b = full.b, c = full.c, d = true)
  )
  testHasSameBehavior[Full, (b: String, c: Boolean, a: String, d: Boolean)](
    "update replacing field and adding new field",
    full => full.update((a = full.b, d = full.c)),
    full => (b = full.b, c = full.c, a = full.b, d = full.c)
  )

  testHasSameBehavior[EmptyTuple, WithEmptyTuple](
    "apply to empty tuple inside struct",
    _ => Expr[WithEmptyTuple]((empty = tuple(EmptyTuple))),
    _ => WithEmptyTuple(NamedTuple.Empty)
  )

  testHasSameBehavior[EmptyTuple, WithNamedTupleEmpty](
    "apply to empty named tuple inside struct",
    _ => Expr[WithNamedTupleEmpty]((empty = namedTuple(NamedTuple.Empty))),
    _ => WithNamedTupleEmpty(NamedTuple.Empty)
  )

  testHasSameBehavior[NamedTupleAlias, Struct](
    "apply to case class with exact fields",
    Expr[Struct](_),
    nt => Struct(a = nt.a, b = nt.b, c = nt.c)
  )

  val b = "abc"
  testHasSameBehavior[NamedTupleAlias, Struct](
    "apply to case class with literal",
    nt => Expr[Struct]((a = nt.a, b = b, c = nt.c)),
    nt => Struct(a = nt.a, b = b, c = nt.c)
  )

  testHasSameBehavior[NamedTupleAlias, Struct](
    "apply to case class and auto tupling",
    nt => Expr[Struct](a = nt.a, b = nt.b, c = nt.c),
    nt => Struct(a = nt.a, b = nt.b, c = nt.c)
  )

  testHasSameBehavior[NamedTupleAliasWithUnused, Struct](
    "apply to case class with transformation",
    nt => Expr[Struct]((a = nt.a, b = nt.b, c = nt.c)),
    nt => Struct(a = nt.a, b = nt.b, c = nt.c)
  )

  testHasSameBehavior[NamedTupleAliasWithUnused, NamedTupleAlias](
    "apply to named tuple",
    nt => Expr((a = nt.a, b = nt.b, c = nt.c)),
    nt => (a = nt.a, b = nt.b, c = nt.c)
  )

  {
    val maxSize = 100
    given Arbitrary[(Int, Int)] =
      for {
        start <- Arbitrary.between(Int.MinValue + maxSize, Int.MaxValue - maxSize)
        size <- Arbitrary.between(-maxSize, maxSize)
      } yield (start, start + size)
    testHasSameBehavior[(Int, Int), Seq[Int]](
      "make range",
      { case Expr(start, end) => range(start, end) },
      (start, end) => (start until end).toSeq
    )
  }
  testHasSameBehavior[Int, Seq[Int]]("make empty seq", _ => seq(), _ => Seq())
  testHasSameBehavior[(Int, Int), Seq[Int]]("make seq", t => seq(t._1, t._2), t => Seq(t._1, t._2))
  testHasSameBehavior[(Seq[Int], Seq[Int]), Seq[Int]]("concat seq", t => t._1 ++ t._2, t => t._1 ++ t._2)
  testHasSameBehavior[List[Int], Seq[Int]]("seq toSeq", _.toSeq, _.toSeq)
  testHasSameBehavior[Seq[Int], Seq[Int]]("seq map", _.map(_ / 2), _.map(_ / 2))
  testHasSameBehavior[Seq[Seq[Int]], Seq[Seq[Int]]](
    "seq map nested",
    _.map(_.map(_ / 2)),
    _.map(_.map(_ / 2))
  )
  testHasSameBehavior[Option[Seq[Int]], Seq[Seq[Int]]](
    "nested seq inside option map",
    _.map(seq(_)).getOrElse(Seq.empty),
    _.map(Seq(_)).getOrElse(Seq.empty)
  )
  testHasSameBehavior[List[Int], List[Int]]("seq map list", _.map(_ / 2), _.map(_ / 2))
  testHasSameBehavior[CustomIntSeq, IndexedSeq[Int]]("seq map custom collection", _.map(_ / 2), _.map(_ / 2))
  testHasSameBehavior[Int, Seq[Int]](
    "seq map constructed",
    seq(_, lit(2), lit(3)).map(_ / 2),
    Seq(_, 2, 3).map(_ / 2)
  )
  testHasSameBehavior[(Seq[Int], Int), Seq[Boolean]](
    "seq map outer",
    { case Expr(seq, i) => seq.map(_ > i) },
    (seq, i) => seq.map(_ > i)
  )
  testHasSameBehavior[Seq[(Int, Int)], Seq[Int]]("seq map from product", _.map(_._1), _.map(_._1))
  testHasSameBehavior[Seq[Int], Seq[(Int, Int)]]("seq map to product", _.map((_, 1)), _.map((_, 1)))
  testHasSameBehavior[Seq[(Seq[Int], Seq[Int])], Seq[(a: Seq[Boolean])]](
    "seq map nested with",
    _.map(t => (a = t._1.map(_ > 1))),
    _.map(t => (a = t._1.map(_ > 1)))
  )
  testHasSameBehavior[Seq[(Seq[Int], Int)], Seq[(a: Seq[Boolean])]](
    "seq map nested outer",
    _.map(t => (a = t._1.map(_ > t._2))),
    _.map(t => (a = t._1.map(_ > t._2)))
  )
  testHasSameBehavior[Seq[Int], Boolean]("seq exists", _.exists(_ < 0), _.exists(_ < 0))
  testHasSameBehavior[Seq[Boolean], Boolean]("seq contains", _.contains(true), _.contains(elem = true))
  testHasSameBehavior[Seq[Int], Boolean]("seq forall", _.forall(_ < 0), _.forall(_ < 0))
  testHasSameBehavior[Seq[Seq[Int]], Seq[Boolean]](
    "seq forall nested",
    _.map(_.forall(_ < 0)),
    _.map(_.forall(_ < 0))
  )
  testHasSameBehavior[(Seq[Int], Int), Boolean](
    "seq exists outer",
    { case Expr(seq, i) => seq.exists(_ < i) },
    (seq, i) => seq.exists(_ < i)
  )
  testHasSameBehavior[(Seq[Int], Int), Boolean](
    "seq forall outer",
    { case Expr(seq, i) => seq.forall(_ < i) },
    (seq, i) => seq.forall(_ < i)
  )
  testHasSameBehavior[Seq[Int], Seq[Int]]("seq filter", _.filter(_ > 0), _.filter(_ > 0))
  testHasSameBehavior[List[Int], List[Int]]("seq filter list", _.filter(_ > 0), _.filter(_ > 0))
  testHasSameBehavior[(Seq[Int], Int), Seq[Int]](
    "seq filter outer",
    { case Expr(seq, i) => seq.filter(_ > i) },
    (seq, i) => seq.filter(_ > i)
  )
  testHasSameBehavior[(Seq[Int], Seq[Int]), Seq[(Int, Int)]](
    "seq zip",
    t => t._1.zip(t._2),
    (a, b) => a.zip(b)
  )

  testHasSameBehavior[(Int, String), Map[Int, String]]("make empty map", _ => makeMap(), _ => Map.empty)

  testHasSameBehavior[(Int, String), Map[Int, String]](
    "make map",
    t => makeMap((t._1, t._2)),
    t => Map(t._1 -> t._2)
  )

  testHasSameBehavior[(String, String, String), Map[Int, String]](
    "toMap",
    t => seq(tuple(1, t._1), tuple(2, t._2), tuple(3, t._3)).toMap,
    t => Map(1 -> t._1, 2 -> t._2, 3 -> t._3)
  )

  testHasSameBehavior[Seq[Int], Map[Int, Int]](
    "toMap with many",
    t => t.map(v => (v, v)).distinct.toMap,
    t => t.map(v => (v, v)).distinct.toMap
  )

  testHasSameBehavior[Map[Int, String], Seq[(key: Int, value: String)]](
    "mapEntries",
    _.entries,
    _.toSeq.map { case (k, v) => (key = k, value = v) }
  )
  testHasSameBehavior[Seq[Map[Int, String]], Seq[Seq[(key: Int, value: String)]]](
    "mapEntries inside seq",
    _.map(_.entries),
    _.map(_.toSeq.map { case (k, v) => (key = k, value = v) })
  )

  testHasSameBehavior[(Map[Int, String], Int), Option[String]](
    "mapGet existing key",
    t => t._1.get(t._2),
    t => t._1.get(t._2)
  )

  testHasSameBehavior[Option[Int], (Option[Option[Int]], Option[Option[Int]])](
    "mapGet Option value",
    t => {
      val map = makeMap((lit(1), t))
      tuple(map.get(lit(1)), map.get(lit(2)))
    },
    t => (Some(t), None)
  )

  testHasSameBehavior[(Map[Int, String], Int), Option[String]](
    "mapGet missing key",
    t => makeMap((lit(1), lit("x"))).get(t._2),
    t => Map(1 -> "x").get(t._2)
  )

  testHasSameBehavior[(Int, String), Option[String]](
    "mapGet with literal key",
    t => makeMap((t._1, t._2)).get(t._1),
    t => Map(t._1 -> t._2).get(t._1)
  )

  testHasSameBehavior[(Int, String), Map[Int, String]](
    "make map with multiple entries",
    _ => makeMap((lit(1), lit("a")), (lit(2), lit("b"))),
    _ => Map(1 -> "a", 2 -> "b")
  )

  testHasSameBehavior[(Int, String), Map[Int, String]](
    "make map with a mix of literal and non-literal entries",
    t => makeMap((t._1, lit("a")), (lit(2), lit("b"))),
    t => Map(t._1 -> "a", 2 -> "b")
  )

  testFailure[(Int, String), Map[Int, String]](
    "make map fails on duplicate keys",
    t => makeMap((t._1, t._2), (t._1, lit("duplicate"))),
    "[Dd]uplicate".r
  )

  testFailure[String, Map[Int, String]](
    "make map fails on literal duplicate keys",
    t => makeMap((lit(1), t), (lit(1), lit("duplicate"))),
    "[Dd]uplicate".r
  )

  def testComparisons[T: Arbitrary: Codec: TypeName: Comparable: Ord]: Unit = {
    val tpeName = TypeName.name[T]
    testHasSameBehavior[(T, T), Boolean](s"gt for $tpeName", p => p._1 > p._2, Ord[T].gt(_, _))
    testHasSameBehavior[(T, T), Boolean](s"gteq for $tpeName", p => p._1 >= p._2, Ord[T].gteq(_, _))
    testHasSameBehavior[(T, T), Boolean](s"lt for $tpeName", p => p._1 < p._2, Ord[T].lt(_, _))
    testHasSameBehavior[(T, T), Boolean](s"lteq for $tpeName", p => p._1 <= p._2, Ord[T].lteq(_, _))
  }

  testComparisons[Byte]
  testComparisons[Int]
  testComparisons[Long]
  testComparisons[Float]
  testComparisons[Double]

  testHasSameBehavior[Float, Boolean]("isNaN for Float", _.isNaN, _.isNaN)
  testHasSameBehavior[Double, Boolean]("isNaN for Double", _.isNaN, _.isNaN)
  testComparisons[String]
  testComparisons[Decimal[20, 10]]
  testComparisons[Date]
  testComparisons[Duration]
  testComparisons[Timestamp]

  testHasSameBehavior[EmptyTuple, EmptyTuple]("make empty tuple", _ => tuple(EmptyTuple), _ => EmptyTuple)

  testHasSameBehavior[(Int, Boolean), (Int, Boolean)](
    "make tuple 2",
    t => tuple(t._1, t._2),
    t => (t._1, t._2)
  )
  testHasSameBehavior[(Int, Boolean, String), (Int, Boolean, String)](
    "make tuple 3",
    t => tuple(t._1, t._2, t._3),
    t => (t._1, t._2, t._3)
  )
  testHasSameBehavior[Int, (Int, String)](
    "make tuple with literal",
    t => tuple(t, "literal"),
    t => (t, "literal")
  )

  testHasSameBehavior[(Int, Boolean), Seq[(Int, Boolean)]](
    "make tuple in seq",
    t => seq(tuple(t._1, t._2)),
    t => Seq((t._1, t._2))
  )

  testHasSameBehavior[(Int, Boolean), NamedTuple.Empty](
    "make empty namedTuple",
    _ => namedTuple(NamedTuple.Empty),
    _ => NamedTuple.Empty
  )

  testHasSameBehavior[(Int, Boolean), (one: Int, two: Boolean)](
    "make namedTuple 2",
    t => namedTuple(one = t._1, two = t._2),
    t => (one = t._1, two = t._2)
  )

  testHasSameBehavior[(Int, Boolean, String), (int: Int, boolean: Boolean, string: String)](
    "make namedTuple 3",
    t => namedTuple(int = t._1, boolean = t._2, string = t._3),
    t => (int = t._1, boolean = t._2, string = t._3)
  )
  testHasSameBehavior[Int, (int: Int, string: String)](
    "make namedTuple with literal",
    t => namedTuple(int = t, string = "literal"),
    t => (int = t, string = "literal")
  )
  testHasSameBehavior[(Int, Boolean), Seq[(int: Int, boolean: Boolean)]](
    "make namedTuple in seq",
    t => seq(namedTuple(int = t._1, boolean = t._2)),
    t => Seq((int = t._1, boolean = t._2))
  )

  testHasSameBehavior[Struct, NamedTuple.From[Struct]](
    "make namedTuple from product",
    _.toNamedTuple,
    t => (a = t.a, b = t.b, c = t.c)
  )

  protected def expectedSplit(str: String, delimiter: String) = str.split(Pattern.quote(delimiter), -1).toSeq

  protected def expectedTrim(str: String): String = {
    val first = str.indexWhere(_ != ' ')
    if first < 0 then ""
    else {
      val last = str.lastIndexWhere(_ != ' ')
      str.substring(first, last + 1)
    }
  }

  testHasSameBehavior[(String, String, String), String](
    "concat strings",
    t => concat(t._1, t._2, t._3),
    t => t._1 + t._2 + t._3
  )
  testHasSameBehavior[(String, String), Boolean](
    "endsWith string",
    t => t._1.endsWith(t._2),
    t => t._1.endsWith(t._2)
  )
  testHasSameBehavior[String, String]("trim string", _.trim(), expectedTrim)

  testHasSameBehavior[(String, String), Seq[String]](
    "split string by delimiter",
    v => v._1.split(v._2),
    v => expectedSplit(v._1, v._2)
  )

  testHasSameBehavior[(String, String), Boolean](
    "string startsWith",
    t => t._1.startsWith(t._2),
    t => t._1.startsWith(t._2)
  )

  testHasSameBehavior[(String, String), Boolean](
    "string startsWith literal",
    t => t._1.startsWith("foo"),
    t => t._1.startsWith("foo")
  )

  testHasSameBehavior[Seq[Int], Int]("size on Seq[Int]", _.size, _.size)
  testHasSameBehavior[List[String], Int]("size on List[String]", _.size, _.size)

  testHasSameBehavior[Seq[Int], Seq[Int]]("distinct on Seq[Int]", _.distinct, _.distinct)

  {
    given Arbitrary[(Seq[Int], Int)] =
      for {
        seq <- Arbitrary[Seq[Int]].filter(!_.isEmpty)
        idx <- Arbitrary.between(0, seq.size)
      } yield (seq, idx)
    testHasSameBehavior[(Seq[Int], Int), Int](
      "array element access with Expr index",
      t => t._1.get(t._2),
      t => t._1(t._2)
    )
  }

  {
    given Arbitrary[Seq[String]] = Arbitrary[Seq[String]].filter(!_.isEmpty)
    testHasSameBehavior[Seq[String], String]("array element access with literal index", _.get(0), _(0))
  }
  {
    given Arbitrary[Seq[Seq[Int]]] = Arbitrary[Seq[Seq[Int]]].filter(!_.isEmpty)
    testHasSameBehavior[Seq[Seq[Int]], Seq[Int]](
      "array element access with literal index and array element",
      _.get(0),
      _(0)
    )
  }

  testFailure[Int, Int](
    "array element access fails on negative index",
    _ => lit(Seq(1, 2, 3)).get(-1),
    "(Negative array index)|(index)|(-1)".r
  )

  testFailure[Int, Int](
    "array element access fails on index too large",
    _ => lit(Seq(1, 2, 3)).get(100),
    "(out of bounds)|(index)|(Invalid)|(100)".r
  )

  testHasSameBehavior[Int, Boolean](
    "raise error is lazy in &&",
    _ => lit(false) && raiseError[Boolean]("error"),
    _ => false
  )
  testHasSameBehavior[Int, Boolean](
    "raise error is lazy in ||",
    _ => lit(true) || raiseError[Boolean]("error"),
    _ => true
  )
  testHasSameBehavior[Int, Option[Int]](
    "raise error is lazy in coalesce",
    p => coalesce(some(p), raiseError("error")),
    Some(_)
  )
  testHasSameBehavior[Int, Int](
    "raise error is lazy in getOrElse",
    p => some(p).getOrElse(raiseError[Int]("error")),
    identity
  )

  testHasSameBehavior[Int, Option[Int]](
    "raise error is lazy in orElse",
    p => some(p).orElse(raiseError[Option[Int]]("error")),
    Some(_)
  )

  testHasSameBehavior[(Int, Int), Int]("min", t => Expr.min(t._1, t._2), (a, b) => Math.min(a, b))

  testHasSameBehavior[(Int, Int), Int]("max", t => Expr.max(t._1, t._2), (a, b) => Math.max(a, b))

  testHasSameBehavior[(Boolean, Option[Int], Option[Int]), Option[Int]](
    "ternary with Option branches",
    t => Expr.ternary(t._1, t._2, t._3),
    (cond, ifTrue, ifFalse) => if (cond) ifTrue else ifFalse
  )

  testHasSameBehavior[Int, Int](
    "raise error is lazy in ternary ifTrue",
    t => Expr.ternary(lit(false), raiseError[Int]("error"), t),
    t => t
  )

  testHasSameBehavior[Int, Int](
    "raise error is lazy in ternary ifFalse",
    t => Expr.ternary(lit(true), t, raiseError[Int]("error")),
    t => t
  )

  testHasSameBehavior[EmptyTuple, Option[Int]](
    "raise error is lazy in when",
    _ => Expr.when(lit(false), raiseError[Int]("error")),
    _ => None
  )

  testFailure[String, Int](
    "raiseError throws static error message",
    _ => raiseError[Int](errorMessage),
    errorMessage
  )

  testFailure[String, Option[String]](
    "raise error raises in coalesce after None",
    _ => coalesce(none, raiseError(errorMessage)),
    errorMessage
  )

  testFailure[String, Option[String]](
    "raise error raises in coalesce before some",
    p => coalesce(raiseError(errorMessage), some(p)),
    errorMessage
  )

  testHasSameBehavior[Int, Int](
    "expect does not raise error when Some",
    p => some(p).expect("should not happen"),
    identity
  )
  testFailure[Int, Int]("raise error in expect when None", _ => none[Int].expect("oh no!"), "oh no!")

  testHasSameBehavior[(Boolean, Int, Int), Int](
    "ternary",
    t => Expr.ternary(t._1, t._2, t._3),
    t => if t._1 then t._2 else t._3
  )

  testHasSameBehavior[(Boolean, Int), Option[Int]](
    "when",
    t => Expr.when(t._1, t._2),
    t => Option.when(t._1)(t._2)
  )

  {
    given Arbitrary[Long] = Arbitrary.int.map(_.toLong)
    testHasSameBehavior[(Long, Long), Long]("add two numbers", t => t._1 + t._2, (lhs, rhs) => lhs + rhs)
  }

  {
    given Arbitrary[Int] = Arbitrary.short.map(_.toInt)
    testHasSameBehavior[(Int, Int), Int]("add two int numbers", t => t._1 + t._2, (lhs, rhs) => lhs + rhs)
  }

  {
    given Arbitrary[Int] = Arbitrary.int.filter(_ > 0)
    testFailure[Int, Int]("fail on int overflow", _ + Expr.lit(Int.MaxValue), "(overflow)|(out of range)".r)
  }

  {
    given Arbitrary[Long] = Arbitrary.long.filter(_ != 0)
    testHasSameBehavior[(Long, Long), Long](
      "truncating divide first argument by second",
      t => t._1 / t._2,
      (lhs, rhs) => lhs / rhs
    )
  }

  {
    given Arbitrary[Int] = Arbitrary.int.filter(_ != 0)
    testHasSameBehavior[(Int, Int), Int](
      "truncating divide first int argument by second",
      t => t._1 / t._2,
      (lhs, rhs) => lhs / rhs
    )
  }

  testFailure[Int, Int]("fail on division by zero", _ / lit(0), "by zero")

  testHasSameBehavior[TestEnum.A.type, TestEnum.A.type]("identity enum singleton", identity, identity)
  testHasSameBehavior[TestSealedTrait.A.type, TestSealedTrait.A.type](
    "identity sealed trait singleton",
    identity,
    identity
  )
  testHasSameBehavior[TestEnumString.A.type, TestEnumString.A.type](
    "identity string enum singleton",
    identity,
    identity
  )

  private def testAsBase[From: ClassTag: Codec: Arbitrary: Mirror.ProductOf, To >: From: Codec: Mirror.SumOf](
      name: String
  ) = testHasSameBehavior[From, To](name, _.asBase[To], identity)

  testAsBase[TestEnum.A.type, TestEnum]("asBase from enum case object A to enum")
  testAsBase[TestEnum.B.type, TestEnum]("asBase from enum case object B to enum")
  testAsBase[TestEnum.C, TestEnum]("asBase from enum case class C to enum")
  testAsBase[TestEnum.D, TestEnum]("asBase from enum case class D to enum")

  testHasSameBehavior[(TestEnum.C, Int), TestEnum]("asBase complex", t => t._1.asBase[TestEnum], (e, _) => e)

  testAsBase[TestEnumString.A.type, TestEnumString]("asBase from string enum case object A to enum")
  testAsBase[TestEnumString.B.type, TestEnumString]("asBase from string enum case class B to enum")

  testHasSameBehavior[(TestEnumString.A.type, Int), TestEnumString](
    "asBase from string complex",
    t => t._1.asBase[TestEnumString],
    (e, _) => e
  )

  private def testAsCase[Base: ClassTag: Mirror.SumOf: Codec: Arbitrary, Case <: Base: Codec: Typeable](
      name: String
  ) =
    testHasSameBehavior[Base, Option[Case]](
      name,
      _.asCase[Case],
      {
        case v: Case => Some(v)
        case _ => None
      }
    )

  testAsCase[TestEnum, TestEnum.A.type]("asCase from enum to singleton case")
  testAsCase[TestEnum, TestEnum.C]("asCase from enum to product case")
  testAsCase[TestSealedTrait, TestSealedTrait.A.type]("asCase from sealed trait to singleton case")
  testAsCase[TestSealedTrait, TestSealedTrait.B]("asCase from sealed trait to product case")
  testAsCase[TestEnumString, TestEnumString.A.type]("asCase from string enum to singleton case")

  testHasSameBehavior[TestEnum, Option[Int]](
    "asCase: select from product case",
    _.asCase[TestEnum.C].map(_.i),
    {
      case TestEnum.C(i) => Some(i)
      case _ => None
    }
  )

  testHasSameBehavior[Option[TestEnum], Option[Option[TestEnum.A.type]]](
    "asCase: in option map",
    _.map(_.asCase[TestEnum.A.type]),
    _.map {
      case TestEnum.A => Some(TestEnum.A)
      case _ => None
    }
  )

  private def testAsBaseAsCaseRoundtrip[
      Base: ClassTag: Codec: Arbitrary: Mirror.SumOf,
      Case <: Base: ClassTag: Codec: Arbitrary: Mirror.ProductOf: Typeable
  ](name: String) =
    testHasSameBehavior[Case, Case](
      s"asBase inverts asCase for $name case",
      _.asBase[Base].asCase[Case].expect("Failed to invert asBase"),
      identity
    )
    testHasSameBehavior[Base, Option[Base]](
      s"asCase inverts asBase for $name case",
      _.asCase[Case].map(_.asBase[Base]),
      base =>
        base match {
          case _: Case => Some(base)
          case _ => None
        }
    )
  testAsBaseAsCaseRoundtrip[TestEnum, TestEnum.A.type]("enum singleton")
  testAsBaseAsCaseRoundtrip[TestEnum, TestEnum.C]("enum product")
  testAsBaseAsCaseRoundtrip[TestEnumString, TestEnumString.A.type]("string enum singleton")
  testAsBaseAsCaseRoundtrip[TestSealedTrait, TestSealedTrait.A.type]("sealed trait singleton")
  testAsBaseAsCaseRoundtrip[TestSealedTrait, TestSealedTrait.B]("sealed trait product")

  testHasSameBehavior[TestEnum, Int](
    "match-case only when cases",
    _.cases[Int]
      .when[TestEnum.A.type](_ => lit(1))
      .when[TestEnum.B.type](_ => lit(2))
      .when[TestEnum.C](e => e.i)
      .when[TestEnum.D](e => e.i),
    {
      case TestEnum.A => 1
      case TestEnum.B => 2
      case TestEnum.C(i) => i
      case TestEnum.D(i) => i
    }
  )
  testHasSameBehavior[TestEnum, Int](
    "match-case when cases + otherwise",
    _.cases[Int].when[TestEnum.A.type](_ => lit(1)).when[TestEnum.C](e => e.i).otherwise(lit(99)),
    {
      case TestEnum.A => 1
      case TestEnum.C(i) => i
      case _ => 99
    }
  )
  testHasSameBehavior[TestEnum, Int]("match-case only otherwise", _.cases[Int].otherwise(lit(99)), _ => 99)
  testHasSameBehavior[TestSealedTrait, Int](
    "match-case all cases sealed trait",
    _.cases[Int].when[TestSealedTrait.A.type](_ => lit(1)).when[TestSealedTrait.B](e => e.i),
    {
      case TestSealedTrait.A => 1
      case TestSealedTrait.B(i) => i
    }
  )
  testHasSameBehavior[TestEnumString, String](
    "match-case all cases string enum",
    _.cases[String].when[TestEnumString.A.type](_ => lit("A")).when[TestEnumString.B.type](_ => lit("B")),
    {
      case TestEnumString.A => "A"
      case TestEnumString.B => "B"
    }
  )

  testHasSameBehavior[(NamedTuple1, NamedTuple2), NamedTuple.Concat[NamedTuple1, NamedTuple2]](
    "named tuple concat",
    t => t._1 ++ t._2,
    _ ++ _
  )

  testHasSameBehavior[NamedTuple1, NamedTuple.Concat[NamedTuple1, NamedTuple2]](
    "named tuple concat literal",
    _ ++ (c = "abc", d = 2L),
    _ ++ (c = "abc", d = 2L)
  )

  testHasSameBehavior[Int, (select: Int, group: Int, by: Int)](
    "support field names that are SQL keywords",
    _ => Expr(select = 1, group = 2, by = 3),
    _ => (1, 2, 3)
  )

  testHasSameBehavior[Boolean, Boolean](
    "select from literal product",
    _ == lit((a = true, b = false)).a,
    _ == true
  )

  val specialStrings = Seq("\b", "\f", "\n", "\r", "\t", "\u000B", "'", "\\", "\"", "`", "${var}")
  testHasSameBehavior[Int, Seq[String]](
    "handle special strings",
    _ => seq(specialStrings.map(lit)),
    _ => specialStrings
  )

  private def testCast[
      From: ClassTag: Codec: Arbitrary: TypeName,
      To: CanCast.From[From]: TypeName: Equality
  ](expected: From => To) =
    testHasSameBehavior[From, To](
      s"cast ${TypeName.name[From]} to ${TypeName.name[To]}",
      _.cast[To],
      expected
    )

  private def testTryCast[From: ClassTag: Codec: Arbitrary: TypeName, To: CanTryCast.From[From]: TypeName](
      expected: From => Option[To]
  ) =
    testHasSameBehavior[From, Option[To]](
      s"try cast ${TypeName.name[From]} to ${TypeName.name[To]}",
      _.tryCast[To],
      expected
    )

  testCast[Byte, Short](_.toShort)
  testCast[Byte, Int](_.toInt)
  testCast[Byte, Long](_.toLong)
  testCast[Byte, Float](_.toFloat)
  testCast[Byte, Double](_.toDouble)

  testCast[Short, Int](_.toInt)
  testCast[Short, Long](_.toLong)
  testCast[Short, Float](_.toFloat)
  testCast[Short, Double](_.toDouble)

  testCast[Int, Long](_.toLong)
  testCast[Int, Float](_.toFloat)
  testCast[Int, Double](_.toDouble)

  testCast[Long, Float](_.toFloat)
  testCast[Long, Double](_.toDouble)

  testCast[Byte, String](_.toString)
  testCast[Short, String](_.toString)
  testCast[Int, String](_.toString)
  testCast[Long, String](_.toString)

  testCast[Float, Double](_.toDouble)
  testCast[Double, Float](_.toFloat)

  testTryCast[Long, Byte](_.toString.toByteOption)
  testTryCast[Long, Short](_.toString.toShortOption)
  testTryCast[Long, Int](_.toString.toIntOption)

  testTryCast[Int, Byte](_.toString.toByteOption)
  testTryCast[Int, Short](_.toString.toShortOption)

  testTryCast[Short, Byte](_.toString.toByteOption)

  testCast[Decimal[28, 2], Float](_.toFloat)
  testCast[Decimal[35, 2], Double](_.toDouble)
  testCast[Decimal[38, 30], Float](_.toFloat)
  testCast[Decimal[38, 30], Double](_.toDouble)

  /* The default parser allows unicode chars as digits, but expected query engine behavior is only to accept
   * ascii digits and signs. */
  def checkedFromString[T: Numeric as numeric]: String => Option[T] =
    s =>
      s.trim match {
        case trimmed if trimmed.forall(c => (c >= '0' && c <= '9') || c == '-' || c == '+') =>
          numeric.parseString(trimmed)
        case _ => None
      }

  testTryCast[String, Byte](checkedFromString)
  testTryCast[String, Short](checkedFromString)
  testTryCast[String, Int](checkedFromString)
  testTryCast[String, Long](checkedFromString)

  def testCastToDecimalAndBack[F <: Float | Double: TypeName: ClassTag: Codec: Arbitrary, D: TypeName](using
      CanTryCast[F, D],
      CanCast[D, F]
  )(make: F => Option[D], back: D => F) =
    testHasSameBehavior[F, Option[F]](
      s"cast ${TypeName.name[F]} to ${TypeName.name[D]} and back",
      v => v.tryCast[D].map(_.cast[F]),
      v => make(v).map(back)
    )
  // We have been unable to implement consitent Float/Double to Decimal for different backends.
  // But we do promise that casting back to the floating point type works correctly.
  {
    // This test does not always work for Spark due to spark doing double rounding when first using the
    // string representation (which is rounded) and then rounding that to the desired number of decimal
    // places.
    // Until a proper fix is implemented we only test some exact values to avoid flakiness.
    given Arbitrary[Double] =
      Arbitrary.oneOf(
        0.11646182983629394, 0.6191807159733975, 0.7614390962486298, 0.1975704839901261, 0.8059014990503447,
        0.21775409201523077, 0.16134244357565863, 0.47207858309534234, 0.9097204864050706, 0.8627117098475925,
        0.6126475141443474
      )
    given Arbitrary[Float] = Arbitrary[Double].map(_.toFloat)

    testCastToDecimalAndBack[Float, Decimal[2, 0]](Decimal[2, 0](_), _.toFloat)
    testCastToDecimalAndBack[Float, Decimal[38, 9]](Decimal[38, 9](_), _.toFloat)
    testCastToDecimalAndBack[Float, Decimal[38, 18]](Decimal[38, 18](_), _.toFloat)
    testCastToDecimalAndBack[Double, Decimal[3, 1]](Decimal[3, 1](_), _.toDouble)
    testCastToDecimalAndBack[Double, Decimal[38, 9]](Decimal[38, 9](_), _.toDouble)
    testCastToDecimalAndBack[Double, Decimal[38, 18]](Decimal[38, 18](_), _.toDouble)
  }

  testCast[Decimal[10, 2], Decimal[11, 3]](_.widen)
  testCast[Decimal[10, 0], Decimal[20, 5]](_.widen)

  testCast[Byte, Decimal[10, 2]](Decimal[10, 2](_))
  testCast[Short, Decimal[10, 2]](Decimal[10, 2](_))
  testCast[Int, Decimal[12, 2]](Decimal[12, 2](_))
  testCast[Long, Decimal[22, 2]](Decimal[22, 2](_))

  testCast[Seq[Int], Seq[Long]](_.map(_.toLong))
  testCast[Seq[Byte], Seq[Double]](_.map(_.toDouble))
  testCast[Seq[Float], Seq[Double]](_.map(_.toDouble))
  testCast[Seq[Seq[Int]], Seq[Seq[Long]]](_.map(_.map(_.toLong)))

  testTryCast[Decimal[19, 3], Decimal[38, 9]](v => Decimal[38, 9](v.toBigDecimal))
  testTryCast[Decimal[19, 1], Decimal[19, 0]](v => Decimal[19, 0](v.toBigDecimal))
  testTryCast[Decimal[19, 3], Decimal[10, 3]](v => Decimal[10, 3](v.toBigDecimal))

  def testLiteralCreation[T: Codec: TypeName](fixed: T) =
    testHasSameBehavior[EmptyTuple, T](
      s"create literal ${fixed.toString} ${TypeName.name[T]}",
      _ => lit(fixed),
      _ => fixed
    )

  testLiteralCreation[Int](0)
  testLiteralCreation[Long](0)
  testLiteralCreation[Short](0)
  testLiteralCreation[Byte](0)
  testLiteralCreation[Float](0)
  testLiteralCreation[Double](0)
  testLiteralCreation[String]("")
  testLiteralCreation[Boolean](fixed = false)
  testLiteralCreation[Decimal[13, 2]](Decimal[13, 2](20))
  testLiteralCreation[Option[Int]](None)
  testLiteralCreation[Option[Int]](Some(0))
  testLiteralCreation[Option[Option[Option[Int]]]](None)
  testLiteralCreation[Option[Option[Option[Int]]]](Some(None))
  testLiteralCreation[Option[Option[Option[Int]]]](Some(Some(None)))
  testLiteralCreation[Option[Option[Option[Int]]]](Some(Some(Some(0))))
  testLiteralCreation[Seq[Int]](Seq(1, 2))
  testLiteralCreation[Seq[Option[Int]]](Seq(Some(1), None, Some(2)))
  testLiteralCreation[Seq[Seq[Int]]](Seq(Seq(), Seq(1, 2)))
  testLiteralCreation[Seq[Option[Seq[Int]]]](Seq(Some(Seq()), Some(Seq(1, 2)), None))
  testLiteralCreation[Option[Option[Seq[Int]]]](Some(Some(Seq(1, 2))))
  testLiteralCreation[Option[Seq[Option[Seq[Int]]]]](Some(Seq(Some(Seq(1, 2)), None, Some(Seq()))))
  testLiteralCreation[List[String]](List("a", "b"))
  testLiteralCreation[List[Int]](List())
  testLiteralCreation[Map[String, Int]](Map("a" -> 1, "b" -> 2))
  testLiteralCreation[Map[String, String]](Map())
  testLiteralCreation[Option[Map[String, Int]]](None)
  testLiteralCreation[NamedTuple1](a = "a", b = 1)
  testLiteralCreation[NamedTuple2](c = "a", d = 2)
  testLiteralCreation[Struct](Struct(1, "a", false))
  testLiteralCreation[Option[Struct]](None)
  testLiteralCreation[Option[Struct]](Some(Struct(1, "a", false)))
  testLiteralCreation[TestEnum](TestEnum.A)
  testLiteralCreation[Option[TestEnum]](None)
  testLiteralCreation[Option[TestEnum]](Some(TestEnum.C(1)))
  testLiteralCreation[Timestamp](Timestamp.fromMicros(1))
  testLiteralCreation[Timestamp](Timestamp.fromMicros(148600399274030924L))
  testLiteralCreation[Duration](Duration.fromMicros(1))
  testLiteralCreation[Option[Duration]](None)
  testLiteralCreation[Date](Date.fromDays(1))

  testHasSameBehavior[Timestamp, Long]("Timestamp toMicros", _.toMicros, _.toMicros)
  {
    given Arbitrary[Long] = Arbitrary[Timestamp].map(_.toMicros)
    testHasSameBehavior[Long, Timestamp]("Long toTimestamp", microsToTimestamp, Timestamp.fromMicros)
    testHasSameBehavior[Long, Long](
      "Long toTimestamp toMicros roundtrip",
      t => microsToTimestamp(t).toMicros,
      identity
    )
  }
  testHasSameBehavior[Timestamp, Timestamp](
    "Timestamp toMicros toTimestamp roundtrip",
    t => microsToTimestamp(t.toMicros),
    identity
  )
  {
    given Arbitrary[Long] = Arbitrary[Duration].map(_.toMicros)

    testHasSameBehavior[Duration, Long]("Duration toMicros", _.toMicros, _.toMicros)
    testHasSameBehavior[Long, Duration]("Long toDuration", microsToDuration, Duration.fromMicros)
    testHasSameBehavior[Duration, Duration](
      "Duration toMicros toDuration roundtrip",
      d => microsToDuration(d.toMicros),
      identity
    )
    testHasSameBehavior[Long, Long](
      "Long toDuration toMicros roundtrip",
      l => microsToDuration(l).toMicros,
      identity
    )
  }
  testHasSameBehavior[Date, Int]("Date toDays", _.toDays, _.daysSinceEpoch)
  {
    given Arbitrary[Int] = Arbitrary[Date].map(_.daysSinceEpoch)
    testHasSameBehavior[Int, Date]("Int toDate", daysToDate, Date.fromDays)
  }
  testHasSameBehavior[Date, Date]("Date toDays toDate roundtrip", d => daysToDate(d.toDays), identity)

  testLiteralCreation[TreeBounded[Int, 2]](
    Node(1, Seq(Node(2, Seq(Leaf(3), Leaf(4))), Node(5, Seq(Leaf(6)))))
  )
  testLiteralCreation[EmptyTuple](EmptyTuple)
  testLiteralCreation[WithEmptyTuple](WithEmptyTuple(EmptyTuple))
  testLiteralCreation[WithNamedTupleEmpty](WithNamedTupleEmpty(NamedTuple.Empty))

  testHasSameBehavior[(i: Int, s: String, b: Boolean), (s: String, b: Boolean)](
    "remove",
    t => t.remove[Int],
    t => Remover.remove[Int](t)
  )

  testHasSameBehavior[(Int, String, Boolean), NamedTuple[("_1", "_3"), (Int, Boolean)]](
    "remove for tuple",
    t => t.remove[String],
    t => Remover.remove[String](t)
  )

  testHasSameBehavior[(i: Int, s: String, b: Boolean), Int](
    "select",
    t => t.select[Int],
    t => Selector.select[Int](t)
  )

  testHasSameBehavior[Option[
    (a: Int, b: String, c: Boolean)
  ], (a: Option[Int], b: Option[String], c: Option[Boolean])](
    "spreadOption",
    _.spreadOption,
    t => (t.map(_.a), t.map(_.b), t.map(_.c))
  )

  def testJsonRoundtrip[T: ClassTag: Codec: Arbitrary: TypeName: JsonArrayOrObject]: Unit =
    testHasSameBehavior[T, Option[T]](
      s"toJson/fromJson roundtrip for ${TypeName.name[T]}",
      t => fromJson[T](toJson(t)),
      Some(_)
    )

  testJsonRoundtrip[(`name with space`: Int)]
  testJsonRoundtrip[(`name-with-dash`: Int)]
  testJsonRoundtrip[(`0digitfirst`: Int)]
  testJsonRoundtrip[Tuple1[Timestamp]]
  testJsonRoundtrip[Tuple1[Date]]
  testJsonRoundtrip[Tuple1[Duration]]
  testJsonRoundtrip[Tuple1[Option[Option[Int]]]]
  testJsonRoundtrip[Seq[Int]]
  testJsonRoundtrip[Map[String, Int]]
  testJsonRoundtrip[Struct]
  testJsonRoundtrip[Seq[Struct]]
  testJsonRoundtrip[Seq[Seq[Int]]]
  {
    given Arbitrary[(Date, String)] = Arbitrary[Date].map(date => date -> s"""{"_1":"${date.toIsoString}"}""")
    testHasSameBehavior[(Date, String), Option[Date]](
      "json date as YYYY-MM-DD",
      { case Expr(_, json) => fromJson[Tuple1[Date]](json).map(_._1) },
      (date, _) => Some(date)
    )
  }
  {
    given Arbitrary[(Long, String)] = Arbitrary[Long].map(long => long -> s"""{"_1":$long}""")
    testHasSameBehavior[(Long, String), Option[Duration]](
      "json duration as long of microseconds",
      { case Expr(_, json) => fromJson[Tuple1[Duration]](json).map(_._1) },
      (long, _) => Some(Duration.fromMicros(long))
    )
  }
  {
    given Arbitrary[String] =
      Arbitrary.oneOf(
        "{",
        "}",
        "{}",
        """{"value": 0}""",
        """[{"_1": 0}""",
        "{_1:0}",
        """{"_1":null}""",
        "[,]",
        "[null]",
        s"""["${Int.MaxValue.toLong + 1}"]""",
        s"""["${Long.MaxValue.toString}0"]""",
        """[{}]"""
      )
    testHasSameBehavior[String, Option[Tuple1[Int]]](
      "malformed json object results in None",
      fromJson[Tuple1[Int]](_),
      _ => None
    )
    testHasSameBehavior[String, Option[Seq[Int]]](
      "malformed json array results in None",
      fromJson[Seq[Int]](_),
      _ => None
    )
  }
  {
    // extra stuff on the end is not an error
    given Arbitrary[(Int, String)] =
      for {
        int <- Arbitrary.int
        json <- Arbitrary.oneOf(s"""{"_1": $int}""")
        extra <- Arbitrary.string
      } yield (int, json + extra)
    testHasSameBehavior[(Int, String), Option[Tuple1[Int]]](
      "extra stuff after json object is ignored",
      { case Expr(_, json) => fromJson[Tuple1[Int]](json) },
      (int, _) => Some(Tuple1(int))
    )
  }
}
