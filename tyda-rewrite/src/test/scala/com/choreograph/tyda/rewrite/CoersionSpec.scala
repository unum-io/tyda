package com.choreograph.tyda.rewrite

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.SimpleTypeName
import com.choreograph.tyda.SimpleTypeName.name as typeName
import com.choreograph.tyda.iterator.ExprEvaluation
import com.choreograph.tyda.rewrite.Coercion.PathSegment

object CoersionSpec {
  given Arbitrary[Float] = Arbitrary.float.filter(!_.isNaN)
}

class CoercionSpec extends AnyFunSuite {
  import CoersionSpec.given

  private def testExact[From: Codec: SimpleTypeName, To: Codec: SimpleTypeName]: Unit =
    test(s"${typeName[From]} => ${typeName[To]} is Exact") {
      assert(Coercion.exact(Codec[From], Codec[To]) == Coercion.Exact)
    }

  private def testExact[T: Codec: SimpleTypeName]: Unit = testExact[T, T]

  private def testWiden[F: Arbitrary: Codec: SimpleTypeName, T: Codec: SimpleTypeName](ref: F => T): Unit =
    test(s"${typeName[F]} widens to ${typeName[T]}") {
      Coercion.exact(Codec[F], Codec[T]) match {
        case Coercion.Widen(f) => (0 to 10).foreach(_ =>
            val v = Arbitrary[F]()
            val eval = ExprEvaluation.lambda(f)
            assert(eval(v) == ref(v), s"Unexpected coercion for $v")
          )
        case other => fail(s"Expected Coercion.Widen but got: $other")
      }
    }

  private def testIncompatible[F: Codec: SimpleTypeName, T: Codec: SimpleTypeName](
      expectedErrors: Coercion.Error*
  ): Unit =
    test(s"${typeName[F]} => ${typeName[T]} is Incompatible") {
      Coercion.exact(Codec[F], Codec[T]) match {
        case Coercion.Incompatible(errors) => assert(errors == expectedErrors)
        case other => fail(s"Expected Coercion.Incompatible but got: $other")
      }
    }

  private def testMismatch[F: Codec: SimpleTypeName, T: Codec: SimpleTypeName]: Unit =
    testIncompatible[F, T](Coercion.Error(Seq.empty, Coercion.ErrorType.Mismatch(Codec[F], Codec[T])))

  testExact[Byte]
  testExact[Short]
  testExact[Int]
  testExact[Long]
  testExact[Float]
  testExact[Double]
  testExact[Boolean]
  testExact[String]
  testExact[Option[Int]]
  testExact[Seq[Int]]
  testExact[Map[String, Int]]
  testExact[(a: Int, b: Int)]
  testExact[(a: Int, b: String, c: Boolean)]
  testExact[(head: (x: Int, y: Int), tail: Int)] // nested product
  testExact[Seq[(a: Int, b: Int)]]
  testExact[Option[(a: Int, b: Int)]]
  testExact[Map[String, (a: Int, b: Int)]]
  testExact[Option[Option[Int]]]
  testExact[Seq[Seq[Int]]]
  testExact[Map[String, Seq[Int]]]

  testWiden[Byte, Short](_.toShort)
  testWiden[Byte, Int](_.toInt)
  testWiden[Byte, Long](_.toLong)
  testWiden[Short, Int](_.toInt)
  testWiden[Short, Long](_.toLong)
  testWiden[Int, Long](_.toLong)
  testWiden[Float, Double](_.toDouble)

  testWiden[Byte, Option[Byte]](Some(_))
  testWiden[Short, Option[Short]](Some(_))
  testWiden[Int, Option[Int]](Some(_))
  testWiden[Long, Option[Long]](Some(_))
  testWiden[(value: Long), Option[Long]](v => Some(v.value))
  testWiden[(value: Option[Long]), Option[Long]](v => v.value)

  testWiden[Option[Short], Option[Long]](_.map(_.toLong))
  testWiden[Option[Option[Short]], Option[Option[Long]]](_.map(_.map(_.toLong)))
  testWiden[Option[Option[Option[Short]]], Option[Option[Option[Long]]]](_.map(_.map(_.map(_.toLong))))
  testWiden[Option[Float], Option[Double]](_.map(_.toDouble))
  testWiden[Option[(a: Int, b: Int)], Option[(a: Long, b: Long)]](_.map(t =>
    (a = t.a.toLong, b = t.b.toLong)
  ))
  testWiden[Option[(value: Option[Int])], Option[Option[Long]]](_.map(_.value.map(_.toLong)))
  testWiden[Option[(value: Option[Int])], Option[Option[Int]]](_.map(_.value))

  testWiden[Seq[Byte], Seq[Short]](_.map(_.toShort))
  testWiden[Seq[Float], Seq[Double]](_.map(_.toDouble))
  testWiden[Seq[(a: Int, b: Int)], Seq[(a: Long, b: Long)]](_.map(t => (a = t.a.toLong, b = t.b.toLong)))
  testWiden[(value: Seq[Int]), Seq[Int]](_.value)

  testWiden[Byte, Option[Long]](b => Some(b.toLong))
  testWiden[Int, Option[Long]](i => Some(i.toLong))
  testWiden[Float, Option[Double]](f => Some(f.toDouble))
  testWiden[(b: Int, a: Int), (a: Int, b: Int)](t => (a = t.a, b = t.b))
  testWiden[(a: Int, b: Int), (a: Long, b: Long)](t => (a = t.a.toLong, b = t.b.toLong))
  testWiden[(a: Int), (a: Int, b: Option[String])](t => (a = t.a, b = None))
  testWiden[(a: Int, b: Int), (a: Int, b: Option[Int])](t => (a = t.a, b = Some(t.b)))
  testWiden[(head: (x: Int, y: Int), tail: Int), (head: (x: Long, y: Long), tail: Int)](t =>
    (head = (x = t.head.x.toLong, y = t.head.y.toLong), tail = t.tail)
  )
  testWiden[(a: Int, b: Int, c: Int), (a: Int, b: Int)](t => (a = t.a, b = t.b))

  testMismatch[Long, Int]
  testMismatch[Long, Short]
  testMismatch[Int, Short]
  testMismatch[Int, Byte]
  testMismatch[Double, Float]
  testMismatch[String, Int]
  testMismatch[Int, String]
  testMismatch[Int, Boolean]
  testMismatch[Boolean, Int]
  testMismatch[Option[Int], Int]
  testMismatch[Seq[Int], Int]
  testMismatch[Int, Seq[Int]]
  testMismatch[Map[String, Int], Seq[Int]]
  testMismatch[(a: Int, b: Int), Int]

  testIncompatible[Option[(value: Option[String])], Option[Option[Int]]](Coercion.Error(
    Seq.empty,
    Coercion.ErrorType.Mismatch(Codec[(value: Option[String])], Codec[Int])
  ))

  testIncompatible[Map[Int, Int], Map[Long, Int]](Coercion.Error(Seq.empty, Coercion.ErrorType.WideningMap))
  testIncompatible[Seq[Int], Seq[Short]](
    Coercion.Error(Seq(PathSegment.Element), Coercion.ErrorType.Mismatch(Codec[Int], Codec[Short]))
  )
  testIncompatible[Seq[(a: Int, b: Int)], Seq[(a: Int, b: Short)]](Coercion.Error(
    Seq(PathSegment.Element, PathSegment.Field("b")),
    Coercion.ErrorType.Mismatch(Codec[Int], Codec[Short])
  ))
  testIncompatible[Map[String, Int], Map[String, Long]](
    Coercion.Error(Seq.empty, Coercion.ErrorType.WideningMap)
  )
  testIncompatible[(a: Int, b: Int), (a: Int, b: String)](
    Coercion.Error(Seq(PathSegment.Field("b")), Coercion.ErrorType.Mismatch(Codec[Int], Codec[String]))
  )
  testIncompatible[(head: (x: Int, y: Int), tail: Int), (head: (x: String, y: Int), tail: Int)](
    Coercion.Error(
      Seq("head", "x").map(PathSegment.Field(_)),
      Coercion.ErrorType.Mismatch(Codec[Int], Codec[String])
    )
  )
  testIncompatible[(a: Long, b: Long), (a: Int, b: Int)](
    Coercion.Error(Seq(PathSegment.Field("a")), Coercion.ErrorType.Mismatch(Codec[Long], Codec[Int])),
    Coercion.Error(Seq(PathSegment.Field("b")), Coercion.ErrorType.Mismatch(Codec[Long], Codec[Int]))
  )
  testIncompatible[Option[Long], Option[Int]](
    Coercion.Error(Seq.empty, Coercion.ErrorType.Mismatch(Codec[Long], Codec[Int]))
  )
  testIncompatible[(p: Int, q: Int), (a: Int, b: Int)](
    Coercion.Error(Seq(PathSegment.Field("a")), Coercion.ErrorType.MissingRequired(Codec[Int])),
    Coercion.Error(Seq(PathSegment.Field("b")), Coercion.ErrorType.MissingRequired(Codec[Int]))
  )
  testIncompatible[(a: Long, b: Int), (a: Int, c: String)](
    Coercion.Error(Seq(PathSegment.Field("a")), Coercion.ErrorType.Mismatch(Codec[Long], Codec[Int])),
    Coercion.Error(Seq(PathSegment.Field("c")), Coercion.ErrorType.MissingRequired(Codec[String]))
  )

  test("formatErrors renders every problem in a record") {
    Coercion.exact(Codec[(a: Seq[String], b: Long)], Codec[(a: Seq[Int], b: Int)]) match {
      case Coercion.Incompatible(errors) =>
        val expected = s"""Could not coerce the value:
           |  - a[*] has type ${Codec[String]}, which cannot be coerced to ${Codec[Int]}
           |  - b has type ${Codec[Long]}, which cannot be coerced to ${Codec[Int]}""".stripMargin
        assert(errors.fmt == expected)
      case other => fail(s"Expected Coercion.Incompatible but got: $other")
    }
  }

}
