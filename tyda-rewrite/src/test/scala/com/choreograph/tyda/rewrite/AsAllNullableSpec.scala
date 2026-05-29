package com.choreograph.tyda.rewrite

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.NumericsReadMode.FailableReadAdapter
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.iterator.ExprEvaluation

object AsAllNullableSpec {
  private final case class SimpleProduct(a: String, b: Int, c: Option[Double])
  private type SimpleProductAllNullable = (a: Option[String], b: Option[Int], c: Option[Double])

  private def traverseSeq[T, U](seq: Seq[T])(f: T => Option[U]): Option[Vector[U]] =
    seq.foldLeft(Option(Vector.empty[U])) { case (acc, elem) =>
      for { accVec <- acc; u <- f(elem) } yield accVec :+ u
    }

  extension [T](seq: Seq[Option[T]]) private def sequence: Option[Seq[T]] = traverseSeq(seq)(identity)

  private def simpleProductExpected(nullable: SimpleProductAllNullable): Option[SimpleProduct] =
    for {
      a <- nullable.a
      b <- nullable.b
    } yield SimpleProduct(a, b, nullable.c)

  private final case class NestedProduct(x: SimpleProduct, y: Option[SimpleProduct])
  private type NestedProductAllNullable =
    (x: Option[SimpleProductAllNullable], y: Option[SimpleProductAllNullable])

  private def nestedProductExpected(nullable: NestedProductAllNullable): Option[NestedProduct] =
    for {
      x <- nullable.x.flatMap(simpleProductExpected)
      y <- nullable.y match {
        case None => Some(None)
        case Some(yNullable) => simpleProductExpected(yNullable).map(Some(_))
      }
    } yield NestedProduct(x, y)
}

class AsAllNullableSpec extends AnyFunSuite {
  import AsAllNullableSpec.*

  private def checkAdapter[T: Codec: TypeName, Nullable: Codec: Arbitrary](
      expected: Nullable => Option[T]
  ) = {
    test(s"all nullable adapter for ${TypeName.name[T]}") {
      val nullableCodec = Codec[Nullable]
      AsAllNullable.children(Codec[T]) match {
        case Some(FailableReadAdapter(`nullableCodec`, cast, check)) => for (_ <- 1 to 100) {
            val nullableValue = Arbitrary[Nullable]()
            val castEval = ExprEvaluation.lambda(cast)
            val checkEval = check.fold((_: Nullable) => true)(ExprEvaluation.lambda(_))
            val expectedValue = expected(nullableValue)
            val actualValue = Option.when(checkEval(nullableValue))(castEval(nullableValue))
            assert(
              actualValue == expectedValue,
              s"Expected ${expectedValue} but got ${actualValue} for input ${nullableValue}"
            )
          }
        case Some(FailableReadAdapter(otherCodec, _, _)) => fail(s"Expected a adapter for type ${TypeName
              .name[T]} with codec ${Codec[Nullable]}, but got codec ${otherCodec}")
        case None => fail(s"Expected a adapter for type ${TypeName.name[T]}, but got None")
      }
    }
  }

  private def checkNoAdapter[T: Codec: TypeName](): Unit = {
    test(s"no all nullable adapter for ${TypeName.name[T]}") {
      AsAllNullable.children(Codec[T]) match {
        case Some(FailableReadAdapter(otherCodec, _, _)) =>
          fail(s"Expected no adapter for type ${TypeName.name[T]}, but got codec ${otherCodec}")
        case None => ()
      }
    }
  }

  checkAdapter[SimpleProduct, SimpleProductAllNullable](simpleProductExpected)
  checkAdapter[NestedProduct, NestedProductAllNullable](nestedProductExpected)
  checkAdapter[Option[SimpleProduct], Option[SimpleProductAllNullable]] {
    case None => Some(None)
    case Some(nullable) => simpleProductExpected(nullable).map(Some(_))
  }
  checkAdapter[Seq[SimpleProduct], Seq[Option[SimpleProductAllNullable]]] { nullable =>
    for {
      seq <- nullable.sequence
      products <- seq.map(simpleProductExpected).sequence
    } yield products
  }
  checkAdapter[IndexedSeq[Int], Seq[Option[Int]]] { nullable =>
    for { seq <- nullable.sequence } yield seq.toIndexedSeq
  }
  checkAdapter[Seq[Int], Seq[Option[Int]]] { _.sequence }

  checkNoAdapter[Option[Int]]()
  checkNoAdapter[SimpleProductAllNullable]()
  checkNoAdapter[NestedProductAllNullable]()
  checkNoAdapter[Seq[Option[Int]]]()
  checkNoAdapter[Option[Seq[Option[Int]]]]()
  checkNoAdapter[Option[Option[Int]]]()
  checkNoAdapter[Option[Option[Option[Int]]]]()
  checkNoAdapter[Option[Option[Option[Option[Int]]]]]()
}
