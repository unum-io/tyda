package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

object RemoverSpec {
  private final case class A(a: Int, b: String, c: Long) derives Arbitrary

  opaque type MyInt = Int

  private object MyInt {
    given Arbitrary[MyInt] = Arbitrary[Int]
  }

  // In principle we want an even bigger tuple here, but there's a limit where we run into
  // stack overflow issues. An alternative if we need a bigger tuple is to bump the -Xss JVM flag.
  //
  // format: off
  type HugeTuple = (
      Int, String, Double, Boolean, Long, Float, Short, Byte, Option[Int], Option[String], Option[Double], Option[Boolean],
      Option[Long], Option[Float], Option[Short], Option[Byte], Seq[Int], Seq[String], Seq[Double], Seq[Boolean], Seq[Long], Seq[Float], Seq[Short], Seq[Byte],
      List[Int], List[String], List[Double], List[Boolean], List[Long], List[Float], List[Short], List[Byte], Map[Int, Int], Map[String, String], Map[Double, Double], Map[Boolean, Boolean],
      Map[Long, Long], Map[Float, Float], Map[Short, Short], Map[Byte, Byte], Map[Int, String], Map[String, Int], Map[Double, String], Map[String, Double], Map[Boolean, String], Map[String, Boolean], Map[Long, String], Map[String, Long],
      Map[Float, String], Map[String, Float], Map[Short, String], Map[String, Short], Map[Byte, String], Map[String, Byte], Map[Int, Option[Int]], Map[String, Option[String]], Map[Double, Option[Double]], Map[Boolean, Option[Boolean]], Map[Long, Option[Long]], Map[Short, Option[Short]],
      Map[Byte, Option[Byte]], Map[Float, Option[Float]], Seq[Option[Int]], Seq[Option[String]], Seq[Option[Double]], Seq[Option[Boolean]], Seq[Option[Long]], Seq[Option[Float]], Seq[Option[Short]], Seq[Option[Byte]], Seq[Seq[Int]], Seq[Seq[String]],
      Seq[Seq[Double]], Seq[Seq[Boolean]], Seq[Seq[Long]], Seq[Seq[Float]], Seq[Seq[Short]], Seq[Seq[Byte]], Option[Option[Int]], Option[Option[String]], Option[Option[Double]], Option[Option[Boolean]], Option[Option[Long]], Option[Option[Float]],
      Option[Option[Short]], Option[Option[Byte]], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int,
      Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int,
  )
  // format: on
}

class RemoverSpec extends AnyFunSuite {
  import RemoverSpec.{A, HugeTuple, MyInt}

  private def testWorks[E: TypeName, T: TypeName: Arbitrary: Remover.Of[E] as remover](
      expected: T => remover.Out
  ) = {
    test(s"Remover can remove ${TypeName.name[E]} from ${TypeName.name[T]}") {
      val input = Arbitrary[T]()
      assert(remover(input) == expected(input))
    }
  }

  private given Arbitrary[Double] = Arbitrary[Double].filter(!_.isNaN)

  testWorks[String, (Int, String, Double)](t => (t._1, t._3))
  testWorks[String, (a: Int, b: String, c: Double)](t => (a = t.a, c = t.c))
  testWorks[String, A](t => (a = t.a, c = t.c))
  testWorks[Int, A](t => (b = t.b, c = t.c))
  testWorks[Long, A](t => (a = t.a, b = t.b))
  testWorks[String, (MyInt, String, Double)](t => (t._1, t._3))
  testWorks[MyInt, (MyInt, Int, Double)](t => (t._2, t._3))
  testWorks[Int, (MyInt, Int, Double)](t => (t._1, t._3))
  testWorks[MyInt, (MyInt, String, Double)](t => (t._2, t._3))

  test("Remover can be built quickly for huge tuple") {
    val _ = summon[Remover[NamedTuple.From[HugeTuple], Long]]
  }

  test("Remover fails if field is present multiple times") {
    assertCompileTimeError(
      "summon[Remover[(String, String, Double), String]]",
      "Type String must occur exactly once in *:[String, *:[String, *:[Double, EmptyTuple]]]"
    )
  }

  test("Remover fails if field is not present") {
    assertCompileTimeError(
      "summon[Remover[(Int, String, Double), Float]]",
      "Type Float must occur exactly once in *:[Int, *:[String, *:[Double, EmptyTuple]]]"
    )
  }
}
