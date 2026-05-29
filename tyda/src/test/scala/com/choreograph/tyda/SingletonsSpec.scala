package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object SingletonsSpec {
  enum Color {
    case Red, Green, Blue
  }

  enum Shape {
    case Circle, Square
    case Rectangle(width: Int, height: Int)
  }

  enum Coordinate {
    case Point2D(x: Int, y: Int)
    case Point3D(x: Int, y: Int, z: Int)
  }

  sealed trait Result
  object Result {
    case object Success extends Result
    case object Failure extends Result
    final case class Error(message: String) extends Result
  }
}

class SingletonsSpec extends AnyFunSuite {
  import SingletonsSpec.*

  test("all singleton enum") {
    val singletons = Singletons[Color]
    assert(singletons.toSet == Set(Color.Red, Color.Green, Color.Blue))
  }

  test("mixed enum filters out product cases") {
    val singletons = Singletons[Shape]
    assert(singletons.toSet == Set(Shape.Circle, Shape.Square))
  }

  test("product-only enum returns empty") { assert(Singletons[Coordinate].isEmpty) }

  test("consistent across calls") { assert(Singletons[Color] == Singletons[Color]) }

  test("sealed trait filters out product cases") {
    val singletons = Singletons[Result]
    assert(singletons.toSet == Set(Result.Success, Result.Failure))
  }
}
