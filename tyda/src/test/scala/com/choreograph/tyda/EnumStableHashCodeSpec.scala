package com.choreograph.tyda

import scala.annotation.nowarn

import org.scalatest.funsuite.AnyFunSuite

@nowarn("cat=deprecation")
object EnumStableHashCodeSpec {
  enum Color extends EnumStableHashCode {
    case Red, Green, Blue
  }

  enum ColorWithProduct extends EnumStableHashCode {
    case None
    case RGB(r: Int, g: Int, b: Int)
  }
}

class EnumStableHashCodeSpec extends AnyFunSuite {
  import EnumStableHashCodeSpec.{Color, ColorWithProduct}

  test("makes hashCode stable for singletons") {
    val red = Color.Red
    val green = Color.Green
    val blue = Color.Blue

    assert(red.hashCode() == 82033)
    assert(green.hashCode() == 69066467)
    assert(blue.hashCode() == 2073722)
  }

  test("makes hashCode stable for products") {
    val none = ColorWithProduct.None
    val rgb = ColorWithProduct.RGB(0, 0, 0)

    assert(none.hashCode() == 2433880)
    assert(rgb.hashCode() == -269054409)
  }
}
