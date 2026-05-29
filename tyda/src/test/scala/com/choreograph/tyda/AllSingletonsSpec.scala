package com.choreograph.tyda

import scala.util.NotGiven

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

object AllSingletonsSpec {
  enum E1 {
    case C1
    case C2
  }

  enum E2 {
    case C1
    case C2(a: Int)
  }
}

class AllSingletonsSpec extends AnyFunSuite {
  import AllSingletonsSpec.*

  test("exists for enum with only singletons") {
    assert(summon[AllSingletons[E1]].values == Seq(E1.C1, E1.C2))
  }

  test("not exists for enum with non-singleton cases") {
    summon[NotGiven[AllSingletons[E2]]] // Compiletime test
  }
}
