/* rule = FinalCaseClass */
package com.choreograph.scalafix

object FinalCaseClassTest {
  case class Person(name: String, age: Int)

  // Already final - should remain unchanged
  final case class Account(id: Long, balance: Double)

  // With other modifiers
  private case class Internal(value: String)

  sealed trait Base
  case class Derived(x: Int) extends Base

  object Container {
    case class Item(id: String)
    case class Metadata(timestamp: Long)
  }

  // With type parameters
  case class Pair[A, B](first: A, second: B)

  // Regular classes should not be affected
  class RegularClass(val x: Int)

  abstract class AbstractClass

  // Case objects should not be affected
  case object Singleton

  // With access modifiers and annotations
  @deprecated("Use Account instead", "1.0")
  private[scalafix] case class OldAccount(id: String)

  enum A {
    case EnumCase1(value: Int)
    case EnumCase2(value: String)
  }
}
