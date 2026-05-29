/* rule = NamedLiteralArguments */
package com.choreograph.scalafix

object NamedLiteralArguments_v1_Test {
  def complete(isSuccess: Boolean): Unit = ()
  def finish(n: Int, isError: Boolean): Unit = ()
  complete(true)
  complete(isSuccess = true)
  complete(false)
  complete(false) // scalafix:ok; rule suppression
  finish(2, true)

  val x = Array.fill(5)(false)

  def lotsOfArgLists(a: Boolean)(b: Boolean, c: Boolean)(d: Boolean, e: Boolean, f: Boolean): Unit = ???
  lotsOfArgLists(true)(false, true)(false, true, false)

  class MyBuilder {
    def foo(a: Boolean, b: Boolean): MyBuilder = ???
  }
  MyBuilder().foo(true, false).foo(true, false).foo(true, false): Unit

  val _ = String.valueOf(true) // Skip method defined in Java

  extension (a: Boolean) {
    def and(b: Boolean): Boolean = ???
    def or(b: Boolean)(using DummyImplicit): Boolean = ???
    def xor(using DummyImplicit)(b: Boolean): Boolean = ???
  }
  extension (using DummyImplicit)(a: Boolean)(using DummyImplicit)(using DummyImplicit) {
    def andThen(b: Boolean)(c: Boolean): Boolean = ???
  }
  val _ = true.and(false)
  val _ = true.or(false)(using summon[DummyImplicit])
  val _ = true.xor(false)
  val _ = true.xor(using summon[DummyImplicit])(false)
  val _ = true.andThen(false)(true)
}
