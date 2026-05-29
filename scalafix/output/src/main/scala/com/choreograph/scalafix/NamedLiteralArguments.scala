package com.choreograph.scalafix

object NamedLiteralArguments_v1_Test {
  def complete(isSuccess: Boolean): Unit = ()
  def finish(n: Int, isError: Boolean): Unit = ()
  complete(isSuccess = true)
  complete(isSuccess = true)
  complete(isSuccess = false)
  complete(false) // scalafix:ok; rule suppression
  finish(2, isError = true)

  val x = Array.fill(5)(elem = false)

  def lotsOfArgLists(a: Boolean)(b: Boolean, c: Boolean)(d: Boolean, e: Boolean, f: Boolean): Unit = ???
  lotsOfArgLists(a = true)(b = false, c = true)(d = false, e = true, f = false)

  class MyBuilder {
    def foo(a: Boolean, b: Boolean): MyBuilder = ???
  }
  MyBuilder().foo(a = true, b = false).foo(a = true, b = false).foo(a = true, b = false): Unit

  val _ = String.valueOf(true) // Skip method defined in Java

  extension (a: Boolean) {
    def and(b: Boolean): Boolean = ???
    def or(b: Boolean)(using DummyImplicit): Boolean = ???
    def xor(using DummyImplicit)(b: Boolean): Boolean = ???
  }
  extension (using DummyImplicit)(a: Boolean)(using DummyImplicit)(using DummyImplicit) {
    def andThen(b: Boolean)(c: Boolean): Boolean = ???
  }
  val _ = true.and(b = false)
  val _ = true.or(b = false)(using summon[DummyImplicit])
  val _ = true.xor(b = false)
  val _ = true.xor(using summon[DummyImplicit])(b = false)
  val _ = true.andThen(b = false)(c = true)
}
