/*
rule = Disallowed

Disallowed.methods = [
  {
    "symbol": "scala.Option#get",
    "message": "Disallow use of Option.get, prefer using getOrElse or pattern matching."
  }
]
 */
package com.choreograph.scalafix

object DisallowOptionGet {
  val _ = Some(1).get // ok value is known to be Some
  val _ = Option(1).get // assert: Disallowed.Method

  List(Some(1), None).foreach(_.get) // assert: Disallowed.Method

  val opt = Option(1)
  import opt.get as myGet // assert: Disallowed.Method
  val _: Int = myGet // assert: Disallowed.Method

  val _: Int = scala.util.Try(1).get
}
