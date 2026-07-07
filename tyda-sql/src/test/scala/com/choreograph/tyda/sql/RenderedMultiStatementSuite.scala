package com.choreograph.tyda.sql

import scala.collection.mutable.ArrayBuffer

import org.scalatest.funsuite.AnyFunSuite

class RenderedMultiStatementSuite extends AnyFunSuite {
  private val setup = Seq("SETUP_1", "SETUP_2")
  private val query = "QUERY"
  private val teardown = Seq("TEARDOWN_1", "TEARDOWN_2")
  private val separator = ";\n\n"

  private val multi = RenderedMultiStatement(setup, query, teardown)
  private def recordExecution(
      onSql: String => Unit = _ => ()
  )(run: (String => Unit) => Any): (Seq[String], Option[Throwable]) = {
    val executed = ArrayBuffer.empty[String]
    val executeQuery: String => Unit = sql => {
      executed += sql
      onSql(sql)
    }
    try {
      run(executeQuery)
      (executed.toSeq, None)
    } catch { case t: Throwable => (executed.toSeq, Some(t)) }
  }

  test("execute runs setup query teardown as separate statements") {
    val (executed, thrown) = recordExecution()(multi.execute)
    assert(thrown.isEmpty)
    assert(executed == setup ++ Seq(query) ++ teardown)
  }

  test("execute runs teardown and rethrows original failure") {
    val boom = new RuntimeException("query failed")
    val (executed, thrown) = recordExecution(sql => if sql == query then throw boom)(multi.execute)
    assert(thrown.exists(_ eq boom))
    assert(executed == setup ++ Seq(query) ++ teardown)
  }

  test("executeSetupAndQuery runs setup+query in one statement then teardown") {
    val (executed, thrown) = recordExecution()(multi.executeSetupAndQuery)
    val setupAndQuery = (setup :+ query).mkString(separator)
    assert(thrown.isEmpty)
    assert(executed == Seq(setupAndQuery) ++ teardown)
  }

  test("executeSetupAndQuery runs teardown and rethrows original failure") {
    val boom = new RuntimeException("setup+query failed")
    val setupAndQuery = (setup :+ query).mkString(separator)
    val (executed, thrown) =
      recordExecution(sql => if sql == setupAndQuery then throw boom)(multi.executeSetupAndQuery)
    assert(thrown.exists(_ eq boom))
    assert(executed == Seq(setupAndQuery) ++ teardown)
  }

  test("executeSingle runs full statement once on success") {
    val (executed, thrown) = recordExecution()(multi.executeSingle)
    assert(thrown.isEmpty)
    assert(executed == Seq(multi.single))
  }

  test("executeSingle retries teardown via cleanup on failure") {
    val boom = new RuntimeException("single failed")
    val (executed, thrown) =
      recordExecution(sql => if sql == multi.single then throw boom)(multi.executeSingle)
    assert(thrown.exists(_ eq boom))
    assert(executed == Seq(multi.single) ++ teardown)
  }

  test("cleanup errors are swallowed so original failure is preserved") {
    val boom = new RuntimeException("query failed")
    val teardownError = new RuntimeException("teardown failed")
    val (executed, thrown) = recordExecution { sql =>
      if sql == query then throw boom
      if sql == teardown.head then throw teardownError
    }(multi.execute)
    assert(thrown.exists(_ eq boom))
    assert(executed == setup ++ Seq(query) ++ teardown)
  }
}
