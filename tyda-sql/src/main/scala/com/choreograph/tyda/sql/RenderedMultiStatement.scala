package com.choreograph.tyda.sql

import scala.util.control.NonFatal

import org.slf4j.LoggerFactory

/** Represents a multi-statement SQL query.
  *
  * The teardown are assumed to be idempotent and will execute even if the main
  * query fails, and any errors during teardown will be logged but not rethrown.
  */
final case class RenderedMultiStatement(setup: Seq[String], query: String, teardown: Seq[String]) {
  import RenderedMultiStatement.logger

  def single: String = (setup :+ query :++ teardown).mkString(";\n\n")

  private def setupAndQuery: String = (setup :+ query).mkString(";\n\n")

  /** Execute the query, setup and cleanup as a single multi statement query. */
  def executeSingle[A](executeQuery: String => A): A =
    try executeQuery(single)
    catch {
      case NonFatal(e) =>
        cleanup(executeQuery)
        throw e
    }

  /** Execute the query and setup as a single multi statement query.
    *
    * The teardown is executed separately.
    */
  def executeSetupAndQuery[A](executeQuery: String => A): A =
    withCleanup(executeQuery)(executeQuery(setupAndQuery))

  /** Execute each statement separately.
    *
    * This is useful for runners that don't support multi-statement queries.
    */
  def execute[A](executeQuery: String => A): A =
    withCleanup(executeQuery) {
      setup.foreach(executeQuery)
      executeQuery(query)
    }

  private def withCleanup[A](executeQuery: String => A)(f: => A): A =
    try f
    finally cleanup(executeQuery)

  private def cleanup[A](executeQuery: String => A): Unit =
    teardown.foreach { q =>
      try executeQuery(q)
      catch { case NonFatal(e) => logger.error(s"Error during teardown query:\n$q", e) }
    }
}

object RenderedMultiStatement {
  private val logger = LoggerFactory.getLogger(getClass)
}
