package com.choreograph.tyda.job

import org.slf4j.LoggerFactory

import com.choreograph.tyda.BuildInfo
import com.choreograph.tyda.table.ArgsParser
import com.choreograph.tyda.table.SourceSinkTraversal

/** Helper trait for defining a main method with argument parsing.
  *
  * Example usage:
  * ```scala
  * final case class MyJobArgs(date: String)
  * object MyJobArgs extends ScalaJob[MyJobArgs] {
  *   def run(args: MyJobArgs): Unit = println(s"Running job for date ${args.date}")
  * }
  * ```
  *
  * The main difference between a ScalaJob and a TydaJob is that the `run`
  * method of TydaJob is expected to be side effect free.
  */
abstract class ScalaJob[JobArgs](using
    parser: => ArgsParser[JobArgs],
    traversal: => SourceSinkTraversal[JobArgs]
) {
  given ScalaJob[JobArgs] = this
  def argsParser: ArgsParser[JobArgs] = parser
  def sourceSinkTraversal: SourceSinkTraversal[JobArgs] = traversal

  private val logger = LoggerFactory.getLogger(getClass)

  final def main(args: Array[String]): Unit =
    logger.info(s"Starting ${getClass.getName.stripSuffix("$")} (${BuildInfo.gitDescribe})")
    ArgsParser.parse[JobArgs](args.toSeq) match {
      case Right(jobArgs) => run(jobArgs)
      case Left(error) => throw new IllegalArgumentException(s"Error parsing arguments: ${error.formatted}")
    }

  /** @param cliArgs
    *   A case class capturing the command-line arguments required for the job,
    *   including the processing date
    */
  def run(cliArgs: JobArgs): Unit
}
