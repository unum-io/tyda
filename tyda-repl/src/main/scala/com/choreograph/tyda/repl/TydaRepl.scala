package com.choreograph.tyda.repl

import scala.util.chaining.scalaUtilChainingOps

import dotty.tools.repl.ReplDriver

import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.table.ArgsParser

object TydaRepl {
  final case class Args(runner: RunnerArgs, showBanner: Boolean = true) derives ArgsParser

  given ArgsParser[RunnerArgs] = ArgsParser.derived

  def initRunner(args: Seq[String]): Runner =
    ArgsParser.parse[RunnerArgs](args) match {
      case Right(args) => RunnerArgs.createRunner(args, "tyda-repl")
      case Left(error) =>
        throw new RuntimeException(s"Internal tyda repl initialization error\n${error.formatted}")
    }

  def run(args: Args, initCode: Option[String] = None, infoMessage: Option[String] = None): Unit = {
    args.runner match
      case RunnerArgs.Spark => if System.getProperty("spark.master") == null then
          System.setProperty("spark.master", "local[*]"): Unit
      case _ =>

    val classPath = System.getProperty("java.class.path")
    val settings = Array("-classpath", classPath, "-color:always")
    val driver = new ReplDriver(settings, System.out, None)

    if args.showBanner then {
      System.out.println(Banner)
      System.out.println(s"Using runner: ${args.runner}")
      infoMessage.foreach(System.out.println)
    }

    val state = driver
      .initialState
      .pipe(s => driver.run(preamble(args.runner))(using s.copy(quiet = true)))
      .pipe(s => initCode.fold(s)(code => driver.run(code)(using s.copy(quiet = true))))
      .copy(quiet = false)
    driver.runUntilQuit(using state)(): Unit
  }

  // If the bind was implemented https://github.com/scala/scala3/issues/5069
  // We should be able to bind the runner instead of needing to build this complicated string
  private def preamble(args: RunnerArgs): String = {
    val serializedArgs = ArgsParser
      .serialize[RunnerArgs](args)
      .map(_.replace("\\", "\\\\").replace("\"", "\\\""))
      .mkString("Seq(\"", "\", \"", "\")")
    s"""|import com.choreograph.tyda.{Dataset, Date}
        |import com.choreograph.tyda.functions.*
        |import com.choreograph.tyda.aggregates.*
        |import com.choreograph.tyda.repl.ReplMethods.*
        |given runner: com.choreograph.tyda.Runner = com.choreograph.tyda.repl.TydaRepl.initRunner($serializedArgs)
        |""".stripMargin
  }

  def main(args: Array[String]): Unit =
    ArgsParser.parse[Args](args.toSeq) match
      case Right(parsed) => run(parsed)
      case Left(error) =>
        System.err.println(error.formatted(0))
        sys.exit(1)

  private val Banner = """
      |   ______        __
      |  /_  __/_ _____/ /__ _
      |   / / / // / _  / _ `/
      |  /_/  \_, /\_,_/\_,_/
      |      /___/
      |""".stripMargin
}
