package com.choreograph.tyda.job.test

import org.apache.spark.sql.SparkSession

import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.UnionMirror.derived
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.job.TydaJob
import com.choreograph.tyda.job.TydaJobContext
import com.choreograph.tyda.spark.SparkRunner
import com.choreograph.tyda.table.ArgsParser

private val runnerEnvironmentVariable = "TYDA_JOB_TEST_RUNNER"

// TODO: Ideally we should support all runners here, but we need to decide how to handle runners that takes extra arguments.
type TestRunnerArg = RunnerArgs.Iterator.type | RunnerArgs.Spark.type

/** Checks if the test runner is using spark
  *
  * This is needed because of existing code that is not compatible with spark.
  * But this should only be used as a last resort in new code and ideally never.
  */
def isTestSparkRunner: Boolean = getRunnerArg == RunnerArgs.Spark

private def getRunnerArg: TestRunnerArg =
  sys.env.get(runnerEnvironmentVariable) match {
    case Some(str) =>
      val parser = ArgsParser.Arg[TestRunnerArg]
      parser
        .parse(str)
        .getOrElse(
          throw new RuntimeException(
            s"Invalid runner value for $runnerEnvironmentVariable: $str hint: ${parser.hint}"
          )
        )
    case None => RunnerArgs.Iterator
  }

def testJob[Args](args: Args)(using job: TydaJob[Args]): Unit = {
  val runnerArg = getRunnerArg
  val runner = runnerArg match {
    case RunnerArgs.Iterator => IteratorRunner
    case RunnerArgs.Spark =>
      val spark = SparkSession
        .builder()
        .config("spark.log.level", "WARN")
        .master("local[2]")
        .appName("unittest")
        .getOrCreate()
      new SparkRunner(using spark)
  }
  val context = TydaJobContext(runner)
  job.run(args)(using context)
  context.run()
  val sinksWrittenTo = context.usedSinks.toSet

  job
    .sourceSinkTraversal
    .sinks(args)
    .filter(sink => !sinksWrittenTo.contains(sink.sink))
    .foreach(sink => throw new RuntimeException(s"Sink for ${sink.name.value} was never written to"))
}
