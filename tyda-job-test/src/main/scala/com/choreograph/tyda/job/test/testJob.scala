package com.choreograph.tyda.job.test

import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.RunnerArgs.SparkLogLevels
import com.choreograph.tyda.iterator.IteratorRunner
import com.choreograph.tyda.job.TydaJob
import com.choreograph.tyda.job.TydaJobContext
import com.choreograph.tyda.table.ArgsParser

private val runnerEnvironmentVariable = "TYDA_JOB_TEST_RUNNER"

/** Checks if the test runner is using spark
  *
  * This is needed because of existing code that is not compatible with spark.
  * But this should only be used as a last resort in new code and ideally never.
  */
def isTestSparkRunner: Boolean = getRunnerArg == TestRunnerArg.Spark

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
    case None => TestRunnerArg.Iterator
  }

def testJob[Args](args: Args)(using job: TydaJob[Args]): Unit = {
  val runnerArg = getRunnerArg
  val runner = runnerArg match {
    case TestRunnerArg.Iterator => IteratorRunner
    case TestRunnerArg.Spark => RunnerArgs.createRunner(
        RunnerArgs.Spark(master = Some("local[2]"), logLevel = Some(SparkLogLevels.Warn)),
        "unittest"
      )
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
