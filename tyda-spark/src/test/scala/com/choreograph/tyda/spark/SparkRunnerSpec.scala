package com.choreograph.tyda.spark

import java.nio.file.Files
import java.nio.file.Path

import scala.util.Using

import org.apache.commons.io.FileUtils
import org.apache.hadoop.mapreduce.lib.output.FileOutputCommitter
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Format
import com.choreograph.tyda.RunnerArgs

object SparkRunnerSpec {
  private final case class TemporaryDirectory(path: Path)
  private given Using.Releasable[TemporaryDirectory] = dir => FileUtils.deleteDirectory(dir.path.toFile)

  private final case class HadoopSetting(key: String, previous: Option[String])
}

class SparkRunnerSpec extends AnyFunSuite with SharedSparkSession {
  import SparkRunnerSpec.{*, given}

  private given Using.Releasable[HadoopSetting] =
    setting => {
      val conf = spark.sparkContext.hadoopConfiguration
      setting.previous.fold(conf.unset(setting.key))(conf.set(setting.key, _))
    }

  private def setHadoopConf(key: String, value: String): HadoopSetting = {
    val conf = spark.sparkContext.hadoopConfiguration
    val previous = Option(conf.get(key))
    conf.set(key, value)
    HadoopSetting(key, previous)
  }

  test("createRunner returns SparkRunner") {
    // Force shared spark session initialization before calling createRunner
    // This should be replaced by making RunnerArg.Spark contain the neccessary args.
    val _ = spark
    val runner = RunnerArgs.createRunner(RunnerArgs.Spark(), "test-app")
    assert(runner.isInstanceOf[SparkRunner])
  }

  test("write has success marker even if the Hadoop configuration disables it") {
    Using.resource(TemporaryDirectory(Files.createTempDirectory("spark-runner-spec"))) { directory =>
      val path = directory.path.resolve("output")
      Using.resource(setHadoopConf(FileOutputCommitter.SUCCESSFUL_JOB_OUTPUT_DIR_MARKER, "false")) { _ =>
        new SparkRunner(using spark).execute(Dataset.from(Seq(1)).writeToPath(path.toString, Format.Parquet))
      }
      assert(Files.isRegularFile(path.resolve(Dataset.SuccessMarker)))
    }
  }
}
