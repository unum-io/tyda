package com.choreograph.tyda

enum RunnerArgs {
  case BigQuery(
      projectId: String,
      validateReadSchemas: RunnerArgs.ValidateReadSchema = RunnerArgs.ValidateReadSchema.Strict,
      validateWriteCompatibility: RunnerArgs.ValidateWriteCompatibility =
        RunnerArgs.ValidateWriteCompatibility.Strict
  )
  case Iterator
  case Spark(master: Option[String] = None, logLevel: Option[RunnerArgs.SparkLogLevels] = None)
}

object RunnerArgs {
  enum SparkLogLevels {
    case All, Debug, Error, Fatal, Info, Off, Trace, Warn

    def toSparkConf: String = toString.toUpperCase
  }

  /** Control how schemas are validated when reading a BigQuery table.
    *
    *   - `Strict`: The schema must match exactly. Any differences will result
    *     in an error.
    *   - `Warn`: Log a warning for any differences, but allow the read to
    *     proceed.
    *   - `Off`: No validation is performed. Any differences will be ignored.
    */
  enum ValidateReadSchema {
    case Strict, Warn, Off
  }

  /** Control how schemas are validated with BigQuery compatibility.
    *
    * BigQuery has some limitations on the types of schemas it can support. The
    * unsupported cases are
    *
    *   - Directly nested arrays e.g `Seq[Seq[Int]]` are not supported
    *   - Array elements can not be null e.g `Seq[Option[Int]]` are not
    *     supported
    *   - Nullable arrays e.g `Option[Seq[Int]]` are lossily supported and on
    *     writes nulls are changed into empty values.
    *
    * The supported values here are:
    *
    *   - `Strict`: The schema must be fully supported by BigQuery. Any
    *     unsupported features will result in an error.
    *   - `Lossy`: The lossy supported features are allowed but other
    *     incompatibilities will result in an error.
    *   - `Warn`: Log a warning for incompatibilities but allow execution.
    *   - `Off`: No validation is performed.
    */
  enum ValidateWriteCompatibility {
    case Strict, Lossy, Warn, Off
  }

  private def runnerClassName(arg: RunnerArgs): String =
    arg match {
      case BigQuery(projectId = _) => "com.choreograph.tyda.bigquery.BigQueryRunner$"
      case Iterator => "com.choreograph.tyda.iterator.IteratorRunner$"
      case Spark(master = _) => "com.choreograph.tyda.spark.SparkRunner$"
    }

  private def dependencyName(arg: RunnerArgs): String =
    arg match {
      case BigQuery(projectId = _) => "tyda-big-query"
      case Iterator => "tyda-iterator"
      case Spark(master = _) => "tyda-spark"
    }

  // We dynamically load the runner classes so that users only need to include
  // the dependencies for the runners they actually use.
  def createRunner(arg: RunnerArgs, name: String): Runner = {
    val className = runnerClassName(arg)
    val companionClass =
      try Class.forName(className)
      catch
        case _: ClassNotFoundException => throw new RuntimeException(
            s"Runner for '$arg' not found. Make sure that '${dependencyName(arg)}' is on your classpath."
          )
    val argsClass = arg match {
      case BigQuery(projectId = _) | Spark(master = _) => arg.getClass
      // Singletons are erased to the base type.
      case Iterator => classOf[RunnerArgs]
    }
    val companion = companionClass.getDeclaredField("MODULE$").get(null)
    val applyMethod = companionClass.getMethod("apply", classOf[String], argsClass)
    // TYPE SAFETY: We control the implementation of all the runner classes.
    applyMethod.invoke(companion, name, arg).asInstanceOf[Runner]
  }
}
