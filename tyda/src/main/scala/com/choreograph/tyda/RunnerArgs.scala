package com.choreograph.tyda

enum RunnerArgs {
  case BigQuery(
      projectId: String,
      validateSchemas: RunnerArgs.ValidateSchema = RunnerArgs.ValidateSchema.Strict
  )
  case Iterator
  case Spark
}

object RunnerArgs {
  enum ValidateSchema {
    case Strict, Warn, Off
  }
  private def runnerClassName(arg: RunnerArgs): String =
    arg match {
      case BigQuery(projectId = _) => "com.choreograph.tyda.bigquery.BigQueryRunner$"
      case Iterator => "com.choreograph.tyda.iterator.IteratorRunner$"
      case Spark => "com.choreograph.tyda.spark.SparkRunner$"
    }

  private def dependencyName(arg: RunnerArgs): String =
    arg match {
      case BigQuery(projectId = _) => "tyda-big-query"
      case Iterator => "tyda-iterator"
      case Spark => "tyda-spark"
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
      case BigQuery(projectId = _) => arg.getClass
      // Singletons are erased to the base type.
      case Iterator | Spark => classOf[RunnerArgs]
    }
    val companion = companionClass.getDeclaredField("MODULE$").get(null)
    val applyMethod = companionClass.getMethod("apply", classOf[String], argsClass)
    // TYPE SAFETY: We control the implementation of all the runner classes.
    applyMethod.invoke(companion, name, arg).asInstanceOf[Runner]
  }
}
