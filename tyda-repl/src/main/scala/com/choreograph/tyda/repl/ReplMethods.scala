package com.choreograph.tyda.repl

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner

object ReplMethods {
  extension [T](ds: Dataset[T])(using runner: Runner)
    // TODO: Implment some nice show here. Do we want tabular output? Or
    // should we just wait for Scala 3.8.x and get pprint for free?
    // https://github.com/scala/scala3/pull/23849
    def show(n: Int = 20): Unit =
      val rows = runner.collect(ds.limit(n))
      rows.foreach(row => System.out.println(row))

    /** Collect the dataset into a sequence.
      *
      * Note: This can cause OOM if the dataset is too large.
      */
    def toSeq: Seq[T] = runner.collect(ds)

    /** Explain the dataset by printing the execution plan.
      */
    def explain(): Unit = System.out.println(runner.explain(ds))
}
