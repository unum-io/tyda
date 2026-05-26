package com.choreograph.tyda.iterator

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs

object IteratorRunner extends Runner {
  def execute(action: Dataset.Action): Unit = DatasetOnIterator.perform(action)
  def collect[T](ds: Dataset[T]): Seq[T] = DatasetOnIterator(ds).toSeq
  def explain[T](ds: Dataset[T]): String = com.choreograph.tyda.iterator.explain(ds)
  def explain(action: Dataset.Action): String = com.choreograph.tyda.iterator.explain(action)

  /** Factory method for reflection-based runner creation. */
  def apply(name: String, arg: RunnerArgs.Iterator.type): Runner = IteratorRunner
}
