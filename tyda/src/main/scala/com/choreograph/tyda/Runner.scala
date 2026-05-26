package com.choreograph.tyda

trait Runner {
  def execute(ds: Dataset.Action): Unit
  def collect[T](ds: Dataset[T]): Seq[T]

  /** Provides a human-readable explanation of the execution plan for the given
    * dataset.
    */
  def explain[T](ds: Dataset[T]): String

  /** Provides a human-readable explanation of the execution plan for the given
    * dataset action.
    */
  def explain(action: Dataset.Action): String
}
