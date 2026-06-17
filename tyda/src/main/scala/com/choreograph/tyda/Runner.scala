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

  /** Collect the value of a Dataset that's known to contain a single value. */
  final def collectValue[T](ds: Dataset.Single[T]): T = {
    val values = collect(ds: Dataset[T])
    assert(values.size == 1, s"Dataset did not contain exactly one value got: $values")
    values.head
  }
}
