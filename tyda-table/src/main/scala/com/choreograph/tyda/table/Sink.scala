package com.choreograph.tyda.table

import com.choreograph.tyda.Format

enum Sink[M, P <: Partitioner] {

  /** Sink that is written to a path on cloud storage.
    *
    * @param basePath
    *   The path to write to.
    * @param format
    *   The data format to use when writing.
    */
  case Path(basePath: String, format: Format = Format.Parquet) extends Sink[M, P]

  /** Sink that is written to in a unit test.
    *
    * @param verify
    *   A function that will be called with the data that was written to the
    *   sink.
    */
  case Test(verify: TestVerifier[M])
}

object Sink {
  object Test {
    def apply[M, P <: Partitioner](verifier: Seq[M] => Any): Sink[M, P] =
      Sink.Test(TestVerifier.Fixed(verifier))

    def apply[M, V, P <: Partitioner: Partitioner.Creator.From[V] as creator](
        data: (V, Seq[M] => Any)*
    ): Sink[M, P] =
      new Test(TestVerifier.Partitioned(
        data.map((partitionValue, verifier) => creator.create(partitionValue).path("/") -> verifier).toMap
      ))
  }
}
