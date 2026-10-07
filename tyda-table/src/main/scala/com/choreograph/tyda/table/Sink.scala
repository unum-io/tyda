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

  /** Sink that is written as opaque documents to a system outside of Tyda's
    * Dataset API (e.g. a graph database). The pipeline builder is responsible
    * for both the meaning of `uri` and for actually performing the write.
    *
    * @param uri
    *   An identifier for the external resource. Also used by DAG discovery to
    *   match this sink up with sources that depend on it.
    */
  case Document(uri: String) extends Sink[M, P]

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
