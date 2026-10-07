package com.choreograph.tyda.job

trait DocumentReader {

  /** Reads all documents available at the external resource identified by
    * `uri`.
    */
  def read(uri: String): Seq[String]
}

object DocumentReader {
  val unimplemented: DocumentReader = uri =>
    throw new UnsupportedOperationException(
      s"No DocumentReader configured for $uri; override TydaJob.documentReader to enable reading."
    )
}
