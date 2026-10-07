package com.choreograph.tyda.job

trait DocumentWriter {

  /** Writes a single document to the external resource identified by `uri`. */
  def write(uri: String, document: String): Unit
}

object DocumentWriter {
  val unimplemented: DocumentWriter = (uri, _) =>
    throw new UnsupportedOperationException(
      s"No DocumentWriter configured for $uri; override TydaJob.documentWriter to enable writing."
    )
}
