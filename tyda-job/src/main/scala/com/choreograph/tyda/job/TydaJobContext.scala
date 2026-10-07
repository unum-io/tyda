package com.choreograph.tyda.job

import java.util.UUID

import scala.collection.mutable

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Runner
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.table.Partitioner
import com.choreograph.tyda.table.Sink
import com.choreograph.tyda.table.Source

class TydaJobContext(
    private val runner: Runner,
    private val documentWriter: DocumentWriter = DocumentWriter.unimplemented,
    private val documentReader: DocumentReader = DocumentReader.unimplemented
) {
  import TydaJobContext.Write

  def this(args: TydaJobArgs, name: String) = this(RunnerArgs.createRunner(args.runner, name))

  def this(args: TydaJobArgs, name: String, documentWriter: DocumentWriter, documentReader: DocumentReader) =
    this(RunnerArgs.createRunner(args.runner, name), documentWriter, documentReader)

  private val writes = mutable.Queue.empty[Write[?, ?]]
  private val documentWrites = mutable.Queue.empty[(Dataset[String], Sink.Document[?, ?])]

  private[tyda] def usedSinks: Seq[Sink[?, ?]] =
    writes.iterator.map(_.sink).toSeq ++ documentWrites.iterator.map(_._2).toSeq

  def write[T, P <: Partitioner](ds: Dataset[T], sink: Sink[T, P], partitioner: P): Unit =
    writes.enqueue(Write(ds, sink, partitioner))

  def writeDocument(ds: Dataset[String], sink: Sink.Document[?, ?]): Unit = documentWrites.enqueue((ds, sink))

  def readDocument(source: Source.Document[?, ?]): Dataset[String] =
    Dataset.from(documentReader.read(source.uri))

  private def toSinkSource[T](
      checkpoint: CheckpointArg,
      ds: Dataset[T]
  ): (Sink[T, Partitioner.None], Source[T, Partitioner.None]) =
    checkpoint match {
      case CheckpointArg.Test(verify) => (Sink.Test(verify), Source.Test(privateCollect(ds)))
      case CheckpointArg.Path(basePath) =>
        val uuid = UUID.randomUUID().toString
        (Sink.Path(s"$basePath/$uuid/"), Source.Path(s"$basePath/$uuid/"))
    }

  def checkpoint[T](ds: Dataset[T], checkpoint: CheckpointArg): Dataset[T] =
    val (sink, source) = toSinkSource(checkpoint, ds)
    writes.enqueue(Write(ds, sink, Partitioner.None))
    source.read(using ds.codec)

  @deprecated(
    "Should not be used and will be removed.\n" +
      "Jobs structure should not depend on data directly instead subqueries can be used in some cases."
  )
  def collect[T](ds: Dataset[T]): Seq[T] = runner.collect(ds)

  private def privateCollect[T](ds: Dataset[T]): Seq[T] = runner.collect(ds)

  def run(): Unit =
    writes.foreach { case Write(dataset, sink, partitioner) =>
      sink match {
        case Sink.Path(basePath, format) =>
          val write = dataset.writeToPath(partitioner.path(basePath), format)
          runner.execute(write)
        case Sink.Document(_) => throw new UnsupportedOperationException(
            "Document sinks are not writable through Tyda's Dataset API; write directly"
          )
        case Sink.Test(verifiers) =>
          val verify = verifiers.getVerifier(partitioner)
          val collected = privateCollect(dataset)
          verify(collected)
      }
    }
    documentWrites.foreach { case (dataset, sink) =>
      privateCollect(dataset).foreach(document => documentWriter.write(sink.uri, document))
    }
}

object TydaJobContext {
  private final case class Write[T, P <: Partitioner](ds: Dataset[T], sink: Sink[T, P], partitioner: P)
}
