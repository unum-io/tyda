package com.choreograph.tyda.job

import org.slf4j.LoggerFactory

import com.choreograph.tyda.BuildInfo
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.table.ArgsParser
import com.choreograph.tyda.table.Partitioner
import com.choreograph.tyda.table.Partitioner.Creator
import com.choreograph.tyda.table.Sink
import com.choreograph.tyda.table.Source
import com.choreograph.tyda.table.SourceSinkTraversal

final case class TydaJobArgs(runner: RunnerArgs) derives ArgsParser
object TydaJobArgs {
  /* Currently we can not define this on RunnerArgs as ArgsParser is defined in tyda-table and runner args in
   * tyda. */
  given ArgsParser[RunnerArgs] = ArgsParser.derived
}

/** Base class for Tyda jobs.
  *
  * Note: Most implementations of this class are objects and therefore do not
  * need to be serialized themselves. However, not extending [[Serializable]]
  * makes it impossible for subclasses to be serialized. So out of convenience
  * we extend [[Serializable]] here.
  */
abstract class TydaJob[JobArgs](using
    parser: => ArgsParser[JobArgs],
    traversal: => SourceSinkTraversal[JobArgs]
) extends Serializable {
  given TydaJob[JobArgs] = this
  def argsParser: ArgsParser[JobArgs] = parser
  /* We cache the traversal here to avoid deriving it multiple times without adding boilerplate to the job
   * class */
  def sourceSinkTraversal: SourceSinkTraversal[JobArgs] = traversal

  /** Override to supply a real implementation capable of writing to an external
    * system (e.g. an http4s-backed client with credentials), enabling
    * `writeDocument`.
    */
  protected def documentWriter: DocumentWriter = DocumentWriter.unimplemented

  /** Override to supply a real implementation capable of reading from an
    * external system (e.g. an http4s-backed client with credentials), enabling
    * `readDocument`.
    */
  protected def documentReader: DocumentReader = DocumentReader.unimplemented

  private val logger = LoggerFactory.getLogger(getClass)

  final def main(args: Array[String]): Unit =
    logger.info(s"Starting ${getClass.getName.stripSuffix("$")} (${BuildInfo.gitDescribe})")
    if args.contains("--help") then
      Console.println(s"TydaJob options:\n${ArgsParser.help[TydaJobArgs]}\n")
      Console.println(s"Job ${getClass.getName.stripSuffix("$")} options:\n${ArgsParser.help[JobArgs]}")
      System.exit(0)

    ArgsParser.parse[TydaJobArgs, JobArgs](args.toSeq) match {
      case Right((tydaArgs, jobArgs)) =>
        val context: TydaJobContext =
          new TydaJobContext(tydaArgs, getClass.getName, documentWriter, documentReader)
        run(jobArgs)(using context)
        context.run()
      case Left(error) => throw new IllegalArgumentException(s"Error parsing arguments: ${error.formatted}")
    }

  extension [T, V, P <: Partitioner: Creator.From[V] as creator](
      dataset: Dataset[T]
  )(using context: TydaJobContext) {
    def write(sink: Sink[T, P], partitionValue: V): Unit =
      context.write(dataset, sink, creator.create(partitionValue))
  }

  extension [T](dataset: Dataset[T])(using context: TydaJobContext) {

    /** Checkpoints the dataset.
      *
      * Checkpointing means the data up until this point will be stored, and any
      * downstream operations will reuse the stored data. Crucially, this means
      * that data lineage is truncated.
      *
      * Note: The arg should be provided as job input, in order to support unit
      * testing.
      */
    def checkpoint(arg: CheckpointArg): Dataset[T] = context.checkpoint(dataset, arg)
  }

  extension (dataset: Dataset[String])(using context: TydaJobContext) {

    /** Writes each row as a document to an external sink via the configured
      * [[documentWriter]].
      */
    def writeDocument(sink: Sink.Document[?, ?]): Unit = context.writeDocument(dataset, sink)
  }

  extension (source: Source.Document[?, ?])(using context: TydaJobContext) {

    /** Reads each document available at the source as a row, via the configured
      * [[documentReader]].
      */
    def readDocument(): Dataset[String] = context.readDocument(source)
  }

  /** @param cliArgs
    *   A case class capturing the command-line arguments required for the job,
    *   including the processing date
    */
  def run(cliArgs: JobArgs)(using TydaJobContext): Unit
}

object TydaJob {}
