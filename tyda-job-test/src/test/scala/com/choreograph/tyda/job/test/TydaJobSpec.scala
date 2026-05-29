package com.choreograph.tyda.job.test

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.job.CheckpointArg
import com.choreograph.tyda.job.TydaJob
import com.choreograph.tyda.job.TydaJobContext
import com.choreograph.tyda.table.Partitioner
import com.choreograph.tyda.table.Sink
import com.choreograph.tyda.table.Source

object TydaJobSpec {
  final case class Model(name: String)
  object Job extends TydaJob[Job.Args] {
    final case class Args(source: Source[Model, Partitioner.None], sink: Sink[Model, Partitioner.None])

    def run(args: Args)(using TydaJobContext): Unit = {
      args.source.read.filter(_.name == "a").write(args.sink, EmptyTuple)
    }
  }

  object CheckpointJob extends TydaJob[CheckpointJob.Args] {
    final case class Args(
        source: Source[String, Partitioner.None],
        checkpoint: CheckpointArg,
        sink: Sink[String, Partitioner.None]
    )

    def run(args: Args)(using TydaJobContext): Unit = {
      args
        .source
        .read
        .map(_ + ": before checkpoint")
        .checkpoint(args.checkpoint)
        .map(_.replaceAll("before", "after"))
        .write(args.sink, EmptyTuple)
    }
  }

  object CheckpointReuseJob extends TydaJob[CheckpointReuseJob.Args] {
    final case class Args(
        source: Source[String, Partitioner.None],
        checkpoint: CheckpointArg,
        sink1: Sink[String, Partitioner.None],
        sink2: Sink[String, Partitioner.None]
    )

    def run(args: Args)(using TydaJobContext): Unit = {
      val cp = args.source.read.checkpoint(args.checkpoint)
      cp.write(args.sink1, EmptyTuple)
      cp.write(args.sink2, EmptyTuple)
    }
  }

  object CheckpointTwiceJob extends TydaJob[CheckpointTwiceJob.Args] {
    final case class Args(
        source: Source[String, Partitioner.None],
        checkpoint: CheckpointArg,
        sink: Sink[String, Partitioner.None]
    )

    def run(args: Args)(using TydaJobContext): Unit = {
      args
        .source
        .read
        .map(_ + ": before")
        .checkpoint(args.checkpoint)
        .map(_.replaceAll("before", "after"))
        .checkpoint(args.checkpoint)
        .map(_.replaceAll("after", "finally"))
        .write(args.sink, EmptyTuple)
    }
  }

  final case class PartitionValue(p: Int)
  object PartitionValue {
    extension [T: Codec](source: Source[T, Partitioner.Hive[PartitionValue]]) {
      def readWithPartitions(): Dataset[(PartitionValue, T)] = {
        val partitioner = Partitioner.Hive.fromSeqs[PartitionValue](Seq[Int]() *: EmptyTuple)
        source.asDataset(partitioner).withPartitionValues
      }
      def read(): Dataset[T] = readWithPartitions().select(_._2)
      def read(p: Int): Dataset[T] = {
        val partitioner = Partitioner.Hive.fromValue(PartitionValue(p))
        source.asDataset(partitioner).values
      }
    }
  }

  object JobPartitionedRead extends TydaJob[JobPartitionedRead.Args] {
    final case class Args(
        source: Source[Model, Partitioner.Hive[PartitionValue]],
        sink: Sink[Model, Partitioner.None]
    )

    def run(args: Args)(using TydaJobContext): Unit = {
      def read(p: Int): Dataset[Model] = args.source.read(p).map(v => v.copy(name = v.name + p.toString))

      read(1).union(read(2)).write(args.sink, EmptyTuple)
    }
  }

  object JobPartitionedReadUnfixed extends TydaJob[JobPartitionedReadUnfixed.Args] {
    final case class Args(
        source: Source[Model, Partitioner.Hive[PartitionValue]],
        sink: Sink[Model, Partitioner.None]
    )

    def run(args: Args)(using TydaJobContext): Unit = {
      args.source.readWithPartitions().map((p, v) => v.copy(name = v.name + p.p)).write(args.sink, EmptyTuple)
    }
  }

  object JobPartitionedWrite extends TydaJob[JobPartitionedWrite.Args] {
    final case class Args(
        source: Source[Model, Partitioner.None],
        sink: Sink[Model, Partitioner.Hive[PartitionValue]]
    )

    def run(args: Args)(using TydaJobContext): Unit = {
      def write(p: Int): Unit =
        args.source.read.map(v => v.copy(name = v.name + p.toString)).write(args.sink, PartitionValue(p))

      write(1)
      write(2)
    }
  }
}

class TydaJobSpec extends AnyFunSuite {
  import TydaJobSpec.*

  test("basic") {
    var ranVerify = false
    testJob(Job.Args(
      Source.Test(Seq(Model("a"), Model("b"))),
      Sink.Test { data =>
        ranVerify = true
        val _ = assert(data == Seq(Model("a")))
      }
    ))
    assert(ranVerify)
  }

  test("checkpoint") {
    var ranVerify1 = false
    var ranVerify2 = false
    testJob(CheckpointJob.Args(
      Source.Test(Seq("a", "b")),
      CheckpointArg.Test { data =>
        ranVerify1 = true
        val _ = assert(data == Seq("a: before checkpoint", "b: before checkpoint"))
      },
      Sink.Test { data =>
        ranVerify2 = true
        val _ = assert(data == Seq("a: after checkpoint", "b: after checkpoint"))
      }
    ))
    assert(ranVerify1)
    assert(ranVerify2)
  }

  test("checkpoint twice") {
    var ranCheckpointVerify = 0
    testJob(CheckpointTwiceJob.Args(
      Source.Test(Seq("a", "b")),
      CheckpointArg.Test { _ => ranCheckpointVerify += 1 },
      Sink.Test { data => assert(data == Seq("a: finally", "b: finally")) }
    ))
    assert(ranCheckpointVerify == 2)
  }

  test("checkpoint reuse") {
    var ranCheckpointVerify = 0
    testJob(CheckpointReuseJob.Args(
      Source.Test(Seq("a", "b")),
      CheckpointArg.Test { _ => ranCheckpointVerify += 1 },
      Sink.Test { data => assert(data == Seq("a", "b")) },
      Sink.Test { data => assert(data == Seq("a", "b")) }
    ))
    assert(ranCheckpointVerify == 1)
  }

  test("support reading any partitions from a unfixed source") {
    testJob(JobPartitionedRead.Args(
      Source.Test(Seq(Model("a"), Model("b"))),
      Sink.Test { data => assert(data == Seq(Model("a1"), Model("b1"), Model("a2"), Model("b2"))) }
    ))
  }

  test("support reading from different partitions") {
    testJob(JobPartitionedRead.Args(
      Source.Test(PartitionValue(1) -> Seq(Model("a"), Model("b")), PartitionValue(2) -> Seq(Model("c"))),
      Sink.Test { data => assert(data == Seq(Model("a1"), Model("b1"), Model("c2"))) }
    ))
  }

  test("support unfixed reading from different partitions") {
    testJob(JobPartitionedReadUnfixed.Args(
      Source.Test(PartitionValue(1) -> Seq(Model("a"), Model("b")), PartitionValue(2) -> Seq(Model("c"))),
      Sink.Test { data => assert(data == Seq(Model("a1"), Model("b1"), Model("c2"))) }
    ))
  }

  test("support verifying different partitions independently") {
    testJob(JobPartitionedWrite.Args(
      Source.Test(Seq(Model("a"), Model("b"))),
      Sink.Test(
        PartitionValue(1) -> { data => assert(data == Seq(Model("a1"), Model("b1"))) },
        PartitionValue(2) -> { data => assert(data == Seq(Model("a2"), Model("b2"))) }
      )
    ))
  }

  test("throw on writing to a partition without a specified verify") {
    val e = intercept[RuntimeException] {
      testJob(JobPartitionedWrite.Args(
        Source.Test(Seq(Model("a"), Model("b"))),
        Sink.Test(
          PartitionValue(1) -> { data => assert(data == Seq(Model("a1"), Model("b1"))) },
          PartitionValue(3) -> { data => assert(data == Seq(Model("a2"), Model("b2"))) }
        )
      ))
    }
    assert(e.getMessage().contains("Missing verifier"))
  }

  test("support verifying different partitions using a single verify") {
    testJob(JobPartitionedWrite.Args(
      Source.Test(Seq(Model("a"), Model("b"))),
      Sink.Test { data => assert(data.map(_.name.head) == Seq('a', 'b')) }
    ))
  }

  test("throw when reading from unspecified partition") {
    val e = intercept[RuntimeException] {
      testJob(JobPartitionedRead.Args(
        Source.Test(PartitionValue(1) -> Seq(Model("a"), Model("b"))),
        Sink.Test(_ => ())
      ))
    }
    assert(e.getMessage().contains("does not match any of the specified partitions"))
  }

  test("throw exception on verify failure") {
    assertThrows[AssertionError] {
      testJob(Job.Args(
        Source.Test(Seq(Model("b"))),
        Sink.Test { data => val _ = Predef.assert(data == Seq(Model("a"))) }
      ))
    }
  }
}
