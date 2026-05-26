package com.choreograph.tyda.table

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.*

import com.choreograph.tyda.TypeName

object SourceSinkTraversalSpec {
  final case class Model1()
  final case class Model2()
  final case class Model3()
  final case class Model4()
  object Table1 extends PathTable[Model1, Partitioner.None] {
    def prefix: String = "table1"
  }
  object Table2 extends PathTable[Model2, Partitioner.None] {
    def prefix: String = "table2"
  }

  object Table3 extends PathTable[Model3, Partitioner.None] {
    def prefix: String = "table3"
  }

  object Table4 extends PathTable[Model4, Partitioner.None] {
    def prefix: String = "table4"
  }

  enum Sum derives SourceSinkTraversal {
    case T4Source(table: Table4.Source)
    case T4Sink(table: Table4.Sink)
    case None
  }

  final case class Args(
      int: Int,
      source: Table1.Source,
      somethingElse: String,
      sink: Table1.Sink,
      source2: Option[Table2.Source],
      sink2: Option[Table2.Sink],
      source3: Option[Table3.Source],
      sink3: List[Table3.Sink],
      source3List: List[Table3.Source],
      sum1: Sum,
      sum2: Sum
  )
}

class SourceSinkTraversalSpec extends AnyFunSuite {
  import SourceSinkTraversalSpec.*

  private val source = Table1.source("sourcePath")
  private val sink = Table1.sink("sinkPath")
  private val source2 = Table2.source("sourcePath2")
  private val sink2 = Table2.sink("sinkPath2")
  private val sink3 = Table3.sink("sinkPath3")
  private val source4 = Table4.source("sourcePath4")

  private val args = Args(
    1,
    source,
    "something",
    sink,
    Some(source2),
    Some(sink2),
    None,
    List(sink3),
    List.empty,
    Sum.T4Source(source4),
    Sum.None
  )

  test("extract sources") {
    val sources = SourceSinkTraversal[Args].sources(args)
    sources must contain theSameElementsAs
      (List(
        SourceAndModelName(source, TypeName[Model1]),
        SourceAndModelName(source2, TypeName[Model2]),
        SourceAndModelName(source4, TypeName[Model4])
      ))
  }

  test("extract sinks") {
    val sinks = SourceSinkTraversal[Args].sinks(args)
    sinks must contain theSameElementsAs
      (List(
        SinkAndModelName(sink, TypeName[Model1]),
        SinkAndModelName(sink2, TypeName[Model2]),
        SinkAndModelName(sink3, TypeName[Model3])
      ))
  }

  test("modify sources") {
    val argsWithModelNamesAsPath = SourceSinkTraversal[Args].mapSources {
      [m, p <: Partitioner] => (s: Source[m, p], name: TypeName[m]) =>
        s match {
          case path @ Source.Path(basePath = _) => path.copy(basePath = name.value)
          case other => other
        }
    }(args)
    val expected = args.copy(
      source = source.copy(basePath = TypeName[Model1].value),
      source2 = Some(source2.copy(basePath = TypeName[Model2].value)),
      sum1 = Sum.T4Source(source4.copy(basePath = TypeName[Model4].value))
    )
    assert(argsWithModelNamesAsPath == expected)
  }

  test("modify sinks") {
    val argsWithModelNamesAsPath = SourceSinkTraversal[Args].mapSinks {
      [m, p <: Partitioner] => (s: Sink[m, p], name: TypeName[m]) =>
        s match {
          case path @ Sink.Path(_, _) => path.copy(basePath = name.value)
          case other => other
        }
    }(args)
    val expected = args.copy(
      sink = sink.copy(basePath = TypeName[Model1].value),
      sink2 = Some(sink2.copy(basePath = TypeName[Model2].value)),
      sink3 = List(sink3.copy(basePath = TypeName[Model3].value))
    )
    assert(argsWithModelNamesAsPath == expected)
  }
}
