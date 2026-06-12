package com.choreograph.tyda.table

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.FileMetadata
import com.choreograph.tyda.iterator.IteratorRunner

object LatestSpec {
  final case class Model(f: String)
  final case class Date(date: Int)
  object Table extends PathTable[Model, Partitioner.Hive[Date]] {
    def prefix = "latest"
  }

  final case class DateAndOther(date: Int, other: String)
  object TableWithOther extends PathTable[Model, Partitioner.Hive[DateAndOther]] {
    def prefix = "latest"
  }

  final case class ArgsWithSource(source: Table.Source)
  final case class ArgsWithLatestSource(source: Latest[Table.Source, Int])

  extension [T](ds: Dataset[T]) { def compute: Seq[T] = IteratorRunner.collect(ds) }
  extension [T](ds: Dataset.Single[T]) { def value: T = IteratorRunner.collectValue(ds) }
}

class LatestSpec extends AnyFunSuite {
  import LatestSpec.*
  private val without = ArgsWithSource(Source.Path("/tmp"))

  private def checkParseAndSerialize[T: ArgsParser](args: Seq[String], expected: T) = {
    val result = ArgsParser.parse[T](args)
    assert(result == Right(expected))
    assert(args == ArgsParser.serialize(expected))
  }

  test("Latest should have the same args a plain source") {
    checkParseAndSerialize(ArgsParser.serialize(without), ArgsWithLatestSource(Latest(without.source)))
  }

  test("support specifying an override date in args") {
    val i = Arbitrary[Int]()
    val args = ArgsParser.serialize(without) ++ Seq("--source-override-date", i.toString)
    checkParseAndSerialize(args, ArgsWithLatestSource(Latest(without.source, overrideDate = Some(i))))
  }

  test("support reading from fixed test source with metadata") {
    val source =
      Latest[Table.Source, Int](Source.Test(Seq(Model("a")), FileMetadata("/tmp/date=1/file.parquet")))
    assert(source.readLatest(2).compute == Seq(Model("a")))
  }

  test("support reading for each partition from fixed test source with metadata") {
    val source = Latest[TableWithOther.Source, Int](
      Source.Test(Seq(Model("a")), FileMetadata("/tmp/date=1/other=other/file.parquet"))
    )
    val ds = source.readLatestForEachPartition(2)
    assert(ds.compute == Seq(1 -> Model("a")))
  }

  test("support reading from partitioned test source") {
    val source = Latest[Table.Source, Int](
      Source.Test(Date(1) -> Seq(Model("a")), Date(2) -> Seq(Model("b")), Date(3) -> Seq(Model("c")))
    )
    assert(source.readLatest(2).compute == Seq(Model("b")))
    assert(source.readLatest(10).compute == Seq(Model("c")))
    val exception = intercept[RuntimeException](source.readLatest(-1).compute)
    assert(exception.getMessage.contains("No partitions found"))
  }

  test("support getting the latestDate") {
    val source = Latest[Table.Source, Int](
      Source.Test(Date(1) -> Seq(Model("a")), Date(2) -> Seq(Model("b")), Date(3) -> Seq(Model("c")))
    )
    assert(source.latestDate(0).value == None)
    assert(source.latestDate(2).value == Some(2))
    assert(source.latestDate(10).value == Some(3))
  }

  test("support reading for each partition from partitioned test source") {
    val source = Latest[TableWithOther.Source, Int](Source.Test(
      DateAndOther(1, "other1") -> Seq(Model("other1-1")),
      DateAndOther(2, "other1") -> Seq(Model("other1-2")),
      DateAndOther(2, "other2") -> Seq(Model("other2-2")),
      DateAndOther(2, "other3") -> Seq(Model("other3-2")),
      DateAndOther(3, "other3") -> Seq(Model("other3-3"))
    ))
    val ds2 = source.readLatestForEachPartition(2)
    assert(ds2.compute == Seq((2, Model("other3-2")), (2, Model("other2-2")), (2, Model("other1-2"))))
    val ds10 = source.readLatestForEachPartition(10)
    assert(ds10.compute == Seq((3, Model("other3-3")), (2, Model("other2-2")), (2, Model("other1-2"))))
    val exception = intercept[RuntimeException](source.readLatestForEachPartition(-1).compute)
    assert(exception.getMessage.contains("No partitions found"))
  }
}
