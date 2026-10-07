package com.choreograph.tyda.table

import org.scalatest.funsuite.AnyFunSuite

class SourceSpec extends AnyFunSuite {
  private final case class Model(f: String)
  private final case class Date(date: Int)

  test("Document source path is its uri") {
    val source: Source[Model, Partitioner.None] =
      Source.Document("graphdb://graphdb.example.com/repositories/my-repo")
    assert(source.path == "graphdb://graphdb.example.com/repositories/my-repo")
  }

  test("Document source can not be read through Tyda's Dataset API") {
    val source: Source[Model, Partitioner.None] =
      Source.Document("graphdb://graphdb.example.com/repositories/my-repo")
    intercept[UnsupportedOperationException](source.read)
  }

  test("Document source can not be read partitioned through Tyda's Dataset API") {
    val source: Source[Model, Partitioner.Hive[Date]] =
      Source.Document("graphdb://graphdb.example.com/repositories/my-repo")
    intercept[UnsupportedOperationException](source.asPartitionDataset(Partitioner.Hive.fromValue(Date(1))))
  }
}
