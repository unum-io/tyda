package com.choreograph.tyda.testsuites

import scala.NamedTuple.NamedTuple

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.TableLocation
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.aggregates.countIf

object DatasetReadBigQueryTableSuite {
  final case class UtilityUs(region_code: Option[String])
  final case class Extras(extra1: Option[Int])

  type UtilityUsWithExtras = NamedTuple.Concat[NamedTuple.From[UtilityUs], NamedTuple.From[Extras]]
}

abstract class DatasetReadBigQueryTableSuite extends DatasetSuite {
  import DatasetReadBigQueryTableSuite.*

  test("read bigquery table") {
    BigQueryIntegrationTestEnvVariables.skipIfProjectNotSet()

    val ds = Dataset
      .readTable[UtilityUs, EmptyTuple](
        "bigquery-public-data.utility_us.us_states_area",
        TableLocation.BigQuery
      )
      .aggregate(count)
    implementation.collect(ds) match {
      case Seq(Some(count)) => assert(count > 50)
      case Seq(None) => fail("Expected a count, but got None")
      case Seq() => fail("Expected a count, but got an empty sequence")
    }
  }

  test("read bigquery table with extra optional fields") {
    BigQueryIntegrationTestEnvVariables.skipIfProjectNotSet()

    val ds = Dataset
      .readTable[UtilityUsWithExtras, EmptyTuple](
        "bigquery-public-data.utility_us.us_states_area",
        TableLocation.BigQuery
      )
      .select(_._1)
      .aggregate(r => (count = count(r), extra = countIf(r.extra1.isEmpty)))
    implementation.collect(ds) match {
      case Seq(Some((total, extraNone))) => assert(total == extraNone)
      case Seq(None) => fail("Expected a count, but got None")
      case Seq() => fail("Expected a count, but got an empty sequence")
    }
  }
}
