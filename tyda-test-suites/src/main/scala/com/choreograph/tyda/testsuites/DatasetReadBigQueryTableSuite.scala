package com.choreograph.tyda.testsuites

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.TableLocation
import com.choreograph.tyda.aggregates.count

object DatasetReadBigQueryTableSuite {
  final case class UtilityUs(region_code: Option[String])
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
}
