package com.choreograph.tyda.spark
import scala.util.Random
import scala.util.Using

import org.apache.spark.sql.DataFrameWriter
import org.apache.spark.sql.SaveMode
import org.apache.spark.sql.SparkSession
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.TableLocation
import com.choreograph.tyda.spark.CodecToEncoder.convert

object ReadTablePartitionsSpec {
  final case class Model(column: String, value: String) derives Arbitrary, Codec
  final case class PartitionValue(p1: String, p2: String)
  type ModelAndPartitionValue = NamedTuple.Concat[NamedTuple.From[Model], NamedTuple.From[PartitionValue]]

  final case class TmpTable(identifier: String)
  object TmpTable {
    def apply[T: Codec](values: Seq[T], partitions: String*)(using spark: SparkSession): TmpTable = {
      val tableName = s"tmp_table_${Random.alphanumeric.take(10).mkString}"
      extension [T](w: DataFrameWriter[T])
        def partitionByIfNoneEmpty(partitions: Seq[String]): DataFrameWriter[T] =
          if (partitions.isEmpty) w else w.partitionBy(partitions*)
      spark
        .createDataset(values)
        .write
        .partitionByIfNoneEmpty(partitions)
        .mode(SaveMode.Overwrite)
        .saveAsTable(tableName)
      TmpTable(identifier = tableName)
    }

    given release(using spark: SparkSession): Using.Releasable[TmpTable] with {
      def release(table: TmpTable): Unit = spark.sql(s"DROP TABLE IF EXISTS ${table.identifier}"): Unit
    }
  }
}

class ReadTablePartitionsSpec extends AnyFunSuite, SharedSparkSession {
  import ReadTablePartitionsSpec.*
  val runner = new SparkRunner(using spark)

  test("read partition values from table") {
    val partitions = (0 until 3).map { i => PartitionValue(s"v$i", s"v$i") }
    val values = partitions.flatMap { p =>
      Seq.fill(10)(Arbitrary[Model]()).map(m => (column = m.column, value = m.value, p1 = p.p1, p2 = p.p2))
    }
    Using.resource(TmpTable(values, "p1", "p2")) { tmpTable =>
      val readPartitions =
        Dataset.readTablePartitions[PartitionValue](tmpTable.identifier, TableLocation.Native)
      val res = runner.collect(readPartitions)
      res should contain theSameElementsAs partitions
    }
  }
}
