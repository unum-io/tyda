package com.choreograph.tyda.spark

import java.util.regex.Matcher.quoteReplacement

import scala.collection.mutable

import org.apache.spark.sql.execution.ExtendedMode

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr.explode
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.aggregates.min
import com.choreograph.tyda.spark.CodecToEncoder.convert
import com.choreograph.tyda.testsuites.GoldenTestSuite

/** Golden explain plan tests for DatasetOnSpark.
  *
  * These test are mostly to ensure we generates plans that do not contains lots
  * of unnecessary operations or operations that spark cannot optimize away.
  */
class DatasetOnSparkGoldenExplainSuite extends GoldenTestSuite, SharedSparkSession {
  import DatasetOnSparkGoldenExplainSuite.{SeqAndInt, normalizeIds}

  override def goldenFileName = {
    val sparkMajor = spark.version.head
    s"${getClass.getSimpleName}${sparkMajor}.golden"
  }

  def testExplain(name: String)(ds: => Dataset[?]): Unit =
    goldenTest(name) { normalizeIds(DatasetOnSpark(ds).queryExecution.explainString(ExtendedMode)) }

  def createTable[T: Arbitrary: Codec](name: String): Dataset[T] = {
    spark.createDataset[T](Seq.fill(100)(Arbitrary())).createOrReplaceTempView(name)
    Dataset.readTable[EmptyTuple, T](name).select(_._2)
  }

  private val ds1 = createTable[(Int, String)]("golden_test_t1")
  private val ds2 = createTable[(Int, String)]("golden_test_t2")
  private val ds3 = createTable[(Int, String)]("golden_test_t3")
  private val ds4 = createTable[(integer: Int, string: String)]("golden_test_t4")
  private val ds5 = createTable[SeqAndInt]("golden_test_t5")
  private val ds6 = createTable[(opt1: Option[Int], opt2: Option[Int])]("golden_test_t6")

  testExplain("read table") { ds1 }

  testExplain("join") { ds1.join(ds2, _._1 == _._1) }

  testExplain("multiple joins") { ds1.join(ds2, _._1 == _._1).join(ds3, _._1._1 == _._1) }

  testExplain("union") { ds1.union(ds2) }

  testExplain("except") { ds1.except(ds2) }

  testExplain("aggregate") { ds1.groupByKey(_._1).aggregate(r => (count = count(), min = min(r._1))) }

  testExplain("project") { ds4.project[(string: String)] }

  testExplain("project in select") { ds4.select(_.project[(string: String)].string) }

  testExplain("select in select") { ds1.select(_.select[String]) }

  testExplain("use typed api for map and flatMap") { ds5.flatMap(_.seq).map(identity) }

  testExplain("explode option") { ds6.select(explode(_.opt1)) }

  testExplain("explode option multiple") { ds6.select(explode(_.opt1), explode(_.opt2)) }
}

object DatasetOnSparkGoldenExplainSuite {
  // We use a case class to get stable deserializer in the explain
  final case class SeqAndInt(seq: Seq[Int], integer: Int)

  /* The normalize logic is stolen directly from Spark golden suite
   * https://github.com/apache/spark/blob/c70c728eb1ea2e6c6aff29e84af89eba4be345dc/sql/core/src/test/scala/org/apache/spark/sql/PlanStabilitySuite.scala#L232
   *
   * And shamefully modified to also normalize MapPartition lambdas. */
  private val exprIdRegexp = "(?<prefix>(?<!id=)#)\\d+L?".r
  private val planIdRegex = "(?<prefix>(plan_id=|id=#))\\d+".r
  val mapPartitionLambdaRegex = "(\\$\\$Lambda\\$?[0-9]*/0x)[0-9a-f]+@[0-9a-f]+".r
  private def normalizeIds(plan: String): String = {
    val exprIdMap = new mutable.HashMap[String, String]()
    val exprIdNormalized = exprIdRegexp.replaceAllIn(
      plan,
      m => exprIdMap.getOrElseUpdate(m.toString(), s"${m.group("prefix")}${exprIdMap.size + 1}")
    )

    // Normalize the plan ids in Exchange and Subquery nodes.
    // See `Exchange.stringArgs` and `SubqueryExec.stringArgs`
    val planIdMap = new mutable.HashMap[String, String]()
    val planIdNormalized = planIdRegex.replaceAllIn(
      exprIdNormalized,
      m => planIdMap.getOrElseUpdate(s"$m", s"${m.group("prefix")}${planIdMap.size + 1}")
    )

    val lambdaIdMap = new mutable.HashMap[String, String]()
    mapPartitionLambdaRegex.replaceAllIn(
      planIdNormalized,
      m =>
        lambdaIdMap.getOrElseUpdate(m.toString(), quoteReplacement(s"$$$$Lambda/0x${lambdaIdMap.size + 1}"))
    )
  }
}
