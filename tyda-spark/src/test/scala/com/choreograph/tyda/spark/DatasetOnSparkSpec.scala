package com.choreograph.tyda.spark
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.execution.SparkPlan
import org.apache.spark.sql.execution.adaptive.AdaptiveSparkPlanHelper
import org.apache.spark.sql.execution.columnar.InMemoryTableScanExec
import org.apache.spark.sql.execution.exchange.Exchange
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.Seconds
import org.scalatest.time.Span

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.aggregates.min

object DatasetOnSparkSpec extends AdaptiveSparkPlanHelper {
  // We override this method to also include the cached plans
  override protected def allChildren(p: SparkPlan): Seq[SparkPlan] = {
    p match {
      case e: InMemoryTableScanExec => Seq(e.relation.cachedPlan)
      case _ => super.allChildren(p)
    }
  }
  def count(ds: Dataset[?], p: SparkPlan => Boolean)(using SparkSession): Int =
    var result = 0
    foreach(DatasetOnSpark(ds).queryExecution.executedPlan)(p.andThen(if _ then result += 1))
    result

  def countExchanges[T](ds: Dataset[T])(using SparkSession): Int = count(ds, _.isInstanceOf[Exchange])

  def countCached[T](ds: Dataset[T])(using SparkSession): Int =
    count(ds, _.isInstanceOf[InMemoryTableScanExec])
}

class DatasetOnSparkSpec extends AnyFunSuite, SharedSparkSession, Eventually {
  import DatasetOnSparkSpec.{countExchanges, countCached}

  /** Inorder to prevent Spark Datasets from being unpersisted we need to keep a
    * reference to the correponding Dataset alive. This method just uses the
    * hashCode which seems to be enough to keep the reference alive until after
    * this funciton is called.
    *
    * Note: A smart enough optimizer is probably allowed to optimize this away,
    * so when that starts happening we need to do something with the compute
    * value here.
    */
  def preventGc(v: Any): Unit = v.hashCode(): Unit

  // Disable broadcast hash join to simplify exchange count
  test("Reuse partitioning when doing multiple joins on the same key") {
    withConf(("spark.sql.autoBroadcastJoinThreshold", "-1")) {
      val ds1 = Dataset.FromSeq(Seq((1, 2)))
      val ds2 = Dataset.FromSeq(Seq((1, 3)))
      val ds3 = Dataset.FromSeq(Seq((1, 4)))

      val joined = ds1.join(ds2, _._1 == _._1).join(ds3, _._1._1 == _._1)
      assert(countExchanges(joined) == 3, "Each dataset should only be shuffled once")
    }
  }

  test("Reuse partitioning in aggregate after join") {
    withConf(("spark.sql.autoBroadcastJoinThreshold", "-1")) {
      val ds1 = Dataset.FromSeq(Seq((1, 2)))
      val ds2 = Dataset.FromSeq(Seq((1, 2)))

      val aggregated = ds1.join(ds2, _._1 == _._1).groupByKey(_._1._1).aggregateValue(count).values
      assert(countExchanges(aggregated) == 2, "Each dataset should only be shuffled once")
    }
  }

  test("cache should work for simple case") {
    val ds1 = Dataset.FromSeq(Seq((1, 2)))
    val ds2 = Dataset.FromSeq(Seq((1, 2)))

    val joined = ds1.join(ds2, _._1 == _._1)
    val cached = joined.cache()
    val _ = DatasetOnSpark(cached)
    assert(countCached(joined) == 1, "The join should be cached")
    preventGc(cached)
  }

  test("cache should work for udaf aggregate") {
    val ds1 = Dataset.FromSeq(Seq((1, 2)))
    val ds2 = Dataset.FromSeq(Seq((1, 2)))

    val aggregation = ds1.join(ds2, _._1 == _._1).groupByKey(_._1._1).aggregateValue(min(_._1._2)).values
    assert(countCached(aggregation) == 0, "The aggregation should not be cached yet")
    val cached = aggregation.cache()
    val _ = DatasetOnSpark(cached)
    /* Without the extra select it seems like we get a cached Dataset physical plan that does not use the
     * cache. */
    assert(countCached(aggregation.select(identity)) == 1, "The aggregation should be cached")
    preventGc(cached)
  }

  test("cache should sit in the right place for udaf aggregate") {
    val ds1 = Dataset.FromSeq(Seq((1, 2, 10))).cache()
    val ds2 = Dataset.FromSeq(Seq((1, 2, 11)))

    val aggregation = ds1.join(ds2, _._1 == _._1).groupByKey(_._1._1).aggregateValue(min(_._1._2)).values
    val _ = DatasetOnSpark(aggregation)
    assert(countCached(ds1) == 1, "The ds1 should be cached")
    assert(countCached(aggregation) == 1, "The aggregation should have one cached stage")
    preventGc(ds1)
  }

  test("cache should work for udaf aggregate many times") {
    val ds1 = Dataset.FromSeq(Seq((1, 2)))
    val ds2 = Dataset.FromSeq(Seq((1, 3)))

    val cached = ds1.cache()
    val aggregate = cached
      .join(ds2, _._1 == _._1)
      .cache()
      .groupByKey(_._1._1)
      .aggregateValue(min(_._1._2))
      .values
    val _ = DatasetOnSpark(aggregate)
    assert(countCached(cached) == 1, "The reciped data should be cached")
    assert(countCached(aggregate) == 2, "There should be another cache on aggregate")
    preventGc(cached)
  }

  test("cached dataset are automatically cleaned up") {
    val ds1 = Dataset.FromSeq(Seq((1, 2, 3)))

    /* Without the extra select we return the same SparkDataset instance and that contains a cached plan. But
     * we need Spark to recompute the plan to see if the cache is still there. */
    def usesCache: Dataset[?] = ds1.select(identity)

    def unreferencedCached[T](ds: Dataset[T]) = {
      val cached = ds.cache()
      val _ = DatasetOnSpark(cached)
      assert(countCached(usesCache) == 1, "Dataset should be cached")
      preventGc(cached)
    }
    unreferencedCached(ds1)

    eventually(Timeout(Span(5, Seconds))) {
      System.gc()
      val _ = DatasetOnSpark(ds1)
      assert(countCached(usesCache) == 0, "The dataset should be uncached")
    }
  }
}
