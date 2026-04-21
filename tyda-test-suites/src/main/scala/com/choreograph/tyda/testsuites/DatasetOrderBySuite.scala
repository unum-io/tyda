package com.choreograph.tyda.testsuites

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.testsuites.FloatingPointEquality.given

object DatasetOrderBySuite {
  final case class Inner(x: Int, y: String) derives Arbitrary, Codec
  final case class Outer(a: Inner, b: Int) derives Arbitrary, Codec
  final case class AllNullable(a: Option[Int], b: Option[String]) derives Arbitrary, Codec
}

trait DatasetOrderBySuite extends DatasetSuite {
  import DatasetOrderBySuite.*

  testOrdered[Int, Int]("orderBy int", _.orderBy(identity))
  testOrdered[Long, Long]("orderBy long", _.orderBy(identity))
  testOrdered[Short, Short]("orderBy short", _.orderBy(identity))
  testOrdered[Byte, Byte]("orderBy byte", _.orderBy(identity))
  testOrdered[Boolean, Boolean]("orderBy boolean", _.orderBy(identity))
  testOrdered[Float, Float]("orderBy float", _.orderBy(identity))
  testOrdered[Double, Double]("orderBy double", _.orderBy(identity))
  testOrdered[String, String]("orderBy string", _.orderBy(identity))
  testOrdered[Date, Date]("orderBy date", _.orderBy(identity))
  testOrdered[Timestamp, Timestamp]("orderBy timestamp", _.orderBy(identity))
  testOrdered[Decimal[38, 9], Decimal[38, 9]]("orderBy decimal", _.orderBy(identity))

  testOrdered[Option[Int], Option[Int]]("orderBy Option[Int]", _.orderBy(identity))
  testOrdered[Option[Double], Option[Double]](
    "orderBy Option[Double]",
    _.orderBy(identity),
    Seq(
      None,
      Some(Double.NaN),
      Some(Double.PositiveInfinity),
      Some(Double.NegativeInfinity),
      Some(-0.0),
      Some(0.0)
    )
  )
  testOrdered[Option[Inner], Option[Inner]]("orderBy nested nullable struct", _.orderBy(identity))
  testOrdered[Option[AllNullable], Option[AllNullable]](
    "orderBy nullable struct with only null fields",
    _.orderBy(identity)
  )
  testOrdered[Option[Option[Int]], Option[Option[Int]]]("orderBy Option[Option[Int]]", _.orderBy(identity))

  testOrdered[Inner, Inner]("orderBy nested struct", _.orderBy(identity))
  testOrdered[Outer, Outer]("orderBy deeply nested struct", _.orderBy(identity))
  testOrdered[Outer, Outer]("orderBy nested struct inner field", _.orderBy(_.a))
  testOrdered[(Inner, Int), (Inner, Int)]("orderBy tuple with struct", _.orderBy(_._1))

  testOrdered[(Int, Int), (Int, Int)]("orderBy 2 keys", _.orderBy(_._1, _._2))
  testOrdered[(Int, Int, Int), (Int, Int, Int)]("orderBy 3 keys", _.orderBy(_._1, _._2, _._3))
  testOrdered[(Int, Int, Int, Int), (Int, Int, Int, Int)]("orderBy 4 keys", _.orderBy(_._1, _._2, _._3, _._4))
  testOrdered[(Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int)](
    "orderBy 5 keys",
    _.orderBy(_._1, _._2, _._3, _._4, _._5)
  )
  testOrdered[(Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int)](
    "orderBy 6 keys",
    _.orderBy(_._1, _._2, _._3, _._4, _._5, _._6)
  )
  testOrdered[(Int, Int, Int, Int, Int, Int, Int), (Int, Int, Int, Int, Int, Int, Int)](
    "orderBy 7 keys",
    _.orderBy(_._1, _._2, _._3, _._4, _._5, _._6, _._7)
  )

  testOrdered[Int, Int]("filter then orderBy", _.where(_ > 0).orderBy(identity))
  testOrdered[Int, Int]("orderBy then filter", _.orderBy(identity).where(_ > 0))
  testOrdered[Int, Int]("orderBy then limit", _.orderBy(identity).limit(3))
  testOrdered[(Int, Int), Int]("orderBy then select", _.orderBy(_._1).select(_._2))
  testOrdered[(Int, Long), (Int, Long)](
    "groupBy aggregate then orderBy",
    ds => ds.groupByKey(_._1).aggregateValue(count).pairs.orderBy(_._1)
  )
  test[(Int, Long), (Int, Long)](
    "orderBy then groupBy aggregate",
    ds => ds.orderBy(_._1).groupByKey(_._1).aggregateValue(count).pairs
  )
  testOrdered[Int, Int, Int](
    "join then orderBy",
    (ds1, ds2) => ds1.join(ds2, _ == _).select(_._1).orderBy(identity)
  )
  testOrdered[Int, Int, Int](
    "orderBy then join",
    (ds1, ds2) => ds1.orderBy(identity).join(ds2, _ == _).select(_._1)
  )

  testOrdered[Int, Int]("distinct then orderBy", _.distinct.orderBy(identity))
  test[Int, Int]("orderBy then distinct", _.orderBy(identity).distinct)
}
