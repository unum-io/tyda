package com.choreograph.tyda.testsuites

import org.scalactic.Equality

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Orderable
import com.choreograph.tyda.SimpleTypeName
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.testsuites.FloatingPointEquality.given

object DatasetOrderBySuite {
  private final case class Inner(x: Int, y: String) derives Arbitrary, Codec
  private final case class Outer(a: Inner, b: Int) derives Arbitrary, Codec
  private final case class AllNullable(a: Option[Int], b: Option[String]) derives Arbitrary, Codec
}

trait DatasetOrderBySuite extends DatasetSuite {
  import DatasetOrderBySuite.*

  def testOrderBy[T: Arbitrary: Codec: Equality: SimpleTypeName: Orderable](inputs: Seq[T]*): Unit =
    testOrdered[T, T](s"orderBy ${SimpleTypeName.name}, input: ${inputs}", _.orderBy(identity), inputs*)

  testOrderBy[Int]()
  testOrderBy[Long]()
  testOrderBy[Short]()
  testOrderBy[Byte]()
  testOrderBy[Boolean]()
  testOrderBy[Float]()
  testOrderBy[Double]()
  testOrderBy[String]()
  testOrderBy[Date]()
  testOrderBy[Timestamp]()
  testOrderBy[Decimal[38, 9]]()
  testOrderBy[Option[Int]]()
  testOrderBy[Option[Double]](Seq(
    None,
    Some(Double.NaN),
    Some(Double.PositiveInfinity),
    Some(Double.NegativeInfinity),
    Some(-0.0),
    Some(0.0)
  ))
  testOrderBy[Option[Inner]]()
  testOrderBy[Option[AllNullable]]()
  testOrderBy[Option[Option[Int]]]()
  testOrderBy[Inner]()
  testOrderBy[Outer]()

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
