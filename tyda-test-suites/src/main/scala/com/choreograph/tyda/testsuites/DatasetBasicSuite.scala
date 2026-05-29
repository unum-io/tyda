package com.choreograph.tyda.testsuites

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.aggregates.sum
import com.choreograph.tyda.functions.explode

object DatasetBasicSuite {
  type WideTuple = (Int, Int, Int, Int, Int, Int, Int)
  final case class SimpleProduct(a: Int, b: String)
}

/* Testsuite focusing on select and filter that will compare a Dataset backend to a reference implementation. */
trait DatasetBasicSuite extends DatasetSuite {
  import DatasetBasicSuite.{WideTuple, SimpleProduct}
  import DatasetSuite.{MyEnum, TinyByte}

  test[Boolean, Boolean]("filter", _.filter(identity))
  test[Boolean, Boolean]("where", _.where(identity))
  test[Boolean, Boolean]("where not", _.where(!_))
  test[Boolean, Boolean]("where literal", _.where(_ => true))
  test[((Int, Int), Int), Int]("select nested access", _.select(_._1._1))
  test[Boolean, (Boolean, Int)]("select Product", _.select(v => (v, 1)))
  test[Boolean, Int]("select primitive", _.select(_ => 1))
  test[MyEnum, MyEnum]("select enum", _.select(identity))
  test[Boolean, Map[Int, Int]]("select complex", _.select(_ => Map.empty[Int, Int]))
  test[(Seq[Int], Seq[Int]), (Int, Int)](
    "select multiple explode seq",
    _.select(explode(_._1), explode(_._2))
  )
  test[(Option[Int], Option[Int]), (Int, Int)](
    "select multiple explode options",
    _.select(explode(_._1), explode(_._2))
  )
  test[Seq[Int], Int]("explode Seq", _.select(explode(identity)))
  test[Seq[Seq[Int]], Int]("explode nested Seq", _.select(explode(identity)).select(explode(identity)))
  test[Seq[Option[Seq[Int]]], Int](
    "explode Seq Option Seq",
    _.select(explode(identity)).select(explode(identity)).select(explode(identity))
  )
  test[(Seq[Seq[Int]], Seq[Seq[Int]]), (Seq[Int], Seq[Int])](
    "explode multiple nested Seq",
    _.select(explode(_._1), explode(_._2))
  )
  test[Map[Int, Int], (Int, Int)]("explode Map", _.select(explode(identity)))
  test[Option[Int], Int]("explode Option", _.select(explode(identity)))

  test[Int, Int]("mapPartitions", _.mapPartitions(_.map(_ + 1)))

  test[WideTuple, (Int, Int)]("select 2", _.select(_._1, _._2))
  test[WideTuple, (Int, Int, Int)]("select 3", _.select(_._1, _._2, _._3))
  test[WideTuple, (Int, Int, Int, Int)]("select 4", _.select(_._1, _._2, _._3, _._4))
  test[WideTuple, (Int, Int, Int, Int, Int)]("select 5", _.select(_._1, _._2, _._3, _._4, _._5))
  test[WideTuple, (Int, Int, Int, Int, Int, Int)]("select 6", _.select(_._1, _._2, _._3, _._4, _._5, _._6))
  test[WideTuple, (Int, Int, Int, Int, Int, Int, Int)](
    "select 7",
    _.select(_._1, _._2, _._3, _._4, _._5, _._6, _._7)
  )
  test[NamedTuple.From[SimpleProduct], SimpleProduct]("as", _.as)
  // First arg here should be Tuple.Map[WideTuple, Seq] but runs into
  // https://github.com/scala/scala3/issues/23195
  test[
    (Seq[Int], Seq[Int], Seq[Int], Seq[Int], Seq[Int], Seq[Int], Seq[Int]),
    (Int, Int, Int, Int, Int, Int, Int)
  ](
    "select 7 explode",
    _.select(
      explode(_._1),
      explode(_._2),
      explode(_._3),
      explode(_._4),
      explode(_._5),
      explode(_._6),
      explode(_._7)
    )
  )

  test[Seq[Int], Int]("flatMap Seq", _.flatMap(identity))
  test[Map[Int, Int], (Int, Int)]("flatMap Map", _.flatMap(identity))
  test[Option[Int], Int]("flatMap Option", _.flatMap(identity))

  test[(Int, Seq[Int]), (Int, Int)]("flatMapValues Seq", _.flatMapValues(identity))

  test[TinyByte, TinyByte]("distinct primitive", _.distinct)
  test[Option[TinyByte], Option[TinyByte]]("distinct Option", _.distinct)
  test[List[TinyByte], List[TinyByte]]("distinct List", _.distinct)

  test[Int, Int, Int]("union", (left, right) => left.union(right))
  test[(Int, Int), (Int, Int), (Int, Int)]("union on struct", (left, right) => left.union(right))
  test[Int, Int]("union with self", ds => ds.union(ds))

  test[Int, Int]("limit basic", _.limit(5))
  test[Int, Int]("limit zero", _.limit(0))
  test[(Int, Int), (Int, Int)]("limit after select", _.select(x => (x._1, x._2)).limit(3))
  test[Int, Int]("limit before filter", _.limit(5).where(_ > 2))
  test[Int, Int]("limit after filter", _.where(_ > 5).limit(3))
  test[(String, Seq[Int]), Int]("limit before explode", _.limit(5).select(explode(_._2)))
  test[(String, Seq[Int]), Int]("limit after explode", _.select(explode(_._2)).limit(5))
  test[(String, Int), (key: String, value: Long)](
    "limit before aggregate",
    _.limit(5).groupByKey(_._1).aggregateValue(sum(_._2))
  )
  test[(String, Int), (key: String, value: Long)](
    "limit after aggregate",
    _.groupByKey(_._1).aggregateValue(sum(_._2)).limit(5)
  )
  test[Int, Int, (Int, Int)]("limit before join", (left, right) => left.limit(5).join(right, _ == _))
  test[Int, Int, (Int, Int)]("limit after join", (left, right) => left.join(right, _ == _).limit(5))

}
