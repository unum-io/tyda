package com.choreograph.tyda.testsuites

import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.testsuites.DatasetSuite.TinyByte

object DatasetJoinSuite {
  final case class M1(key: TinyByte, b: Int)
  final case class M2(m1Key: TinyByte, c: Int)

  type NullableM1 = NamedTuple.Map[NamedTuple.From[M1], Option]
  type NullableM2 = NamedTuple.Map[NamedTuple.From[M2], Option]
}

// Testsuite focusing on joins that will compare a Dataset backend to a reference implementation.
trait DatasetJoinSuite extends DatasetSuite {
  import DatasetJoinSuite.*
  import DatasetSuite.Pair

  test[TinyByte, TinyByte, (TinyByte, TinyByte)]("join", (ds1, ds2) => ds1.join(ds2, _ == _))
  test[TinyByte, TinyByte, (TinyByte, TinyByte)](
    "join filter on lhs",
    (ds1, ds2) => ds1.where(_ < TinyByte(0)).join(ds2, _ == _)
  )
  test[TinyByte, TinyByte, (TinyByte, TinyByte)](
    "join filter on rhs",
    (ds1, ds2) => ds1.join(ds2.where(_ < TinyByte(0)), _ == _)
  )
  test[TinyByte, TinyByte, (TinyByte, TinyByte)](
    "join filter on both sides",
    (ds1, ds2) => ds1.where(_ < TinyByte(0)).join(ds2.where(_ < TinyByte(5)), _ == _)
  )
  test[Pair, Pair, (Pair, Pair)](
    "join complex condition",
    (ds1, ds2) => ds1.join(ds2, (l, r) => (l._1 == r._1) && (r._2 == l._2 || l._2 == TinyByte(0)))
  )
  test[TinyByte, TinyByte, (TinyByte, TinyByte)](
    "join complex not equi condition",
    (ds1, ds2) => ds1.join(ds2, (l, r) => (l == r) || (l == TinyByte(0)))
  )
  test[TinyByte, TinyByte, (TinyByte, TinyByte)](
    "join condition reverse order of exprs",
    (ds1, ds2) => ds1.join(ds2, (l, r) => r == l)
  )
  test[Pair, Pair, (Pair, Pair)](
    "join multiple keys",
    (ds1, ds2) => ds1.join(ds2, (l, r) => (l._1 == r._1) && (l._2 == r._2))
  )
  test[TinyByte, TinyByte, (TinyByte, Option[TinyByte])](
    "leftOuterJoin",
    (ds1, ds2) => ds1.leftOuterJoin(ds2, _ == _)
  )
  test[TinyByte, Option[TinyByte], (TinyByte, Option[Option[TinyByte]])](
    "leftOuterJoin nested Option",
    (ds1, ds2) => ds1.leftOuterJoin(ds2, (l, r) => r.contains(l))
  )
  test[TinyByte, EmptyTuple.type, (TinyByte, Option[EmptyTuple.type])](
    "leftOuterJoin singleton",
    (ds1, ds2) => ds1.leftOuterJoin(ds2, (_, _) => true)
  )
  test[Pair, Pair, (Pair, Option[Pair])](
    "leftOuterJoin product",
    (ds1, ds2) => ds1.leftOuterJoin(ds2, _._1 == _._1)
  )
  test[TinyByte, TinyByte, (Option[TinyByte], TinyByte)](
    "rightOuterJoin",
    (ds1, ds2) => ds1.rightOuterJoin(ds2, _ == _)
  )
  test[Pair, Pair, (Option[Pair], Pair)](
    "rightOuterJoin product",
    (ds1, ds2) => ds1.rightOuterJoin(ds2, _._1 == _._1)
  )
  test[TinyByte, TinyByte, (Option[TinyByte], Option[TinyByte])](
    "fullOuterJoin",
    (ds1, ds2) => ds1.fullOuterJoin(ds2, _ == _)
  )
  test[Option[TinyByte], TinyByte, (Option[Option[TinyByte]], Option[TinyByte])](
    "fullOuterJoin nested Option one side",
    (ds1, ds2) => ds1.fullOuterJoin(ds2, _.contains(_))
  )
  test[Option[TinyByte], Option[TinyByte], (Option[Option[TinyByte]], Option[Option[TinyByte]])](
    "fullOuterJoin nested Option",
    (ds1, ds2) => ds1.fullOuterJoin(ds2, _ == _)
  )
  test[Pair, Pair, (Option[Pair], Option[Pair])](
    "fullOuterJoin product",
    (ds1, ds2) => ds1.fullOuterJoin(ds2, _._1 == _._1)
  )
  test[TinyByte, TinyByte, TinyByte]("leftAntiJoin", (ds1, ds2) => ds1.leftAntiJoin(ds2, _ == _))
  test[Pair, Pair, Pair]("leftAntiJoin product", (ds1, ds2) => ds1.leftAntiJoin(ds2, _._1 == _._1))
  test[TinyByte, TinyByte, (TinyByte, TinyByte)](
    "pair join",
    (ds1, ds2) => ds1.keyBy(identity).join(ds2.keyBy(identity)).values
  )
  test[TinyByte, TinyByte, (TinyByte, Option[TinyByte])](
    "pair leftOuterJoin",
    (ds1, ds2) => ds1.keyBy(identity).leftOuterJoin(ds2.keyBy(identity)).values
  )
  test[TinyByte, TinyByte, (Option[TinyByte], TinyByte)](
    "pair rightOuterJoin",
    (ds1, ds2) => ds1.keyBy(identity).rightOuterJoin(ds2.keyBy(identity)).values
  )
  test[TinyByte, TinyByte, (Option[TinyByte], Option[TinyByte])](
    "pair fullOuterJoin",
    (ds1, ds2) => ds1.keyBy(identity).fullOuterJoin(ds2.keyBy(identity)).values
  )
  test[TinyByte, TinyByte, TinyByte](
    "grouped leftAntiJoin",
    (ds1, ds2) => ds1.keyBy(identity).leftAntiJoin(ds2.keyBy(identity)).values
  )
  test[Pair, (Pair, Pair)]("self join", ds => ds.join(ds, _._1 == _._2))
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join multiple right",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) => ds1.join(ds2, _._1 == _._1).join(ds2, _._1._2 == _._2)
  )
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join mutiple left",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) => ds1.join(ds2, _._1 == _._1).join(ds1, _._1._2 == _._2)
  )
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join multiple right with select",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).join(ds2.select(identity), _._1._2 == _._2)
  )
  test[Pair, Pair, (Pair, Pair)](
    "self join mutiple left with select",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).select(_._1).join(ds1, _._2 == _._2)
  )
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join mutiple left with union",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).union(ds2.select(identity, identity)).join(ds1, _._1._2 == _._2)
  )
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join mutiple left with except",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).except(ds2.select(identity, identity)).join(ds1, _._1._2 == _._2)
  )

  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join mutiple left with distinct",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).distinct.join(ds1, _._1._2 == _._2)
  )
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join mutiple left with filter",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).filter(_ => true).join(ds1, _._1._2 == _._2)
  )
  test[Pair, Pair, ((Pair, Pair), Pair)](
    "self join mutiple left with aggregate",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) =>
      ds1
        .join(ds2, _._1 == _._1)
        .groupByKey(identity)
        .aggregateValue(_ => AggregateExpr.lit(1))
        .keys
        .join(ds1, _._1._2 == _._2)
  )
  test[Pair, Pair, (Pair, (Pair, Pair))](
    "self join mutiple nested left hand side",
    (ds1: Dataset[Pair], ds2: Dataset[Pair]) => ds1.join(ds2.join(ds1, _._2 == _._2), _._1 == _._1._2)
  )
  test[Pair, Pair, Pair, ((Pair, Pair), (Pair, Pair))](
    "self join indirect",
    (ds1: Dataset[Pair], ds2: Dataset[Pair], ds3: Dataset[Pair]) => {
      val left = ds1.join(ds2, _._1 == _._1)
      val right = ds1.join(ds3, _._1 == _._1)
      left.join(right, _._1._2 == _._1._2)
    }
  )
  test[Pair, Pair, Pair, (Pair, Pair)](
    "self leftAntiJoin after overlapping join",
    (ds1: Dataset[Pair], ds2: Dataset[Pair], ds3: Dataset[Pair]) =>
      ds1.join(ds2, _._1 == _._1).leftAntiJoin(ds1.join(ds3, _._1 == _._1), _._1._1 == _._1._1)
  )
  test[Pair, Pair]("self leftAntiJoin", ds => ds.leftAntiJoin(ds, _._1 == _._2))
  test[M1, M2, NamedTuple.Concat[NamedTuple.From[M1], NamedTuple.From[M2]]](
    "joinFlat",
    (ds1, ds2) => ds1.joinFlat(ds2, _.key == _.m1Key)
  )

  test[M1, M2, NamedTuple.Concat[NamedTuple.From[M1], NullableM2]](
    "leftJoinFlat",
    (ds1, ds2) => ds1.leftJoinFlat(ds2, _.key == _.m1Key)
  )

  test[M1, M2, NamedTuple.Concat[NullableM1, NamedTuple.From[M2]]](
    "rightJoinFlat",
    (ds1, ds2) => ds1.rightJoinFlat(ds2, _.key == _.m1Key)
  )

  test[M1, M2, NamedTuple.Concat[NullableM1, NullableM2]](
    "fullJoinFlat",
    (ds1, ds2) => ds1.fullJoinFlat(ds2, _.key == _.m1Key)
  )
}
