package com.choreograph.tyda.sql

import scala.reflect.ClassTag

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Format
import com.choreograph.tyda.aggregates.countIf
import com.choreograph.tyda.aggregates.min
import com.choreograph.tyda.functions.explode
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.functions.namedTuple
import com.choreograph.tyda.functions.some
import com.choreograph.tyda.functions.tuple

private object UnparserSuite {
  final case class O(a: Option[Option[Int]])
  final case class M1(a: Int, b: String, c: Boolean, d: Seq[Double])
  enum E1 {
    case A(a: Int)
    case C
  }
  enum E2 derives Codec.EnumAsString {
    case First
    case Second
  }
}

abstract class UnparserSuite extends SqlGoldenTestSuite {
  import UnparserSuite.*

  private val ds = Dataset.readTable[M1, EmptyTuple]("table1").select(_._1)
  private val ds2 = Dataset.readTable[M1, EmptyTuple]("table2").select(_._1)
  private val ds3 = Dataset.readTable[M1, EmptyTuple]("table3").select(_._1)
  private val ds4 = Dataset.readTable[O, EmptyTuple]("table4").select(_._1)

  testSql("select and where, with where first") { ds.where(_.a > 10).select(_.a) }
  testSql("select and where, with select first") { ds.select(_.a).where(_ > 10) }

  testSql("grouped aggregation + filter") {
    ds.groupByKey(_.b).aggregateValue(min(_.a)).pairs.where(_._2 < 10)
  }

  testSql("group by all") { ds4.groupByKey(_ => lit(None)).aggregateValue(countIf(!_.a.isEmpty)).values }

  testSql("explode multiple expression") { ds.select(_.a, explode(_.d)) }

  testSql("aggregate after grouped aggregate") {
    ds.groupByKey(_.b).aggregateValue(min(_.a)).pairs.aggregate(min(_._2))
  }

  testSql("groupped aggregate after grouped aggregate") {
    ds.groupByKey(_.b).aggregateValue(min(_.a)).pairs.groupByKey(_._2).aggregateValue(min(_._1)).values
  }

  testSql("distinct before select") { ds.select(_.a, _.b).distinct.select(_._1) }

  testSql("make struct") { ds.select(r => some(tuple(r.a, r.b, r.c))) }

  testSql("make named struct") { ds.select(r => some(namedTuple(a = r.a, b = r.b, c = r.c))) }

  testSql("make named struct from product") { ds.select(r => some(r.toNamedTuple)) }

  testSql("select distinct") { ds.select(_.a).distinct }

  testSql("multiple explodes in separate selects") {
    ds.select(identity, explode(_.d)).select(_._2, explode(_._1.d))
  }

  testSql("inner join select after") { ds.join(ds2, (l, r) => l.a == r.a).select(_._1.a, _._2.a) }

  testSql("inner join select before") { ds.select(_.a).join(ds2.select(_.a), (l, r) => l == r) }

  testSql("multiple joins") {
    ds.select(_.a)
      .join(ds2.select(_.a), (l, r) => l == r)
      .join(ds3.select(_.a), (l, r) => l._1 == r)
      .select(_._1._1, _._1._2, _._2)
  }

  testSql("left join product on rhs") { ds.select(_.a).leftOuterJoin(ds2, (l, r) => l == r.a) }

  testSql("left join product on lhs") {
    ds.leftOuterJoin(ds2.select(_.a), (l, r) => l.a == r).select(_._1.a, _._2)
  }

  testSql("full outer join product on lhs") { ds.fullOuterJoin(ds2.select(_.a), (l, r) => l.a == r) }

  testSql("join+distinct on rhs") { ds.select(_.a).join(ds2.select(_.a).distinct, (l, r) => l == r) }

  testSql("join+aggregate on lhs") {
    ds.groupByKey(_.b).aggregateValue(min(_.a)).values.join(ds2.select(_.a), (l, r) => l == r)
  }

  testSql("fromSeq") { Dataset.FromSeq(Seq(1, 2, 3)) }

  testSql("literal enum") { Dataset.FromSeq(Seq(E1.A(1)).map(Tuple1(_))).select(_._1 == E1.C) }

  testSql("literal singleton") { Dataset.FromSeq[E1.C.type](Seq(E1.C)) }

  testSql("literal enum as string") { Dataset.FromSeq(Seq(E2.First)).select(_ == E2.Second) }

  testSql("bytes literal") {
    Dataset.FromSeq(Seq(Binary.fromArray(Array(0x00, 0xca, 0xfe, 0xba, 0xbe).map(_.toByte))))
  }

  testSql("empty collection") { Dataset.FromSeq[M1](Seq()) }

  testSql("escape strings") { Dataset.FromSeq(Seq("O'R", "Line1\nLine2", "Tab\tCharacter")) }

  testSql("escape value columns") { Dataset.FromSeq[(from: Int, `has space`: Long)](Seq((1, 2L))) }

  testSql("escape identifiers") {
    Dataset.readTable[(`dashed-column`: Int), EmptyTuple]("table-name").select(_._1)
  }

  testSql("equals with Option") {
    Dataset.readTable[(c1: Option[Int]), EmptyTuple]("table1").select(_._1).select(_.c1 == None)
  }

  testSql("write parquet to path") { ds.writeToPath("/tmp/test-parquet", Format.Parquet) }

  testSql("write path with sql injection") {
    ds.writeToPath("/tmp/sql-injection-path'; DROP TABLE;", Format.Parquet)
  }
}
