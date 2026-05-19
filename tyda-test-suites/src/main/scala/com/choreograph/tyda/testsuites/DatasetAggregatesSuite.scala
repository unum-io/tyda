package com.choreograph.tyda.testsuites

import scala.compiletime.ops.int.-
import scala.compiletime.ops.int.>=

import org.scalactic.Equality
import org.scalactic.TolerantNumerics

import com.choreograph.tyda.AggregateExpr
import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Decimal.MaxPrecision
import com.choreograph.tyda.Expr
import com.choreograph.tyda.NumericLimits
import com.choreograph.tyda.SumMagnet
import com.choreograph.tyda.aggregates.boolAnd
import com.choreograph.tyda.aggregates.boolOr
import com.choreograph.tyda.aggregates.collect
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.aggregates.countIf
import com.choreograph.tyda.aggregates.countSome
import com.choreograph.tyda.aggregates.max
import com.choreograph.tyda.aggregates.maxBy
import com.choreograph.tyda.aggregates.min
import com.choreograph.tyda.aggregates.minBy
import com.choreograph.tyda.aggregates.reduce
import com.choreograph.tyda.aggregates.sum
import com.choreograph.tyda.functions.lit
import com.choreograph.tyda.functions.some
import com.choreograph.tyda.functions.tuple
import com.choreograph.tyda.functions.when

object DatasetAggregatesSuite {
  enum EnumWithOrdering {
    case A(a: Int)
    case B(b: Int)
    case C(c: Int)
  }
  object EnumWithOrdering {
    given Ordering[EnumWithOrdering] =
      Ordering.by {
        case EnumWithOrdering.A(a) => a
        case EnumWithOrdering.B(b) => b
        case EnumWithOrdering.C(c) => c
      }
  }

  final case class Inner(i: Option[Int] = None, a: Option[String] = None)
  final case class Outer(i: Option[Inner] = None, a: Option[Inner] = None)

  given smallFloat: Arbitrary[Float] = Arbitrary.between(0, 1)
  given smallDouble: Arbitrary[Double] = Arbitrary.between(0, 1)
}

/* Testsuite focusing on aggregates that will compare a Dataset backend to a reference implementation. */
trait DatasetAggregatesSuite extends DatasetSuite {
  import DatasetAggregatesSuite.{EnumWithOrdering, Inner, Outer}
  import DatasetSuite.{Pair, TinyByte}

  given Ordering[Pair] = Ordering.by(_._1)

  test[Int, Option[Long]]("aggregate count whole dataset", ds => ds.aggregate(count))
  test[Int, Option[Seq[Int]]]("aggregate collect whole dataset", ds => ds.aggregate(collect), Seq())
  test[Int, Option[Int]]("aggregate max whole dataset", ds => ds.aggregate(max))
  test[Int, Option[(Long, Seq[Int])]]("aggregate 2 whole dataset", ds => ds.aggregate(count, collect))
  test[Int, Option[(Long, Seq[Int], Int)]](
    "aggregate 3 whole dataset",
    ds => ds.aggregate(count, collect, min)
  )
  test[Int, Option[(Long, Seq[Int], Int, Int)]](
    "aggregate 4 whole dataset",
    ds => ds.aggregate(count, collect, min, max)
  )
  test[(Int, Int), Option[(Long, Seq[(Int, Int)], Int, Int, Int)]](
    "aggregate 5 whole dataset",
    ds => ds.aggregate(count, collect, min(_._1), max(_._1), min(_._2))
  )
  test[(Int, Int), Option[(Long, Seq[(Int, Int)], Int, Int, Int, Int)]](
    "aggregate 6 whole dataset",
    ds => ds.aggregate(count, collect, min(_._1), max(_._1), min(_._2), max(_._2))
  )
  test[(Int, Int), Option[(Long, Seq[(Int, Int)], Int, Int, Int, Int, Seq[Int])]](
    "aggregate 7 whole dataset",
    ds => ds.aggregate(count, collect, min(_._1), max(_._1), min(_._2), max(_._2), collect(_._2))
  )

  test[(Int, Int), Option[(Long, Seq[(Int, Int)], Int, Int, Int, Int, Seq[Int], Long)]](
    "aggregate 8 whole dataset",
    ds =>
      ds.aggregate(v =>
        (count(v), collect(v), min(v._1), max(v._1), min(v._2), max(v._2), collect(v._2), sum(v._1))
      )
  )

  test[Int, Seq[Int]]("collect whole row", ds => ds.groupByKey(_ => lit(1)).aggregateValue(collect).values)
  test[Option[Int], Seq[Option[Int]]](
    "collect Option",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(collect).values
  )
  test[Seq[Int], Seq[Seq[Int]]](
    "collect Seq",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(collect).values
  )
  test[Option[Seq[Int]], Seq[Option[Seq[Int]]]](
    "collect Option Seq",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(collect).values
  )
  test[Option[Int], Option[Option[Long]]]("aggregate nested Option", ds => ds.aggregate(sum))
  test[Pair, Seq[TinyByte]]("collect column", ds => ds.groupByKey(_._1).aggregateValue(collect(_._2)).values)
  test[Pair, Seq[Int]](
    "collect and map",
    ds => ds.groupByKey(_._1).aggregateValue(collect(_._2)).values.select(_.map(_.cast[Int] + 1))
  )
  test[Pair, (TinyByte, Seq[Int])](
    "collect and map pairs",
    ds => ds.groupByKey(_._1).aggregateValue(collect(_._2)).pairs.select(_._1, _._2.map(_.cast[Int] + 1))
  )

  test[Int, Int]("min whole row", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
  test[Int, Long]("count whole row", ds => ds.groupByKey(_ => lit(1)).aggregateValue(count).values)
  test[Boolean, Long]("countIf whole row", ds => ds.groupByKey(_ => lit(1)).aggregateValue(countIf).values)
  test[Int, Long]("countIf expr", ds => ds.groupByKey(_ => lit(1)).aggregateValue(countIf(_ > 0)).values)
  test[(Int, Int), (Int, Long)](
    "countIf group by",
    ds => ds.groupByKey(_._1).aggregateValue(countIf(_._2 > 0)).pairs
  )
  test[Option[Int], Long](
    "countSome whole row",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(countSome).values
  )
  test[(TinyByte, Option[Int]), Long](
    "countSome expr",
    ds => ds.groupByKey(_._1).aggregateValue(countSome(_._2)).values
  )
  test[Boolean, Option[Boolean]]("boolAnd whole row", ds => ds.aggregate(boolAnd))
  test[Boolean, Option[Boolean]]("boolOr whole row", ds => ds.aggregate(boolOr))
  test[(TinyByte, Int), Boolean](
    "boolAnd expr",
    ds => ds.groupByKey(_._1).aggregateValue(boolAnd(_._2 > 0)).values
  )
  test[(TinyByte, Int), Boolean](
    "boolOr expr",
    ds => ds.groupByKey(_._1).aggregateValue(boolOr(_._2 > 0)).values
  )
  test[Pair, Pair]("min expr", ds => ds.groupByKey(_._1).aggregateValue(min(_._2)).pairs)
  test[Boolean, Boolean]("min Boolean", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
  test[Byte, Byte]("min Byte", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
  test[Short, Short]("min Short", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
  test[Int, Int]("min Int", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
  test[String, String]("min String", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
  {
    import FloatingPointEquality.given
    test[Float, Float]("min Float", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
    test[Double, Double]("min Double", ds => ds.groupByKey(_ => lit(1)).aggregateValue(min).values)
    test[Double, Double](
      "max Double",
      ds => ds.groupByKey(_ => lit(1)).aggregateValue(max).values,
      Seq(1, Double.NaN)
    )
  }
  test[Pair, (TinyByte, Long)]("count group by", ds => ds.groupByKey(_._1).aggregateValue(count).pairs)
  test[(Int, Int), (Boolean, Int)](
    "groupBy transform",
    ds => ds.groupByKey(_._1 > 2).aggregateValue(min(_._2)).pairs
  )

  test[Pair, Pair]("maxBy", ds => ds.groupByKey(_._1).aggregateValue(maxBy(_._1, _._2)).pairs)
  test[Pair, Pair]("minBy", ds => ds.groupByKey(_._1).aggregateValue(minBy(_._1, _._2)).pairs)
  test[Pair, (key: TinyByte, min: TinyByte, max: TinyByte)](
    "aggregate minBy and maxBy",
    ds => ds.groupByKey(_._1).aggregate(v => (min = minBy(v._1, v._2), max = maxBy(v._1, v._2)))
  )

  test[Byte, Long]("sum Byte", ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values)
  test[Short, Long]("sum Short", ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values)
  test[Int, Long]("sum Int", ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values)
  {

    /** Generate Long values that are small enough to not cause overflow when
      * summed a resonable number of them.
      */
    given Arbitrary[Long] = Arbitrary.between(Int.MinValue, Int.MaxValue)
    test[Long, Long]("sum Long", ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values)
  }

  testFailure[Long, Option[Long]](
    "sum Long overflow should error",
    Seq(Long.MaxValue, 1L),
    ds => ds.aggregate(sum),
    "overflow"
  )

  {
    /* Summing floating point is sensitive to the order of the elements, since the order in not promised we
     * only check using small values and using some tolerance for equality. */
    import DatasetAggregatesSuite.{smallFloat, smallDouble}
    given Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-12)
    test[Float, Double]("sum Float", ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values)
    test[Double, Double]("sum Double", ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values)
  }
  test[Option[Int], Option[Long]](
    "sum Option[Int]",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values,
    Seq(None, None),
    Seq(None, Some(1)),
    Seq()
  )
  {
    given [S <: Int](using
        Decimal.Valid[MaxPrecision, S],
        MaxPrecision - S >= 19 =:= true
    ): Arbitrary[Decimal[MaxPrecision, S]] = Arbitrary.long.map(l => Decimal(l))
    test[Decimal[MaxPrecision, 2], Decimal[MaxPrecision, 2]](
      "sum Decimal(MaxPrecision, 2)",
      ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values
    )
    test[Decimal[MaxPrecision, 9], Decimal[MaxPrecision, 9]](
      "sum Decimal(MaxPrecision, 9)",
      ds => ds.groupByKey(_ => lit(1)).aggregateValue(sum).values
    )
  }
  testFailure[Decimal[MaxPrecision, 0], Option[Decimal[MaxPrecision, 0]]](
    "sum Decimal overflow should error",
    Seq(NumericLimits[Decimal[MaxPrecision, 0]].max, Decimal[MaxPrecision, 0](1)),
    ds => ds.aggregate(sum),
    "cannot be represented"
  )

  test[(Int, Int), (Int, Int)](
    "reduce aggregate",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(reduce((a, b) => (a._1 + b._1, a._2 + b._2))).values
  )
  {
    // Bug that causes segfautls/data corruption is Spark due to SPARK-52023
    import Ordering.Implicits.seqOrdering

    type V = (String, String, Seq[String])
    test[(TinyByte, Option[V]), (TinyByte, Option[V])](
      "reduce aggregate with Option return type",
      ds => ds.grouped.reduce((option1, option2) => option1.zip(option2).map(Ordering[V].max)).pairs
    )
  }
  test[(TinyByte, Int), (TinyByte, Int)](
    "GroupedDataset.reduce",
    ds => ds.groupByKey(_._1).selectValues(_._2).reduce(_ + _).pairs
  )
  test[(TinyByte, Int), (key: TinyByte, value: Int)](
    "Dataset.reduce",
    ds => ds.keyBy(_._1).selectValues(_._2).reduce(_ + _)
  )
  test[(Int, Int), (Int, Int)](
    "reduce single column",
    ds => ds.groupByKey(_ => lit(1)).aggregateValue(pair => reduce[Int](_ + _)(pair._2)).pairs
  )

  test[Int, (EnumWithOrdering, Long)](
    "support group by with enum",
    ds =>
      ds.groupByKey(i => Expr[EnumWithOrdering.A]((a = i)).asBase[EnumWithOrdering])
        .aggregateValue(count)
        .pairs
  )
  test[Int, (Option[Option[Int]], Long)](
    "support group by with nested option",
    ds => ds.groupByKey(i => Expr.some(Expr.some(i))).aggregateValue(count).pairs
  )
  test[Int, (Int, Long)](
    "support group by with select from literal",
    ds => ds.groupByKey(_ => lit((integer = 1)).integer).aggregateValue(count).pairs
  )
  test[Option[Int], (Outer, Long)](
    "support group with complex literals",
    ds =>
      ds.groupByKey(v => lit(Outer()).copy((i = some(lit(Inner()).copy((i = v))))))
        .aggregateValue(count)
        .pairs
  )
  test[Option[(Int, Int)], Long](
    "reduce group by option.map with struct",
    ds => ds.groupByKey(t => t.map(i => tuple((i._1, 1)))).aggregateValue(count).values
  )
  test[Option[(Int, Int)], Long](
    "reduce group by option.orElse with struct",
    ds => ds.groupByKey(t => t.orElse(some((0, 0)))).aggregateValue(count).values
  )
  test[Option[Int], Long](
    "reduce group by nested option",
    ds => ds.groupByKey(t => when(t.forall(_ > 0), t)).aggregateValue(count).values
  )

  test[Pair, (Pair, TinyByte)](
    "group by whole struct row",
    ds => ds.groupByKey(identity).aggregateValue(min(_._1)).pairs
  )
  test[EnumWithOrdering, EnumWithOrdering](
    "group by whole enum row",
    ds => ds.groupByKey(identity).aggregateValue(_ => AggregateExpr.lit(1)).keys
  )
  test[Option[Option[Int]], Option[Option[Int]]](
    "group by whole nested Option row",
    ds => ds.groupByKey(identity).aggregateValue(_ => AggregateExpr.lit(1)).keys
  )

  test[Int, Int](
    "udf on AggregateExpr",
    ds => ds.groupByKey(_ => 1).aggregateValue(r => min(r).udf(_ + 1)).values
  )
}
