package com.choreograph.tyda.testsuites

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr.explode
import com.choreograph.tyda.aggregates.max
import com.choreograph.tyda.testsuites.DatasetSuite.TinyByte

trait DatasetSubquerySuite extends DatasetSuite {
  test[Long, Long]("scalar subquery", ds1 => ds1.where(v => ds1.aggregate(max).value.contains(v)))

  test[Long, Boolean]("exists subquery", ds1 => ds1.select(_ => ds1.where(_ > 0L).exists))

  test[Long, Long]("exists subquery in where", ds1 => ds1.where(_ => ds1.where(_ > 0L).exists))

  test[TinyByte, TinyByte, TinyByte](
    "collect subquery",
    (ds1, ds2) => ds1.where(v => ds2.collect.exists(_ == v))
  )

  test[Long, Long, Long]("count subquery", (ds1, ds2) => ds1.where(_ > ds2.count))

  test[Seq[Long], Seq[Long], (Long, Long)](
    "explode subquery",
    (ds1, ds2) => ds1.select(x => (explode(x), ds2.select(explode).count))
  )

}
