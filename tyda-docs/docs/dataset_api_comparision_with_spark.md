---
title: Dataset API Comparison with Spark
---

# Dataset API Comparison with Spark

This page aims to compare how code would look like using Tyda's Dataset API vs using Spark's Dataset API directly.

The code will use the following dataset in the examples
```scala mdoc:silent
import org.apache.spark.sql.{Dataset as SparkDataset, DataFrame, SparkSession}
val spark = SparkSession.builder().appName("tyda-docs").master("local").getOrCreate()
import spark.implicits._
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.spark.CodecToEncoder.convert

opaque type UserId = Long
object UserId {
    def apply(id: Long): UserId = id
    given Codec[UserId] = Codec[Long]
}

final case class Event(id: Long, userId: UserId, eventType: String)
val events = Seq(
  Event(1, UserId(1), "click"),
  Event(2, UserId(1), "view"),
  Event(3, UserId(2), "click")
)

final case class User(id: UserId, name: String, age: Int, location: Option[String])
val users = Seq(
  User(UserId(1), "Alice", 30, Some("NY")),
  User(UserId(2), "Bob", 25, None)
)

val sparkEvents = spark.createDataset(events)
val sparkUsers = spark.createDataset(users)

val tydaEvents = Dataset.from(events)
val tydaUsers = Dataset.from(users)
```

## Selection

Similar to Spark, Tyda supports selecting multiple columns at once.
```scala mdoc:silent
val sparkSelected: SparkDataset[(Int, String)] =
    sparkUsers.select($"age".as[Int], $"name".as[String])

val tydaSelected: Dataset[(Int, String)] =
    tydaUsers.select(_.age, _.name)
```

To do named selection in Tyda one would use a select with returning a named tuple.
```scala mdoc:silent
val sparkNamedSelection: DataFrame =
    sparkUsers.select($"id".as("userId"), $"name".as("userName"))

val tydaNamedSelection: Dataset[(userId: UserId, userName: String)] =
    tydaUsers.select(user => (userId = user.id, userName = user.name))
```

There's also a matcher syntax

```scala mdoc:silent
import com.choreograph.tyda.Expr
val tydaNamedSelection: Dataset[(userId: UserId, userName: String)] =
    tydaUsers.select({ case Expr(id = id, name = name) => (userId = id, userName = name)})
```


## Optional/Nullable values

In Tyda we assume all values are non-nullable by default and one should use `Option` to represent nullable values.
The tyda API is also strict about types so one cannot compare an `Option[T]` with `T` directly; instead one would use `contains` like in normal Scala code.
```scala mdoc:silent
val sparkNyUsers: SparkDataset[User] =
    sparkUsers.where($"location" === "NY")

val tydaNyUsers: Dataset[User] =
    tydaUsers.where(_.location.contains("NY"))
```

## Exploding optional values

Tyda similarly to Spark provides an `explode` function that can be used to flatten collections into rows.
But like in Scala `Option` is convertible into a collection of 0 or 1 elements, so one can also use `explode` to flatten optional values into rows.
This is convenient as it captures in the type system that the value can not be `null` after the operation.
```scala mdoc:silent
import com.choreograph.tyda.functions.explode

val sparkUsersWithLocation: DataFrame =
    sparkUsers.where($"location".isNotNull).select($"id", $"location")

val tydaUsersWithLocation: Dataset[(UserId, String)] =
    tydaUsers.select(_.id, explode(_.location))
```

## Joins

For simple inner joins the code is similar, but Tyda uses a lambda to express the join condition.
```scala mdoc:silent
val sparkJoined: SparkDataset[(User, Event)] =
    sparkUsers.joinWith(sparkEvents, sparkUsers("id") === sparkEvents("userId"))

val tydaJoined: Dataset[(User, Event)] =
    tydaUsers.join(tydaEvents, (u, e) => u.id == e.userId)
```
But for outer joins Tyda will use `Option` as part of the API while Spark would produce null values. So users using `Option` for nulls need to manually cast the result
```scala mdoc:silent
val sparkLeftJoined: SparkDataset[(User, Option[Event])] =
    sparkUsers.joinWith(sparkEvents, sparkUsers("id") === sparkEvents("userId"), "left_outer").as[(User, Option[Event])]

val tydaLeftJoined: Dataset[(User, Option[Event])] =
    tydaUsers.leftOuterJoin(tydaEvents, (u, e) => u.id == e.userId)
```


## Aggregations

For aggregations Tyda provides a typed API that is similar to Spark's aggregate functions.
```scala mdoc:silent
import org.apache.spark.sql.functions.count_if
import com.choreograph.tyda.aggregates.countIf

val sparkAgg: DataFrame =
    sparkEvents.groupBy($"userId").agg(count_if($"eventType" === "click"))

val tydaAgg: Dataset[(key: UserId, value: Long)] =
    tydaEvents.groupByKey(_.userId).aggregateValue(countIf(_.eventType == "click"))
```

## Higher order functions

The Tyda API is meant to be very familiar to a Scala developer, so for transforming a `Expr[Seq[Int]]` one would just call `map` on it and provide another `Expr` lambda.
```scala mdoc:silent
import org.apache.spark.sql.functions.transform

val seqs = Seq(Seq(1, 2, 3), Seq(4, 5, 6))
val sparkSeq = spark.createDataset(seqs)
val tydaSeq = Dataset.from(seqs)

val sparkTransformed: DataFrame =
    sparkSeq.select(transform($"value", x => x + 1))
val tydaTransformed: Dataset[Seq[Int]] =
    tydaSeq.select(_.map(_ + 1))
```


## Generic code

Tyda also provides code that makes it possible to write generic code that works with any `T` that has certain fields.
```scala mdoc:silent
import com.choreograph.tyda.Projector

val ageThreshold = 18

// T must have an "age" field of type Int
def filterByAge[T](ds: SparkDataset[T]): SparkDataset[T] =
    ds.where($"age" > ageThreshold)

def filterByAge[T: Projector.To[(age: Int)]](ds: Dataset[T]): Dataset[T] =
    ds.where(_.project[(age: Int)].age > ageThreshold)
```
