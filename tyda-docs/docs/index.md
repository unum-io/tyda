# Tyda

Tyda is a type-safe Dataset library for Scala 3.

## Why Tyda?

The main goal of Tyda is to supply a type-safe independent API that supports multiple execution engines. The main advantages of using tyda.Dataset over Spark's Dataset directly are:

- Fully type safe expression API, no runtime errors from missing columns or incorrect types.
- Support for multiple execution engines, not just Spark.
- Fast unit tests, Tyda provides a simple memory execution engine that is much faster than Spark for small datasets.

### Performance vs Type Safety in Spark

Consider the following Spark code:
```scala mdoc:compile-only
import org.apache.spark.sql.{Dataset as SparkDataset, SparkSession}
val spark = SparkSession.builder().appName("tyda-docs").master("local").getOrCreate()
import spark.implicits._
// To use Spark with Scala3 we need to provide custom Encoders
import com.choreograph.tyda.spark.CodecToEncoder.convert

case class Person(name: String, age: Int)
val persons = spark.createDataset(Seq(Person("Alice", 30), Person("Bob", 25)))

val names1: SparkDataset[String] = persons.map(_.name)

val names2: SparkDataset[String] = persons.select($"name").as[String]
```
It shows 2 ways how to extract the names from the persons dataset.
The first one uses the type-safe API, which takes a normal Scala function and will be type checked by the compiler.
The second one is using the column API, which will allow Spark to optimize the query and perform things like column pruning.
But the downside is that the type information is lost and we need to explicitly cast the result to the expected type.

Tyda aims to solve this by providing a column/expr API that is fully type-safe.
This means there is no longer a trade-off between performance and type-safety; you get both.
In Tyda the above selection would be written as:
```scala mdoc:reset:silent
import com.choreograph.tyda.Dataset

case class Person(name: String, age: Int)
val persons: Dataset[Person] = Dataset.from(Seq(Person("Alice", 30), Person("Bob", 25)))
val names: Dataset[String] = persons.select(_.name)
```

### API independent of execution engine

The implementation of Tyda includes its own logical plan representation.
This is then translated to the desired execution engine, for example Spark.
This means that the business logic is independent of the execution engine and can be easily switched to another engine when something better comes along.

### Fast unit tests

Because Spark is optimized for large datasets, a slow planner/optimizer is not a big deal.
But for tiny datasets, like the ones used in unit tests, the planning time becomes a huge bottleneck and it is easy to end up with tests taking over a second to run.
Tyda solves this by allowing tests to be run using a simple in-memory execution engine.
In our experience this can speed up unit tests by 100x, so that suites run in seconds instead of minutes.

## How it is implemented

In order to get type safe field access on `Expr[T]` Tyda leverages the [computed field names](https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html#computed-field-names) feature that was added in Scala 3. Most other API is provided using extension methods on `Expr[T]`.
