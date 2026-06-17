# Tyda

Tyda is a type-safe Dataset library for Scala 3. It provides a fully type-safe expression API that compiles to multiple execution engines — including Spark and an in-process engine for fast unit tests.

## Why Tyda?

Spark's column API loses type information at compile time, forcing you to cast results and discover errors at runtime. Tyda solves this with an expression API that is checked by the compiler, so missing columns and type mismatches are caught before the job runs.

```scala
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.functions.explode

case class Person(name: String, age: Int)
val persons: Dataset[Person] = Dataset.from(Seq(Person("Alice", 30), Person("Bob", 25)))

// Type-safe: the compiler knows this produces Dataset[String]
val names: Dataset[String] = persons.select(_.name)

// Joins use lambdas — no string column names
case class User(id: Long, name: String, age: Int, location: Option[String])
case class Event(id: Long, userId: Long, eventType: String)
val joined: Dataset[(User, Event)] =
  users.join(events, (u, e) => u.id == e.userId)

// Option is used for nullable values, not null
val withLocation: Dataset[(Long, String)] =
  users.select(_.id, explode(_.location))
```

## Key features

- **Type-safe expressions** — column access and transformations are checked at compile time
- **Multiple execution engines** — the same business logic runs on Spark, BigQuery, or in-process
- **Fast unit tests** — the in-process engine is ~100x faster than Spark for small datasets, turning minute-long test suites into seconds
- **Familiar API** — designed to feel like normal Scala: `map`, `filter`, `Option`, named tuples

## Modules

| Module | Description |
|---|---|
| `tyda` | Core API: `Dataset`, `Expr`, `Codec`, type-safe expressions |
| `tyda-iterator` | In-process execution engine |
| `tyda-spark` | Spark execution engine |
| `tyda-sql` | SQL unparser (BigQuery and Spark SQL dialects) |
| `tyda-sparksql` | Spark SQL execution via the SQL unparser |
| `tyda-big-query` | BigQuery execution engine |
| `tyda-json` | JSON serialisation for tyda types |
| `tyda-parquet` | Parquet reader/writer |
| `tyda-metadata` | Schema metadata extraction |
| `tyda-table` | Table abstraction with partitioning |
| `tyda-job` | Framework for writing data processing jobs with CLI argument parsing |
| `tyda-job-test` | Test utilities for jobs |
| `tyda-repl` | REPL support |
| `tyda-collection` | Collection utilities |
| `tyda-meta` | Metaprogramming utilities |
| `tyda-rewrite` | Shared logic for backends for rewriting Dataset and Expr trees |

## Getting started

Add the core module and an execution engine to your build:

```scala
libraryDependencies ++= Seq(
  "com.wppresolve.tyda" %% "tyda"          % "<version>",
  "com.wppresolve.tyda" %% "tyda-iterator" % "<version>",  // for unit tests
  "com.wppresolve.tyda" %% "tyda-spark"    % "<version>",  // for Spark
)
```

## Documentation

Full API documentation and guides are available at [unum-io.github.io/tyda](https://unum-io.github.io/tyda/docs/index.html)
