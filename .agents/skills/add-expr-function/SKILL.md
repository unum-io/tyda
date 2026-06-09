---
name: add-expr-function
description: Guide for adding a new non-aggregate expression function to the tyda Expr API
---

# Adding a new function to the tyda Expr API

This skill guides you through every file that must be changed when adding a new
non-aggregate expression function to tyda.

---

## Overview

The Expr API spans six modules. A function addition always touches all of them:

| Module | Role |
|---|---|
| `tyda` | Public API (`ExprApi`) + AST (`ExprNode`) |
| `tyda-iterator` | In-process evaluation + `explain` pretty-printer |
| `tyda-spark` | Spark Column translation |
| `tyda-sql` | SQL unparser (BigQuery / Spark SQL dialects) |
| `tyda-test-suites` | Shared behaviour test case |
| `tyda-sql` (test resources) | Golden-file snapshots for both SQL dialects |

---

## Step 1 — Add the AST node in `ExprNode.scala`

File: `tyda/src/main/scala/com/choreograph/tyda/ExprNode.scala`

Add a `final case class` inside `private object ExprNode`. Place it near
semantically related nodes.

```scala
// Example: a node for a single-argument string function with no extra config
final case class MyFunc(string: ExprNode[String]) extends ExprNode[String] {
  override def codec: Codec[String] = Codec[String]
}

// Example: a node for a variadic function over a homogeneous list of inputs
final case class MyVariadic(strings: Seq[ExprNode[String]]) extends ExprNode[String] {
  override def codec: Codec[String] = Codec[String]
}

// Example: a node whose output type differs from its input type
final case class MyTransform[T](operand: ExprNode[T]) extends ExprNode[Int] {
  override def codec: Codec[Int] = Codec[Int]
}
```

Rules:
- The class must be `final case class`.
- `codec` must always return the correct `Codec[T]` for the **output** type.
- For nodes with a single typed input whose codec is reused as output, derive it
  from the input: `override def codec = operand.codec`.
- For nodes with a fixed output type (e.g. `String`, `Int`, `Boolean`) use
  `Codec[T]` directly.

---

## Step 2 — Expose the function in `ExprApi.scala`

File: `tyda/src/main/scala/com/choreograph/tyda/ExprApi.scala`

`ExprApi` is a trait parameterised over `Expr[_]`. Methods here are inherited
by both `Expr` and `AggregateExpr`. There are two placement options:

### A) Top-level `def` (free function, e.g. `concat`)

Use this when the function is not naturally a method on a specific `Expr` type.

```scala
/** One-line ScalaDoc explaining what the function does. */
def myFunc(s0: Expr[String], rest: Expr[String]*): Expr[String] =
  lift(ExprNode.MyVariadic((s0 +: rest).map(unlift).toSeq))
```

### B) `extension` method on a typed `Expr` (e.g. `string.trim()`)

Use this when the function is naturally a method on values of a specific type.

```scala
extension (string: Expr[String]) {

  /** One-line ScalaDoc. */
  def myMethod(): Expr[String] = lift(ExprNode.MyFunc(unlift(string)))
}
```

If there is already an `extension` block for that type, add the method inside
the existing block.

The function must also be accessible as a top-level import. Check
`tyda/src/main/scala/com/choreograph/tyda/functions.scala` — it re-exports
`Expr` as `functions`, so any `def` or `extension` added to `ExprApi` is
automatically available via `import com.choreograph.tyda.functions.*`.

---

## Step 3 — Add runtime evaluation in `ExprEvaluation.scala`

File: `tyda-iterator/src/main/scala/com/choreograph/tyda/iterator/ExprEvaluation.scala`

The `impl` function inside `lambdaN` is a large pattern match. Add a case for
the new node. Place it near semantically related cases.

```scala
// Single-input, output is same type
case ExprNode.MyFunc(string) => impl(string).andThen(s => /* transform s */)

// Multi-input (variadic)
case ExprNode.MyVariadic(strings) =>
  val evals = strings.map(impl(_))
  from => evals.map(_(from)).mkString
```

Tips:
- `impl(node)` returns a `From => T` function.
- Compose with `.andThen(...)` for simple transformations.
- For multi-input nodes, evaluate each child once (outside the lambda) and
  close over the resulting functions inside the returned lambda.

---

## Step 4 — Add a pretty-print case in `explain.scala`

File: `tyda-iterator/src/main/scala/com/choreograph/tyda/iterator/explain.scala`

Inside `explainLambdaBody`, the inner `body` function is a pattern match.
Add a case for the new node:

```scala
// Single-input
case ExprNode.MyFunc(string) => s"${body(string)}.myFunc()"

// Variadic
case ExprNode.MyVariadic(strings) => strings.map(body).mkString("myFunc(", ", ", ")")
```

The output is only used for human-readable debugging, so match the style of
surrounding cases.

---

## Step 5 — Add Spark translation in `ExprOnSpark.scala`

File: `tyda-spark/src/main/scala/com/choreograph/tyda/spark/ExprOnSpark.scala`

Inside the `convert` method, add a case near related translations:

```scala
// Map directly to a Spark built-in function
case ExprNode.MyFunc(string) => someSparkFunction(convert(string))

// Variadic — Spark's concat takes Column*
case ExprNode.MyVariadic(strings) => concat(strings.map(convert)*)
```

Spark column functions are imported at the top of the file via
`import org.apache.spark.sql.functions.*`.

---

## Step 6 — Add SQL unparsing in `ExprUnparser.scala`

File: `tyda-sql/src/main/scala/com/choreograph/tyda/sql/ExprUnparser.scala`

Inside `exprToSqlExpr`, the large pattern match on `ExprNode` cases, add:

```scala
// Single-input mapping to a SQL function
case ExprNode.MyFunc(string) =>
  inner(string).map(str => SqlExpr.Function("my_func", Seq(str)))

// Variadic — sequence converts Seq[Result[SqlExpr]] to Result[Seq[SqlExpr]]
case ExprNode.MyVariadic(strings) =>
  sequence(strings.map(inner)).map(parts => SqlExpr.Function("my_func", parts))
```

`inner` is the local recursive call. `sequence` is imported from
`com.choreograph.tyda.sql.Result.sequence`.

If the SQL representation differs between BigQuery and SparkSQL, use
`dialect` to branch:

```scala
case ExprNode.MyFunc(string) =>
  inner(string).map(str =>
    dialect.someDialectField match {
      case SomeSealedTrait.CaseA(name) => SqlExpr.Function(name, Seq(str))
      case SomeSealedTrait.CaseB(name) => SqlExpr.Function(name, Seq(str, ...))
    }
  )
```

---

## Step 7 — Add a behaviour test in `ExprEvaluationSuiteBase.scala`

File:
`tyda-test-suites/src/main/scala/com/choreograph/tyda/testsuites/ExprEvaluationSuiteBase.scala`

Add an import for the new function if needed (check existing imports at the
top of the file — they all come from `com.choreograph.tyda.functions.*`):

```scala
import com.choreograph.tyda.functions.myFunc
```

Then add a `testHasSameBehavior` call near related tests:

```scala
testHasSameBehavior[(String, String, String), String](
  "my func description",          // test name — used as key in golden files
  t => myFunc(t._1, t._2, t._3), // Expr expression under test
  t => t._1 + t._2 + t._3        // expected Scala equivalent
)
```

The type parameters are `[Input, Output]`. Use a tuple for multi-input
functions. The test name string is used verbatim as the key in the golden
files (Step 8), so choose it carefully — it must be unique and stable.

`testHasSameBehavior` runs the expression against random inputs using
`Arbitrary[Input]` and compares the result to the Scala reference function.
It also generates a SQL snapshot that is recorded in the golden files.

---

## Step 8 — Regenerate the golden files

The golden files are auto-generated — never edit them by hand. After
implementing Steps 1–7, regenerate them by running:

```bash
TYDA_GOLDEN_GENERATE_FILES=1 bloop test tydaSql --only '*Golden*'
```

This overwrites both golden files with the actual SQL output:

- `tyda-sql/src/test/resources/golden/ExprEvaluationBigQueryGoldenSuite.golden`
- `tyda-sql/src/test/resources/golden/ExprEvaluationSparkSqlGoldenSuite.golden`

Verify the diff looks reasonable (correct SQL for both dialects).

---

## Complete checklist

- [ ] `ExprNode.scala` — new `final case class` with correct `codec`
- [ ] `ExprApi.scala` — new `def` or `extension` method calling `lift`/`unlift`
- [ ] `ExprEvaluation.scala` — new `case` in `impl`'s pattern match
- [ ] `explain.scala` — new `case` in `body`'s pattern match
- [ ] `ExprOnSpark.scala` — new `case` in `convert`'s pattern match
- [ ] `ExprUnparser.scala` — new `case` in `exprToSqlExpr`'s pattern match
- [ ] `ExprEvaluationSuiteBase.scala` — new `testHasSameBehavior` call (+ import if needed)
- [ ] `ExprEvaluationBigQueryGoldenSuite.golden` — new golden entry in correct position
- [ ] `ExprEvaluationSparkSqlGoldenSuite.golden` — new golden entry in correct position

---

## Running the tests

```bash
# Run all tyda tests (iterator + spark + sql)
bloop test tydaIterator tydaSpark tydaSql

# Regenerate golden files and verify SQL output
TYDA_GOLDEN_GENERATE_FILES=1 bloop test tydaSql --only '*Golden*'

# Run a specific test by name
bloop test tydaIterator tydaSpark tydaSql -- -z 'concat strings'
```
