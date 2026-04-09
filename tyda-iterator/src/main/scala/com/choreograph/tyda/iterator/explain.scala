package com.choreograph.tyda.iterator

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.PrimitiveAggregate
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances

/** Create a human-readable explaination of logial plan in a dataset.
  *
  * This is mainly intended for debugging purposes, to understand how
  * computation is structured and comparing with the explain output of other
  * query engines.
  */
private[tyda] def explain[T](ds: Dataset[T] | Dataset.Action): String = explain(ds, 0)

private def explain[T](ds: Dataset[T] | Dataset.Action, indent: Int): String = {
  val description = ds match {
    case simple @ (_: (Dataset.Read[?] & Product) | _: Dataset.ReadWithMetadata[?]) =>
      val options = simple
        .productElementNames
        .zip(simple.productIterator)
        .filter(_._1 != "codec") // toString of codec is currently very verbose
        .map { case (name, value) => s"$name=${value.toString}" }
        .mkString(", ")
      val name = simple.getClass.getSimpleName
      s"$name: $options"
    case Dataset.FromSeq(values, _) => s"FromSeq: ${values.toString()}"
    case other =>
      val childExplain = children(other).map(child => explain(child, indent + 1)).mkString("\n")
      val exprsExplain = exprs(other).map(explain).mkString(", ")
      val name = other.getClass.getSimpleName
      s"$name: ${exprsExplain}\n${childExplain}"
  }
  val prefix = "    " * indent
  s"${prefix}${description}"
}

private def children[T](ds: Dataset[T] | Dataset.Action): Seq[Dataset[?]] =
  ds match {
    case Dataset.Aggregate(input, _) => Seq(input)
    case Dataset.Cache(input) => Seq(input)
    case Dataset.Distinct(input) => Seq(input)
    case Dataset.Filter(input, _) => Seq(input)
    case Dataset.FromSeq(_, _) => Seq.empty
    case Dataset.FullOuterJoin(left, right, _) => Seq(left, right)
    case Dataset.GroupedAggregate(input, _, _) => Seq(input)
    case Dataset.Join(left, right, _) => Seq(left, right)
    case Dataset.LeftOuterJoin(left, right, _) => Seq(left, right)
    case Dataset.LeftAntiJoin(left, right, _) => Seq(left, right)
    case Dataset.MapPartitions(input, _, _) => Seq(input)
    case Dataset.ReadPath(path = _) => Seq.empty
    case Dataset.ReadTable(identifier = _) => Seq.empty
    case Dataset.ReadWithMetadata(read) => Seq(read)
    case Dataset.ReadPathWithHivePartitions(basePath = _) => Seq.empty
    case Dataset.ReadPartitionsPaths(path = _) => Seq.empty
    case Dataset.ReadTablePartitionsPaths(identifier = _) => Seq.empty
    case Dataset.Select1(input, _) => Seq(input)
    case Dataset.SelectN(input, _) => Seq(input)
    case Dataset.Union(left, right) => Seq(left, right)
    case Dataset.Action.Write(input, _, _) => Seq(input)
    case Dataset.Limit(input, _) => Seq(input)
  }

private type AnyCompiledExpr = CompiledExpr[?, ?] | CompiledExpr2[?, ?, ?] | CompiledExplodeExpr[?, ?] |
  CompiledAggregateExpr[?, ?]

private def exprs[T](ds: Dataset[T] | Dataset.Action): Seq[AnyCompiledExpr] =
  ds match {
    case Dataset.Aggregate(_, aggregateExpr) => Seq(aggregateExpr)
    case Dataset.Cache(_) => Seq.empty
    case Dataset.Distinct(_) => Seq.empty
    case Dataset.Filter(_, predicate) => Seq(predicate)
    case Dataset.FromSeq(_, _) => Seq.empty
    case Dataset.FullOuterJoin(_, _, predicate) => Seq(predicate)
    case Dataset.GroupedAggregate(_, keyExpr, aggregateExpr) => Seq(keyExpr, aggregateExpr)
    case Dataset.Join(_, _, predicate) => Seq(predicate)
    case Dataset.LeftOuterJoin(_, _, predicate) => Seq(predicate)
    case Dataset.LeftAntiJoin(_, _, predicate) => Seq(predicate)
    case Dataset.MapPartitions(_, _, _) => Seq()
    case Dataset.ReadPath(path = _) => Seq.empty
    case Dataset.ReadTable(identifier = _) => Seq.empty
    case Dataset.ReadWithMetadata(_) => Seq.empty
    case Dataset.ReadPathWithHivePartitions(basePath = _) => Seq.empty
    case Dataset.ReadPartitionsPaths(path = _) => Seq.empty
    case Dataset.ReadTablePartitionsPaths(identifier = _) => Seq.empty
    case Dataset.Select1(_, compiled) => Seq(compiled)
    case Dataset.SelectN(_, exprs) => tupleInstances(exprs).mapConst([t] => identity(_))
    case Dataset.Union(_, _) => Seq.empty
    case Dataset.Action.Write(_, _, _) => Seq.empty
    case Dataset.Limit(_, _) => Seq.empty
  }

private[tyda] def explain(anyCompiled: AnyCompiledExpr): String =
  anyCompiled match {
    case compiled: CompiledExpr[?, ?] => explainLambda(compiled.expr, Map(compiled.arg -> "x"))
    case compiled: CompiledExpr2[?, ?, ?] =>
      explainLambda(compiled.expr, Map(compiled.arg1 -> "l", compiled.arg2 -> "r"))
    case compiled: CompiledExplodeExpr[?, ?] =>
      val inner = explain(compiled.asCompiledExpr)
      s"explode($inner)"
    case compiled: CompiledAggregateExpr[?, ?] => explainLambda(compiled.expr, Map(compiled.arg -> "x"))
  }

private def explainPrimitiveAggregate(primitive: PrimitiveAggregate[?, ?], arg: String): String =
  primitive match {
    case PrimitiveAggregate.Collect() => "collect($arg)"
    case PrimitiveAggregate.Count() => "count($arg)"
    case PrimitiveAggregate.CountSome() => "countSome($arg)"
    case PrimitiveAggregate.BoolAnd() => "boolAnd($arg)"
    case PrimitiveAggregate.BoolOr() => "boolOr($arg)"
    case PrimitiveAggregate.Min(ord) => s"min($arg)(using $ord)"
    case PrimitiveAggregate.Max(ord) => s"max($arg)(using $ord)"
    case PrimitiveAggregate.MinBy(ord) => s"minBy($arg)(using ${ord})"
    case PrimitiveAggregate.MaxBy(ord) => s"maxBy($arg)(using ${ord})"
    case PrimitiveAggregate.Reduce(f) => s"reduce($f)($arg)"
    case PrimitiveAggregate.Sum(_) => s"sum($arg)"
  }

private def explainLambdaBody[T](expr: ExprNode[T], args: Map[ExprNode.Reference[?], String]): String = {
  def body[T](expr: ExprNode[T]): String =
    expr match {
      case ExprNode.And(lhs, rhs) => s"(${body(lhs)} && ${body(rhs)})"
      case ExprNode.Coalesce(values) => values.map(body).mkString("coalesce(", ", ", ")")
      case ExprNode.Equals(lhs, rhs) => s"(${body(lhs)} == ${body(rhs)})"
      case ExprNode.KnownNotNull(expr) => s"${body(expr)}.get"
      case ExprNode.LessThan(_, lhs, rhs) => s"(${body(lhs)} < ${body(rhs)})"
      case ExprNode.LessThanOrEqual(_, lhs, rhs) => s"(${body(lhs)} <= ${body(rhs)})"
      case ExprNode.Literal(value, _) => value.toString
      case ExprNode.Range(start, end) => s"(${body(start)} until ${body(end)})"
      case ExprNode.MakeSeq(values, _) => values.map(body).mkString("seq(", ", ", ")")
      case ExprNode.ConcatSeq(lhs, rhs) => s"${body(lhs)} ++ ${body(rhs)}"
      case ExprNode.MapSeq(seq, f) =>
        val argName = s"x${args.size}"
        val argsWithOuter = args + (f.arg -> argName)
        s"${body(seq)}.map($argName => ${explainLambdaBody(f.expr, argsWithOuter)})"
      case ExprNode.FilterSeq(seq, predicate) =>
        val argName = s"x${args.size}"
        val argsWithOuter = args + (predicate.arg -> argName)
        s"${body(seq)}.filter($argName => ${explainLambdaBody(predicate.expr, argsWithOuter)})"
      case ExprNode.ConcatString(strings) => strings.map(body).mkString("concat(", ", ", ")")
      case ExprNode.AggregateSeq(seq, lit, primitive) =>
        s"${body(seq)}.aggregate(${body(lit)}, ${explainPrimitiveAggregate(primitive, "_")})"
      case ExprNode.MakeSome(value) => s"some(${body(value)})"
      case ExprNode.MakeProduct(values, Codec.Product(tag, _, _)) =>
        val className = tag.runtimeClass.getName
        val structStart = if className.startsWith("scala.Tuple") then "(" else s"$className("
        tupleInstances(values).mapConst([t] => body(_)).mkString(structStart, ", ", ")")
      case ExprNode.Not(cond) => s"!${body(cond)}"
      case ExprNode.OptionToIterable(expr) => s"Option.option2Iterable(${body(expr)})"
      case ExprNode.Or(lhs, rhs) => s"(${body(lhs)} || ${body(rhs)})"
      case ExprNode.RaiseError(message, _) => s"raiseError(${body(message)})"
      case ref @ ExprNode.Reference(_, _) => args(ref)
      // TODO: We improve the output so we can output the plan of subqueries as well.
      case ExprNode.ScalarSubquery(_) => "<scalar subquery>"
      case ExprNode.ExistsSubquery(_) => "<exists subquery>"
      case ExprNode.Select(expr, name) => s"${body(expr)}.${name}"
      case ExprNode.Udf(arg, f, _) => s"<udf>(${body(arg)})"
      case ExprNode.UpcastToIterable(expr) => s"${body(expr)}"
      case ExprNode.Aggregate(arg, primitive) => explainPrimitiveAggregate(primitive, body(arg))
      case ExprNode.Cases(whenThenExpr, whenThenExprs, elseExpr) =>
        val whenThenStr = (whenThenExpr +: whenThenExprs)
          .map(branch => s"when ${body(branch.whenExpr)} then ${body(branch.thenExpr)}")
          .mkString(" ")
        s"(case $whenThenStr else ${body(elseExpr)})"
      case ExprNode.StartsWith(string, prefix) => s"${body(string)}.startsWith(${body(prefix)})"
      case ExprNode.Trim(string) => s"${body(string)}.trim()"
      case ExprNode.EndsWith(string, suffix) => s"${body(string)}.endsWith(${body(suffix)})"
      case ExprNode.Split(string, delimiter) => s"${body(string)}.split(${body(delimiter)})"
      case ExprNode.ToJson(inner) => s"toJson(${body(inner)})"
      case ExprNode.FromJson(inner, _) => s"fromJson(${body(inner)})"
      case ExprNode.SizeSeq(operand) => s"${body(operand)}.size"
      case ExprNode.ElementSeq(array, index) => s"${body(array)}.get(${body(index)})"
      case ExprNode.Add(_, lhs, rhs) => s"${body(lhs)} + ${body(rhs)}"
      case ExprNode.Quotient(_, lhs, rhs) => s"${body(lhs)} / ${body(rhs)}"
      case ExprNode.Cast(arg, canCast) =>
        val simpleName = canCast.codec.classTag.runtimeClass.getSimpleName
        s"${body(arg)}.cast[$simpleName]"
      case ExprNode.TryCast(arg, canTryCast) =>
        val simpleName = canTryCast.codec.classTag.runtimeClass.getSimpleName
        s"${body(arg)}.tryCast[$simpleName]"
      case ExprNode.TimestampToMicros(inner) => s"${body(inner)}.toMicros"
      case ExprNode.MicrosToTimestamp(inner) => s"microsToTimestamp(${body(inner)})"
      case ExprNode.DurationToMicros(inner) => s"${body(inner)}.toMicros"
      case ExprNode.MicrosToDuration(inner) => s"microsToDuration(${body(inner)})"
      case ExprNode.DateToDays(inner) => s"${body(inner)}.toDays"
      case ExprNode.DaysToDate(inner) => s"daysToDate(${body(inner)})"
      case ExprNode.ToRepr(inner, _) => s"asRepr(${body(inner)})"
      case ExprNode.FromRepr(inner, _) => s"fromRepr(${body(inner)})"
      case ExprNode.MakeMap(pairs) => s"makeMap(${body(pairs)})"
      case ExprNode.MapEntries(map) => s"${body(map)}.entries"
      case ExprNode.MapGet(map, key) => s"${body(map)}.get(${body(key)})"
      case ExprNode.DistinctSeq(operand) => s"${body(operand)}.distinct"
      case ExprNode.None(_) => "None"
      case ExprNode.Rand() => "rand()"
      case ExprNode.IsNaN(operand) => s"${body(operand)}.isNaN"
    }
  body(expr)
}

private def explainLambda[T](expr: ExprNode[T], args: Map[ExprNode.Reference[?], String]): String =
  if args.size == 1 then {
    val argStr = args.head._2
    s"${argStr} => ${explainLambdaBody(expr, args)}"
  } else {
    val argsStr = args.values.mkString("(", ", ", ")")
    s"$argsStr => ${explainLambdaBody(expr, args)}"
  }
