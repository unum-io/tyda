package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ExprNode.WhenThen

private[tyda] object NullIntolerant {

  /** Check if expr will return null if all subtrees matching `arg` are changed
    * to `null`
    *
    * This is based on interpreting ExprNode as the corresponding SQL semantics.
    */
  def apply(expr: ExprNode[?], arg: ExprNode[?]): Boolean = {
    def inner(e: ExprNode[?]): Boolean =
      e match {
        case `arg` => true
        case ExprNode.None(_) => true
        case If(_, ifTrue, ifFalse) => inner(ifTrue) && inner(ifFalse)
        case ExprNode.Cases(WhenThen(_, thenExpr), cases, elseExpr) => inner(thenExpr) &&
          cases.map(_.thenExpr).forall(inner) && inner(elseExpr)
        case ExprNode.MakeSome(Nullable(_)) => false
        case ExprNode.MakeSome(expr) => inner(expr)
        case ExprNode.KnownNotNull(expr) => inner(expr)
        case ExprNode.Coalesce(operands) => operands.forall(inner)
        case ExprNode.Add(_, lhs, rhs) => inner(lhs) || inner(rhs)
        case ExprNode.Quotient(_, lhs, rhs) => inner(lhs) || inner(rhs)
        case ExprNode.Split(string, delimiter) => inner(string) || inner(delimiter)
        case ExprNode.And(lhs, rhs) => inner(lhs) && inner(rhs)
        case ExprNode.Or(lhs, rhs) => inner(lhs) && inner(rhs)
        case ExprNode.Not(operand) => inner(operand)
        case ExprNode.Equals(Nullable(_), Nullable(_)) =>
          false // We perform null safe equals which is never null
        case ExprNode.Equals(lhs, rhs) => inner(lhs) || inner(rhs)
        case ExprNode.LessThan(_, lhs, rhs) => inner(lhs) || inner(rhs)
        case ExprNode.LessThanOrEqual(_, lhs, rhs) => inner(lhs) || inner(rhs)
        case ExprNode.Select(expr, _) => inner(expr)
        case ExprNode.OptionToIterable(expr) => inner(expr)
        case ExprNode.UpcastToIterable(expr) => inner(expr)
        // For bigquery our sql is not. Should we have a dialect here?
        case ExprNode.MapSeq(_, _) | ExprNode.FilterSeq(_, _) | ExprNode.DistinctSeq(_) |
            ExprNode.FlattenSeq(_) => false
        case ExprNode.MakeMap(entries) => inner(entries)
        case ExprNode.AggregateSeq(expr, _, _) => inner(expr)
        case ExprNode.Range(start, end) => inner(start) || inner(end)
        case ExprNode.TryCast(expr, _) => inner(expr)
        case ExprNode.Cast(expr, _) => inner(expr)
        case ExprNode.ElementSeq(array, index) => inner(array) || inner(index)
        case ExprNode.ToRepr(expr, _) => inner(expr)
        case ExprNode.FromRepr(expr, _) => inner(expr)
        case ExprNode.ConcatSeq(lhs, rhs) => inner(lhs) || inner(rhs)
        case ExprNode.SizeSeq(expr) => inner(expr)
        case ExprNode.MapEntries(expr) => inner(expr)
        case ExprNode.MapGet(map, value) => inner(map) || inner(value)
        case ExprNode.StartsWith(str, prefix) => inner(str) || inner(prefix)
        case ExprNode.Trim(expr) => inner(expr)
        case ExprNode.EndsWith(str, suffix) => inner(str) || inner(suffix)
        case ExprNode.ConcatString(exprs) => exprs.exists(inner)
        case ExprNode.TimestampToMicros(expr) => inner(expr)
        case ExprNode.MicrosToTimestamp(expr) => inner(expr)
        case ExprNode.DurationToMicros(expr) => inner(expr)
        case ExprNode.MicrosToDuration(expr) => inner(expr)
        case ExprNode.DateToDays(expr) => inner(expr)
        case ExprNode.DaysToDate(expr) => inner(expr)
        // For spark it is not.
        case ExprNode.IsNaN(_) => false
        // BigQuery supports arbirary types and ToJson for a null will return the string "null"
        case ExprNode.ToJson(expr) => false
        case ExprNode.FromJson(expr, _) => inner(expr)
        case ExprNode.BytesLength(expr) => inner(expr)
        case ExprNode.MakeProduct(_, _) | ExprNode.Aggregate(_, _) | ExprNode.Udf(_, _, _) | ExprNode
              .Reference(_, _) | ExprNode.Literal(_, _) | ExprNode.MakeSeq(_, _) | ExprNode.RaiseError(_, _) |
            ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_) | ExprNode.Rand() => false
      }
    inner(expr)
  }

  // This uses the fact that in sql ternary logic `null && false` is `false` this allows us to simplify more
  // expressions using `ExprNode.And`.
  def nullOrFalse(expr: ExprNode[?], arg: ExprNode[?]): Boolean = {
    def inner(e: ExprNode[?]): Boolean =
      e match {
        case ExprNode.Literal(false, _) => true
        case ExprNode.Or(lhs, rhs) => inner(lhs) && inner(rhs)
        case ExprNode.And(lhs, rhs) => inner(lhs) || inner(rhs)
        case If(_, ifTrue, ifFalse) => inner(ifTrue) && inner(ifFalse)
        case other => NullIntolerant(other, arg)
      }
    inner(expr)
  }
}
