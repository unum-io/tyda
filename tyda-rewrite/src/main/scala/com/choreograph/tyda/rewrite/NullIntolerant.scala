package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ExprNode.WhenThen
import com.choreograph.tyda.TreeApi.Continue

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
        case ExprNode.MakeProduct(_, _) | ExprNode.Aggregate(_, _) | ExprNode.Udf(_, _, _) | ExprNode
              .Reference(_, _) | ExprNode.Literal(_, _) | ExprNode.MakeSeq(_, _) | ExprNode.RaiseError(_, _) |
            ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_) | ExprNode.Rand() => false
        // For bigquery our sql is not. Should we have a dialect here?
        case ExprNode.MapSeq(_, _) | ExprNode.FilterSeq(_, _) | ExprNode.DistinctSeq(_) |
            ExprNode.FlattenSeq(_) => false
        case ExprNode.Equals(Nullable(_), Nullable(_)) =>
          false // We perform null safe equals which is never null
        case ExprNode.MakeSome(Nullable(_)) => false
        // For spark it is not.
        case ExprNode.IsNaN(_) => false
        // BigQuery supports arbitrary types and ToJson for a null will return the string "null"
        case ExprNode.ToJson(_) => false
        case ExprNode.And(lhs, rhs) => inner(lhs) && inner(rhs)
        case ExprNode.Or(lhs, rhs) => inner(lhs) && inner(rhs)
        case ExprNode.Cases(WhenThen(_, thenExpr), cases, elseExpr) => inner(thenExpr) &&
          cases.map(_.thenExpr).forall(inner) && inner(elseExpr)
        case ExprNode.None(_) => true
        case ExprNode.MakeSome(_) | ExprNode.KnownNotNull(_) | ExprNode.Coalesce(_) | ExprNode.Add(_, _, _) |
            ExprNode.Quotient(_, _, _) | ExprNode.Split(_, _) | ExprNode.Not(_) | ExprNode.Equals(_, _) |
            ExprNode.LessThan(_, _, _) | ExprNode.LessThanOrEqual(_, _, _) | ExprNode.Select(_, _) | ExprNode
              .OptionToIterable(_) | ExprNode.UpcastToIterable(_) | ExprNode.MakeMap(_) | ExprNode
              .AggregateSeq(_, _, _) | ExprNode.Range(_, _) | ExprNode.TryCast(_, _) | ExprNode.Cast(_, _) |
            ExprNode.ElementSeq(_, _) | ExprNode.ToRepr(_, _) | ExprNode.FromRepr(_, _) |
            ExprNode.ConcatSeq(_, _) | ExprNode.SizeSeq(_) | ExprNode.MapEntries(_) | ExprNode.MapGet(_, _) |
            ExprNode.StartsWith(_, _) | ExprNode.Trim(_) | ExprNode.EndsWith(_, _) |
            ExprNode.ConcatString(_) | ExprNode.TimestampToMicros(_) | ExprNode.MicrosToTimestamp(_) |
            ExprNode.DurationToMicros(_) | ExprNode.MicrosToDuration(_) | ExprNode.DateToDays(_) | ExprNode
              .DaysToDate(_) | ExprNode.FromJson(_, _) | ExprNode.BytesLength(_) | ExprNode.ArrayJoin(_, _) =>
          ExprNode.api.foldChildren(e)(false)([t] => (acc, child) => Continue(acc || inner(child)))
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
