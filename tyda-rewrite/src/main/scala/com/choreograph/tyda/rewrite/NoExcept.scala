package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

object NoExcept {

  /** Extractor that matches exprs that will never throw an exception */
  def unapply[U](expr: ExprNode[U]): Boolean =
    expr match {
      case ExprNode.RaiseError(_, _) | ExprNode.Udf(_, _, _) | ExprNode.Quotient(_, _, _) |
          ExprNode.Add(_, _, _) | ExprNode.ElementSeq(_, _) => false
      case ExprNode.Reference(_, _) | ExprNode.Select(_, _) | ExprNode.MakeProduct(_, _) |
          ExprNode.Range(_, _) | ExprNode.MakeSeq(_, _) | ExprNode.ConcatSeq(_, _) | ExprNode.MapSeq(_, _) |
          ExprNode.FlattenSeq(_) | ExprNode.FilterSeq(_, _) | ExprNode.AggregateSeq(_, _, _) | ExprNode
            .MakeMap(_) | ExprNode.MapEntries(_) | ExprNode.MapGet(_, _) | ExprNode.Literal(_, _) |
          ExprNode.Not(_) | ExprNode.Or(_, _) | ExprNode.And(_, _) | ExprNode.Equals(_, _) | ExprNode
            .LessThan(_, _, _) | ExprNode.LessThanOrEqual(_, _, _) | ExprNode.UpcastToIterable(_) | ExprNode
            .OptionToIterable(_) | ExprNode.MakeSome(_) | ExprNode.None(_) | ExprNode.Coalesce(_) | ExprNode
            .ScalarSubquery(_) | ExprNode.ExistsSubquery(_) | ExprNode.Aggregate(_, _) |
          ExprNode.Cases(_, _, _) | ExprNode.KnownNotNull(_) | ExprNode.Split(_, _) |
          ExprNode.StartsWith(_, _) | ExprNode.Trim(_) | ExprNode.EndsWith(_, _) | ExprNode.ConcatString(_) |
          ExprNode.Rand() | ExprNode.IsNaN(_) | ExprNode.ToJson(_) | ExprNode.FromJson(_, _) | ExprNode
            .DistinctSeq(_) | ExprNode.SizeSeq(_) | ExprNode.Cast(_, _) | ExprNode.TryCast(_, _) | ExprNode
            .TimestampToMicros(_) | ExprNode.MicrosToTimestamp(_) | ExprNode.DurationToMicros(_) | ExprNode
            .MicrosToDuration(_) | ExprNode.DateToDays(_) | ExprNode.DaysToDate(_) | ExprNode.BytesLength(_) |
          ExprNode.ToRepr(_, _) | ExprNode.FromRepr(_, _) | ExprNode.ArrayJoin(_, _) => true
    }
}

/** Returns true if evaluating the expr e will never fail with an exception */
def allNoExcept(e: ExprNode[?]): Boolean = e.forall(NoExcept.unapply)
