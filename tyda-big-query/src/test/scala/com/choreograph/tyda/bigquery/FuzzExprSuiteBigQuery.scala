package com.choreograph.tyda.bigquery

import org.scalatest.ParallelTestExecution

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.fuzz.FuzzExprSuite

class FuzzExprSuiteBigQuery extends FuzzExprSuite, WithConfiguredBigQueryTestRunner, ParallelTestExecution {
  override def implementation[From, To](expr: CompiledExpr[From, To]): From => To = {
    given Codec[From] = expr.arg.codec
    value => runner.collect(Dataset.Select1(Dataset.from(Seq(value)), expr)).head
  }
  override def implementationExplain[From, To](expr: CompiledExpr[From, To]): String = {
    given Codec[From] = expr.arg.codec
    runner.explain(Dataset.Select1(Dataset.from(Seq.empty[From]), expr))
  }

  override def knownBug[From, To](expr: CompiledExpr[From, To]): Boolean =
    expr
      .expr
      .exists {
        case ExprNode.RaiseError(_, _) => true
        // TODO: Do not generate such exprs, use NonEmpty?
        case ExprNode.ConcatString(e) => e.isEmpty
        case _ => false
      }
}
