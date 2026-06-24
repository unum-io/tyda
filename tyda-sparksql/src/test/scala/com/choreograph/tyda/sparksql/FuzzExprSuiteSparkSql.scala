package com.choreograph.tyda.sparksql

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.fuzz.FuzzExprSuite

class FuzzExprSuiteSparkSql extends FuzzExprSuite, SharedSparkSession {
  override def implementation[From, To](expr: CompiledExpr[From, To]): From => To = {
    given Codec[From] = expr.arg.codec
    value => SparkSqlRunner(using spark).collect(Dataset.Select1(Dataset.from(Seq(value)), expr)).head
  }
  override def implementationExplain[From, To](expr: CompiledExpr[From, To]): String = {
    given Codec[From] = expr.arg.codec
    SparkSqlRunner(using spark).explain(Dataset.Select1(Dataset.from(Seq.empty[From]), expr))
  }

  override def knownBug[From, To](expr: CompiledExpr[From, To]): Boolean =
    expr
      .expr
      .exists {
        // Spark does not enforce date/timestamp limits
        case ExprNode.DaysToDate(_) | ExprNode.MicrosToTimestamp(_) => true
        case ExprNode.RaiseError(_, _) => true
        case _ => false
      }
}
