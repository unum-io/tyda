package com.choreograph.tyda.spark

import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.fuzz.FuzzExprSuite

class FuzzExprSuiteSpark extends FuzzExprSuite, SharedSparkSession {
  override def implementation[From, To](expr: CompiledExpr[From, To]): From => To =
    SparkExprEvaluator.evaluator(expr)
  override def implementationExplain[From, To](expr: CompiledExpr[From, To]): String =
    ExprOnSpark.unresolved[From](expr)(using expr.arg.codec).toString

  override def knownBug[From, To](expr: CompiledExpr[From, To]): Boolean =
    expr
      .expr
      .exists {
        // Spark does not enforce date/timestamp limits
        case ExprNode.DaysToDate(_) | ExprNode.MicrosToTimestamp(_) => true
        case _ => false
      }
}
