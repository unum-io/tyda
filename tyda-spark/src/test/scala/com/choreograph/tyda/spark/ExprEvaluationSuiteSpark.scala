package com.choreograph.tyda.spark

import org.apache.spark.sql.Column

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.Expr
import com.choreograph.tyda.testsuites.ExprEvaluationSuite

/* This suite evaluates expressions without doing creating a full plan.
 * The hope is that this leads to much faster test for the ExprOnSpark logic. */
class ExprEvaluationSuiteSpark extends ExprEvaluationSuite, SharedSparkSession {
  def evaluator[From, To](compiled: CompiledExpr[From, To]): From => To =
    SparkExprEvaluator.evaluator(compiled)

  override def evaluate[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): Seq[To] = {
    val eval = evaluator(CompiledExpr(expr))
    values.map(eval)
  }

  override def explain[From: Codec, To](expr: Expr[From] => Expr[To], values: Seq[From]): String = {
    val c: Column = ExprOnSpark.unresolved[From](CompiledExpr(expr))
    c.toString
  }
}
