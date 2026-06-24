package com.choreograph.tyda.sparksql

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.fuzz.FuzzDatasetSuite

class FuzzDatasetSuiteSparkSql extends FuzzDatasetSuite, SharedSparkSession {
  override def implementation[To](ds: Dataset[To]): Seq[To] = {
    println(SparkSqlRunner(using spark).explain(ds))
    SparkSqlRunner(using spark).collect(ds)
  }
  override def implementationExplain[To](ds: Dataset[To]): String = {
    SparkSqlRunner(using spark).explain(ds)
  }

  override def knownBug[To](ds: Dataset[To]): Boolean =
    ds
      .existsExpr {
        // Spark does not enforce date/timestamp limits
        case ExprNode.DaysToDate(_) | ExprNode.MicrosToTimestamp(_) => true
        case ExprNode.RaiseError(_, _) => true
        case _ => false
      }
}
