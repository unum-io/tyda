package com.choreograph.tyda.spark

import org.apache.spark.sql.execution.ExtendedMode

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.fuzz.FuzzDatasetSuite

class FuzzDatasetSuiteSpark extends FuzzDatasetSuite, SharedSparkSession {
  override def implementation[To](ds: Dataset[To]): Seq[To] = DatasetOnSpark(ds).collect().toSeq
  override def implementationExplain[To](ds: Dataset[To]): String =
    DatasetOnSpark(ds).queryExecution.explainString(ExtendedMode)

  override def knownBug[To](ds: Dataset[To]): Boolean =
    ds.existsExpr {
      // Spark does not enforce date/timestamp limits
      case ExprNode.DaysToDate(_) | ExprNode.MicrosToTimestamp(_) => true
      case ExprNode.RaiseError(_, _) => true
      case _ => false
    }
}
