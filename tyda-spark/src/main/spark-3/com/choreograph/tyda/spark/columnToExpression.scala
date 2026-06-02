package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.Expression

private def columnToExpression(column: Column): Expression = column.expr
