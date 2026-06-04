package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.catalyst.expressions.Expression
import org.apache.spark.sql.classic.ColumnConversions

private def columnToExpression(column: Column): Expression = ColumnConversions.expression(column)
