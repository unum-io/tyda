package com.choreograph.tyda.spark

import org.apache.spark.sql.Row

/** Needs to be java public since it used in spark expressions. */
private[spark] final class RowProduct(row: Row) extends Product, Serializable {
  def productArity: Int = row.length
  def productElement(n: Int): Any = row.get(n)
  def canEqual(that: Any): Boolean = false
}
