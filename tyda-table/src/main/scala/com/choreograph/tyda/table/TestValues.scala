package com.choreograph.tyda.table

enum TestValues[M] {
  case Fixed(values: Seq[M])
  case Partitioned(pathToValues: Map[String, Seq[M]])
}
