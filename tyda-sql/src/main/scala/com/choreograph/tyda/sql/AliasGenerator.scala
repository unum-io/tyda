package com.choreograph.tyda.sql

trait AliasGenerator {
  def table(): String
  def column(): String
}

object AliasGenerator {
  private class CountingGenerator(prefix: String) {
    private var counter = -1
    def apply(): String = {
      counter += 1
      s"$prefix$counter"
    }
  }
  class Default extends AliasGenerator {
    private val tableGen = new CountingGenerator("t")
    private val columnGen = new CountingGenerator("c")
    override def table(): String = tableGen()
    override def column(): String = columnGen()
  }
}
