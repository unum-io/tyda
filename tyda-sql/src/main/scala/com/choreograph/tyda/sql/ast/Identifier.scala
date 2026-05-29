package com.choreograph.tyda.sql.ast

/** An identifier, such as a table or column name.
  */
private[sql] opaque type Identifier = String

private[sql] object Identifier {
  def apply(name: String): Identifier = name

  extension (ident: Identifier) { def value: String = ident }
}
