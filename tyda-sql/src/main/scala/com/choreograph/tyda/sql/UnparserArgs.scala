package com.choreograph.tyda.sql

import com.choreograph.tyda.ExprNode

private final case class UnparserArgs(
    val dialect: SqlDialect,
    val aliasGen: AliasGenerator,
    val references: Map[ExprNode.Reference[?], IdentifierOrSqlExpr] = Map()
) {
  def withReferences(additionalReferences: (ExprNode.Reference[?], IdentifierOrSqlExpr)*): UnparserArgs =
    this.copy(references = this.references ++ additionalReferences)
}
