package com.choreograph.tyda.sql

import com.choreograph.tyda.sql.ast.Identifier
import com.choreograph.tyda.sql.ast.SqlExpr

private enum IdentifierOrSqlExpr {
  case Ident(ident: Identifier)
  case Expr(expr: SqlExpr)
}

object IdentifierOrSqlExpr {
  object Ident {
    def apply(name: String): IdentifierOrSqlExpr = Ident(Identifier(name))
  }
  object Expr {
    def apply(name: String): IdentifierOrSqlExpr = Expr(SqlExpr.Ident(name))
  }
}
