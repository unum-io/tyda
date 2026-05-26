package com.choreograph.tyda.sql.ast

import com.choreograph.tyda.NonEmpty

private[sql] enum From {
  case Table(name: Identifier)
  case Expr(name: SqlExpr, alias: Identifier)
  case Subquery(query: Query, alias: Identifier)
  case Join(left: From, right: From, joinType: JoinType, on: Option[SqlExpr])
  case Values(values: Seq[NonEmpty[Seq[SqlExpr]]], columns: NonEmpty[Seq[Identifier]], alias: Identifier)
}

private[sql] object From {
  object Expr {
    def apply(expr: SqlExpr, alias: String): From = Expr(expr, Identifier(alias))
  }

  object Table {
    def apply(name: String): From = Table(Identifier(name))
  }

  object Subquery {
    def apply(query: Query, alias: String): From = Subquery(query, Identifier(alias))
  }

  object Values {
    def apply(values: Seq[NonEmpty[Seq[SqlExpr]]], columns: NonEmpty[Seq[String]], alias: String): From = {
      assert(values.forall(_.length == columns.length), "All rows must have the same number of columns")
      Values(values, columns.map(Identifier(_)), Identifier(alias))
    }
  }
}
