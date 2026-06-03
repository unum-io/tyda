package com.choreograph.tyda.sql.ast

import com.choreograph.tyda.NonEmpty

private[sql] enum Query {
  case Select(
      select: NonEmpty[Seq[SqlExpr]],
      from: Option[From],
      where: Option[SqlExpr],
      groupBy: Seq[SqlExpr],
      having: Option[SqlExpr],
      distinct: Boolean,
      orderBy: Seq[SqlExpr] = Seq.empty,
      limit: Option[Int] = None
  )
  case Union(left: Query, right: Query, all: Boolean)
  case CreateTable(tableName: String, format: String, location: SqlExpr, query: Query, options: Query.Options)
  case ExportData(query: Query, options: Query.Options)
}

object Query {
  opaque type Options <: Map[String, SqlExpr] = Map[String, SqlExpr]
  object Options {
    def apply(options: (String, String)*): Options =
      options.map { case (k, v) => (k, SqlExpr.LiteralString(v)) }.toMap
  }
  object Select {
    def apply(select: NonEmpty[Seq[SqlExpr]], from: From): Query.Select =
      Query.Select(select, Some(from), None, Seq.empty, None, distinct = false)
  }
}
