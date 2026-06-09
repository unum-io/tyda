package com.choreograph.tyda.sql.ast

private[sql] enum SqlExpr {
  case As(expr: SqlExpr, alias: Identifier)
  case Brackets(exprs: Seq[SqlExpr])
  case BinaryOp(op: String, lhs: SqlExpr, rhs: SqlExpr)
  case FieldAccess(struct: SqlExpr, field: Identifier)
  case Function(name: String, args: Seq[SqlExpr])
  case Index(array: SqlExpr, index: SqlExpr)
  case Ident(name: Identifier)
  case LiteralString(value: String)
  case LiteralHexString(value: Array[Byte])
  case LiteralByteEscapeString(value: Array[Byte])
  case LiteralNumeric(value: String)
  case LiteralBool(value: Boolean)
  case LiteralNull
  case UnaryOp(op: String, expr: SqlExpr, isPrefix: Boolean)
  case Case(whens: Seq[(condition: SqlExpr, result: SqlExpr)], elseExpr: Option[SqlExpr] = None)
  case Cast(variant: String, expr: SqlExpr, to: DdlType)
  case Subquery(query: Query)
  case Exists(query: Query)
  case LambdaFunction(args: Seq[SqlExpr], body: SqlExpr)
}

private[sql] object SqlExpr {
  def not(expr: SqlExpr): SqlExpr = UnaryOp("NOT", expr, isPrefix = true)
  def isNull(expr: SqlExpr): SqlExpr = UnaryOp("IS NULL", expr, isPrefix = false)
  def isNotNull(expr: SqlExpr): SqlExpr = UnaryOp("IS NOT NULL", expr, isPrefix = false)

  object LambdaFunction {
    def apply(arg: SqlExpr, body: SqlExpr): SqlExpr = SqlExpr.LambdaFunction(Seq(arg), body)
  }

  object Cast {
    def apply(expr: SqlExpr, to: DdlType): SqlExpr.Cast = SqlExpr.Cast("CAST", expr, to)
  }

  object As {
    def apply(expr: SqlExpr, alias: String): SqlExpr = new As(expr, Identifier(alias))
  }

  object FieldAccess {
    def apply(struct: SqlExpr, field: String): SqlExpr = new FieldAccess(struct, Identifier(field))
  }

  object Ident {
    def apply(name: String): SqlExpr = new Ident(Identifier(name))
  }
}
