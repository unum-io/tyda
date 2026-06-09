package com.choreograph.tyda.sql.ast

import java.io.Writer

import scala.annotation.targetName

import shapeless3.deriving.K0

import com.choreograph.tyda.NonEmpty

private[tyda] final case class SqlWriter(writer: Writer) {
  import SqlWriter.SqlWritable

  def write(str: String): Unit = writer.write(str)

  @targetName("writeIdent")
  def write(ident: Identifier): Unit = DdlWriter.writeIdentifier(writer, ident)

  def write(joinType: JoinType): Unit =
    joinType match {
      case JoinType.Inner => writer.write("JOIN")
      case JoinType.Left => writer.write("LEFT JOIN")
      case JoinType.Full => writer.write("FULL JOIN")
    }
  def write(tpe: DdlType): Unit = {
    val ddlWriter = DdlWriter(writer, pretty = false)
    ddlWriter.write(tpe, 0)
  }
  def write(expr: SqlExpr): Unit =
    expr match {
      case SqlExpr.As(e, alias) => writeSql"$e AS $alias"
      case SqlExpr.BinaryOp(op, lhs, rhs) => writeSql"($lhs $op $rhs)"
      case SqlExpr.Brackets(exprs) => writeSql1"[$exprs]"
      case SqlExpr.FieldAccess(struct, field) => writeSql"$struct.$field"
      case SqlExpr.Function(name, args) => writeSql"$name($args)"
      case SqlExpr.Index(arr, idx) => writeSql"$arr[$idx]"
      case SqlExpr.Ident(name) => write(name)
      case SqlExpr.UnaryOp(op, e, true) => writeSql"($op $e)"
      case SqlExpr.UnaryOp(op, e, false) => writeSql"($e $op)"
      case SqlExpr.LiteralString(value) =>
        writer.write('\'')
        value.foreach {
          case c @ ('\'' | '\\') =>
            writer.write("\\")
            writer.write(c)
          case '\b' => writer.write("\\b")
          case '\n' => writer.write("\\n")
          case '\r' => writer.write("\\r")
          case '\t' => writer.write("\\t")
          case c => writer.write(c)
        }
        writer.write('\'')
      case SqlExpr.LiteralHexString(value) =>
        write("X'")
        value.foreach(b => write(f"$b%02x"))
        write("'")
      case SqlExpr.LiteralByteEscapeString(value) =>
        write("B'")
        value.foreach(b => write(f"\\x$b%02x"))
        write("'")
      case SqlExpr.LiteralNumeric(value) => write(value)
      case SqlExpr.LiteralBool(value) => if value then write("TRUE") else write("FALSE")
      case SqlExpr.LiteralNull => write("NULL")
      case SqlExpr.Case(whens, elseExpr) =>
        write("CASE")
        whens.foreach { case (cond, result) => writeSql" WHEN $cond THEN $result" }
        elseExpr.foreach(e => writeSql1" ELSE $e")
        write(" END")
      case SqlExpr.Cast(variant, e, toType) => writeSql"$variant($e AS $toType)"
      case SqlExpr.Subquery(query) => writeSql1"($query)"
      case SqlExpr.Exists(query) => writeSql1"EXISTS ($query)"
      case SqlExpr.LambdaFunction(args, body) => args match {
          case Seq(arg) => writeSql"$arg -> $body"
          case args => writeSql"($args) -> $body"
        }
    }

  def write(options: Query.Options): Unit =
    if options.nonEmpty then {
      write(" OPTIONS(")
      options
        .zipWithIndex
        .foreach { case ((k, v), i) =>
          writeSql"$k=$v"
          if i < options.size - 1 then write(", ")
        }
      write(")")
    }

  def write(query: Query): Unit =
    query match {
      case Query.Select(select, from, where, groupBy, having, distinct, limit) =>
        val selectType = if distinct then "SELECT DISTINCT" else "SELECT"
        writeSql"$selectType $select"
        from.foreach(from => writeSql1" FROM $from")
        where.foreach(cond => writeSql1" WHERE $cond")
        if groupBy.nonEmpty then writeSql1" GROUP BY $groupBy"
        having.foreach(cond => writeSql1" HAVING $cond")
        limit.foreach(n => write(s" LIMIT $n"))

      case Query.Union(left, right, all) =>
        val union = if (all) then "UNION ALL" else "UNION"
        writeSql"($left) $union ($right)"

      case Query.Except(left, right, all) =>
        val except = if (all) then "EXCEPT" else "EXCEPT DISTINCT"
        writeSql"($left) $except ($right)"

      case Query.CreateTable(tableName, format, location, query, options) =>
        writeSql"CREATE TABLE $tableName USING $format$options LOCATION $location AS $query"

      case Query.ExportData(query, options) => writeSql"EXPORT DATA$options AS $query"
    }

  def write(from: From): Unit =
    from match {
      case From.Table(name) => write(name)
      case From.Expr(expr, alias) => writeSql"$expr AS $alias"
      case From.Subquery(query, alias) => writeSql"($query) AS $alias"
      case From.Join(left, right, JoinType.Inner, None) => writeSql"$left, $right"
      case From.Join(left, right, tpe, on) =>
        writeSql"$left $tpe $right"
        on.foreach(cond => writeSql1" ON $cond")
      case From.Values(values, columns, alias) =>
        given SqlWritable[Seq[NonEmpty[Seq[SqlExpr]]]] = SqlWritable.mkSeqWritable("(", "), (", ")")
        writeSql"VALUES $values AS $alias($columns)"
    }

  extension (sc: StringContext) {

    /** Same as writeSql where there is a single part, needed because
      * overloading writeSql does not work.
      */
    private def writeSql1[T: SqlWritable](arg: T): Unit = writeSql(Tuple1(arg))

    /** Write a SQL expression using the string context as the template.
      *
      * Example:
      * ```
      * writeSql"SELECT $columns FROM $table WHERE $condition"
      * ```
      *
      * Each value used in the format string must have a given SqlWritable
      * instance.
      */
    private def writeSql[T <: Tuple](args: T)(using instances: K0.ProductInstances[SqlWritable, T]): Unit = {
      val parts = sc.parts.iterator
      instances.foldLeft(args)(()) { [t] => (_, writable, value) =>
        writer.write(parts.next())
        writable(this, value)
      }
      parts.nextOption().foreach(writer.write(_))
      assert(!parts.hasNext)
    }
  }
}

private[tyda] object SqlWriter {

  private trait SqlWritable[-T] {
    def apply(writer: SqlWriter, t: T): Unit
  }

  private object SqlWritable {
    def mkSeqWritable[T: SqlWritable as writable](sep: String): SqlWritable[Seq[T]] =
      mkSeqWritable("", sep, "")
    def mkSeqWritable[T: SqlWritable as writable](
        start: String,
        sep: String,
        end: String
    ): SqlWritable[Seq[T]] =
      new SqlWritable[Seq[T]] {
        def apply(writer: SqlWriter, seq: Seq[T]): Unit = {
          writer.write(start)
          seq
            .iterator
            .zipWithIndex
            .foreach((element, idx) =>
              if idx > 0 then writer.write(sep)
              writable(writer, element)
            )
          writer.write(end)
        }
      }

    given SqlWritable[From] = _.write(_)
    given SqlWritable[Identifier] = _.write(_)
    given seqIdent: SqlWritable[NonEmpty[Seq[Identifier]]] =
      NonEmpty.subtype.substituteContra(mkSeqWritable(", "))
    given SqlWritable[JoinType] = _.write(_)
    given SqlWritable[Query] = _.write(_)
    given SqlWritable[SqlExpr] = _.write(_)
    given SqlWritable[Seq[SqlExpr]] = mkSeqWritable(", ")
    given SqlWritable[NonEmpty[Seq[SqlExpr]]] = NonEmpty.subtype.substituteContra(mkSeqWritable(", "))
    given SqlWritable[String] = _.write(_)
    given SqlWritable[DdlType] = _.write(_)
    given SqlWritable[Query.Options] = _.write(_)
  }
}
