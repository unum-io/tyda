package com.choreograph.tyda.sql

import java.io.StringWriter

import scala.annotation.targetName

import com.choreograph.tyda.Dataset
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.rewrite.ExplodeOptionToFilter
import com.choreograph.tyda.sql.ast.Query
import com.choreograph.tyda.sql.ast.SqlExpr
import com.choreograph.tyda.sql.ast.SqlWriter
import com.choreograph.tyda.unreachable

enum DatasetToSqlError {
  case RequiresUdfCapability(msg: String)
  // This is a temporary error and each usage should be seen as a TODO to implement the feature
  case NotImplemented(msg: String)
}

type Result[T] = Either[DatasetToSqlError, T]

object Result {
  extension [T](seq: Seq[Result[T]]) {
    def sequence: Result[Seq[T]] =
      seq.foldRight(Right(Vector.empty): Result[Seq[T]])((r, acc) => r.flatMap(v => acc.map(v +: _)))
  }
  extension [T](seq: NonEmpty[Seq[Result[T]]]) {
    @targetName("nonEmptySequence")
    def sequence: Result[NonEmpty[Seq[T]]] =
      seq
        .underlying
        .sequence
        .map(NonEmpty.from(_).getOrElse(unreachable("NonEmpty sequence produced empty Seq")))
  }
  extension [T](opt: Option[Result[T]]) {
    def sequence: Result[Option[T]] =
      opt match {
        case Some(r) => r.map(Some(_))
        case None => Right(None)
      }
  }
}

def toSql[T](ds: Dataset[T] | Dataset.Action, dialect: SqlDialect): Result[String] =
  ds match {
    case dataset: Dataset[T] => toSql(dataset, dialect)
    case action: Dataset.Action => toSql(action, dialect)
  }

def toSql[T](ds: Dataset[T], dialect: SqlDialect): Result[String] =
  val transformedDs = dialect.correctnessRules.transform(ds)
  unparseDs(transformedDs, UnparserArgs(dialect, AliasGenerator.Default()))
    .flatMap(_.build())
    .map(queryToString)

def toSql(action: Dataset.Action, dialect: SqlDialect): Result[String] = {
  val args = UnparserArgs(dialect, AliasGenerator.Default())
  val transformedAction = dialect.correctnessRules.transform(action)
  transformedAction match {
    case Dataset.Action.Write(input, path, format) => unparseDs(input, args)
        .flatMap(_.build())
        .map { query =>
          val formatStr = format.toString.toUpperCase
          args.dialect.writeSupport match {
            case SqlDialect.WriteSupport.CreateTable(formatToOptions) =>
              val tableName = s"temp_write_${Math.abs(path.hashCode)}"
              val options = formatToOptions.getOrElse(format, Map.empty)
              Query.CreateTable(
                tableName,
                formatStr,
                SqlExpr.LiteralString(path),
                query,
                Query.Options(options.toSeq*)
              )
            case SqlDialect.WriteSupport.ExportData => Query.ExportData(
                query,
                Query.Options("uri" -> s"${path}part-*.${format.toString.toLowerCase}", "format" -> formatStr)
              )
          }
        }
        .map(queryToString)
  }
}

private def queryToString(query: Query): String = {
  val stringWriter = new StringWriter()
  SqlWriter(stringWriter).write(query)
  stringWriter.toString
}

private def unparseDs[T](ds: Dataset[T], args: UnparserArgs): Result[SelectBuilder[?, T]] = {
  def inner[U](ds: Dataset[U]): Result[SelectBuilder[?, U]] = {
    val result: Result[SelectBuilder[?, U]] = ds match {
      case Dataset.Aggregate(input, aggregate) => inner(input).flatMap(_.aggregate(aggregate))
      case Dataset.Distinct(input) => inner(input).map(_.copy(distinct = true))
      case Dataset.Filter(input, predicate) => inner(input).map(_.filter(predicate))
      case Dataset.FromSeq(values, codec) => SelectBuilder.fromSeq(values, codec, args)
      case Dataset.FullOuterJoin(left, right, on) => for {
          lhs <- inner(left)
          rhs <- inner(right)
          joined <- lhs.fullJoin(rhs, on)
        } yield joined
      case Dataset.GroupedAggregate(input, key, aggregate) =>
        inner(input).flatMap(_.aggregate(key, aggregate))
      case Dataset.Join(left, right, on) => for {
          lhs <- inner(left)
          rhs <- inner(right)
          joined <- lhs.join(rhs, on)
        } yield joined
      case Dataset.LeftOuterJoin(left, right, on) => for {
          lhs <- inner(left)
          rhs <- inner(right)
          joined <- lhs.leftJoin(rhs, on)
        } yield joined
      case Dataset.LeftAntiJoin(left, right, on) => inner(left).flatMap(_.antiJoin(right, on))
      case Dataset.MapPartitions(_, _, _) =>
        Left(DatasetToSqlError.RequiresUdfCapability("MapPartitions is not supported on SQL"))
      case Dataset.ReadPath(path = _) | Dataset.ReadPathWithHivePartitions(basePath = _) | Dataset
            .ReadWithMetadata(_) | Dataset.ReadPartitionsPaths(_) | Dataset.ReadTablePartitionsPaths(_, _) =>
        Left(DatasetToSqlError.NotImplemented("support reading from path in SQL"))
      case Dataset.ReadTable(name, _, partitionCodec, modelCodec) =>
        Right(SelectBuilder.fromTable(name, partitionCodec, modelCodec, args))
      case ExplodeOptionToFilter(ds) => inner(ds)
      case Dataset.Select1(input, expr) => inner(input).flatMap(_.select(expr))
      case Dataset.SelectN(input, exprs) => inner(input).flatMap(_.selectN(exprs))
      case Dataset.Cache(input) => inner(input)
      case Dataset.Limit(input, n) => inner(input).flatMap(_.limit(n))
      case Dataset.OrderBy(input, key) => inner(input).flatMap(_.orderBy(key))
      case Dataset.Union(left, right) => for {
          lhs <- inner(left)
          rhs <- inner(right)
          union <- lhs.union(rhs)
        } yield union
    }
    result.map(_.simplifyExprs())
  }

  inner(ds)
}
