package com.choreograph.tyda.spark

import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.functions.col
import org.apache.spark.sql.functions.struct

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Forbidden
import com.choreograph.tyda.shapeless3extras.mapConst

/** Helper class for creating Spark [[Column]]s.
  *
  * This used to handle that resolved expressions can be created from either a
  * [[Dataset]] or an existing [[Column]]. This is used in the conde converting
  * [[com.choreograph.tyda.Expr]] to [[Column]] in order to handle both cases.
  */
private[spark] trait ColumnFactory[T: Codec] {

  /** The codec of row column.
    */
  def codec: Codec[T] = summon

  /** Create a column acessing a column by name.
    */
  def column(name: String): Column

  /** Returns a Seq of all top level columns
    */
  def columns: Seq[Column] = ColumnFactory.topLevelColumns[T].map(column(_))

  /** Create a column acessing the whole row.
    */
  def row: Column =
    if ColumnFactory.isWrappedForTopLevel[T] then column(ColumnFactory.wrappedColumnName)
    else struct(columns*)
}

private[spark] object ColumnFactory {
  def fromDF[T](df: DataFrame, codec: Codec[T]): ColumnFactory[T] =
    new ColumnFactory[T](using codec) {
      def column(name: String): Column = df(name)
    }

  def apply[T: Codec](ds: Dataset[T]): ColumnFactory[T] =
    new ColumnFactory[T] {
      def column(name: String): Column = ds(name)
    }

  def apply[T: Codec](c: Column): ColumnFactory[T] =
    new ColumnFactory[T] {
      def column(name: String): Column = c(name)
      override def columns = if isWrappedForTopLevel[T] then Seq(c) else super.columns
      override def row: Column = c
    }

  /** Used together with table aliases as it seems the alias is not picked up
    * when creating using Dataset#apply method.
    */
  def fromIdentitfier[T: Codec](identifier: String): ColumnFactory[T] =
    new ColumnFactory[T] {
      def column(name: String): Column = col(s"$identifier.$name")
    }

  def unresolved[T: Codec]: ColumnFactory[T] =
    new ColumnFactory[T] {
      def column(name: String): Column = col(name)
    }

  private val wrappedColumnName = "value"

  private def topLevelColumns[T: Codec]: Seq[String] =
    Codec[T] match {
      case Codec.Product(_, _, Some(_)) => Seq(Forbidden.column)
      case Codec.Product(_, fields, _) => fields.mapConst[String]([t] => _.name)
      case Codec.FromInjection(_, to) => topLevelColumns(using to)
      case _ => Seq(wrappedColumnName)
    }

  private def isWrappedForTopLevel[T: Codec]: Boolean =
    Codec[T] match {
      case _: Codec.Product[T] => false
      case Codec.FromInjection(_, to) => isWrappedForTopLevel(using to)
      case _ => true
    }
}
