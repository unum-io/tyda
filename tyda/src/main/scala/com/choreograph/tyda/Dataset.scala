package com.choreograph.tyda

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple
import scala.annotation.targetName
import scala.deriving.Mirror

import shapeless3.deriving.K0

import com.choreograph.tyda.Expr.AsExpr
import com.choreograph.tyda.Expr.knownNotNull
import com.choreograph.tyda.TreeApi.Control
import com.choreograph.tyda.TreeApi.StopOrContinue
import com.choreograph.tyda.functions.coalesce
import com.choreograph.tyda.functions.explode
import com.choreograph.tyda.functions.namedTuple
import com.choreograph.tyda.functions.seq
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances

sealed trait Dataset[T: Codec] {
  import Dataset.*
  def codec: Codec[T] = summon

  /** Filter the Dataset based on a predicate.
    *
    * If performance is important consider using the expression api with
    * [[where]] instead.
    */
  def filter(p: T => Boolean): Dataset[T] = where(_.udf(p))

  /** Filter the Dataset based on a expression predicate. */
  def where[R: AsExpr.Of[Boolean]](p: Expr[T] => R): Dataset[T] = Dataset.Filter(this, CompiledExpr(p))

  /** Select a single expression from the Dataset.
    *
    * The return type is anything that can be converted to an [[Expr]] or
    * [[ExplodeExpr]] using the [[AsExprOrExplode]] typeclass. This allows the
    * following examples to work:
    * ```scala
    * val ds: Dataset[(Int, Seq[Int])] = ???
    * ds.select(_._1) // Select single column
    * ds.select(v => (v._1, 1)) // Select tuple with expr and literal
    * import com.choreograph.tyda.functions.explode
    * ds.select(explode(_._2)) // Select and explode a iterable expression
    * ```
    * By using a type class for the conversion this can be supported without
    * needing to enable implicit conversions.
    */
  def select[R, I: AsExprOrExplode.Of[R]](s1: Expr[T] => I): Dataset[R] =
    Dataset.Select1[T, R](this, CompiledExprOrExplode(s1))

  /** Select 2 expressions from the Dataset.
    *
    * For detailed usage see [[select]].
    */
  def select[R1, I1: AsExprOrExplode.Of[R1] as asExpr1, R2, I2: AsExprOrExplode.Of[R2] as asExpr2](
      s1: Expr[T] => I1,
      s2: Expr[T] => I2
  ): Dataset[(R1, R2)] = selectN((s1.andThen(asExpr1), s2.andThen(asExpr2)))

  /** Select 3 expressions from the Dataset.
    *
    * For detailed usage see [[select]].
    */
  def select[R1, I1: AsExprOrExplode.Of[R1] as asExpr1, R2, I2: AsExprOrExplode.Of[
    R2
  ] as asExpr2, R3, I3: AsExprOrExplode.Of[R3] as asExpr3](
      s1: Expr[T] => I1,
      s2: Expr[T] => I2,
      s3: Expr[T] => I3
  ): Dataset[(R1, R2, R3)] = selectN((s1.andThen(asExpr1), s2.andThen(asExpr2), s3.andThen(asExpr3)))

  /** Select 4 expressions from the Dataset.
    *
    * For detailed usage see [[select]].
    */
  def select[R1, I1: AsExprOrExplode.Of[R1] as asExpr1, R2, I2: AsExprOrExplode.Of[
    R2
  ] as asExpr2, R3, I3: AsExprOrExplode.Of[R3] as asExpr3, R4, I4: AsExprOrExplode.Of[R4] as asExpr4](
      s1: Expr[T] => I1,
      s2: Expr[T] => I2,
      s3: Expr[T] => I3,
      s4: Expr[T] => I4
  ): Dataset[(R1, R2, R3, R4)] =
    selectN((s1.andThen(asExpr1), s2.andThen(asExpr2), s3.andThen(asExpr3), s4.andThen(asExpr4)))

  /** Select 5 expressions from the Dataset.
    *
    * For detailed usage see [[select]].
    */
  def select[
      R1,
      I1: AsExprOrExplode.Of[R1] as asExpr1,
      R2,
      I2: AsExprOrExplode.Of[R2] as asExpr2,
      R3,
      I3: AsExprOrExplode.Of[R3] as asExpr3,
      R4,
      I4: AsExprOrExplode.Of[R4] as asExpr4,
      R5,
      I5: AsExprOrExplode.Of[R5] as asExpr5
  ](
      s1: Expr[T] => I1,
      s2: Expr[T] => I2,
      s3: Expr[T] => I3,
      s4: Expr[T] => I4,
      s5: Expr[T] => I5
  ): Dataset[(R1, R2, R3, R4, R5)] =
    selectN((
      s1.andThen(asExpr1),
      s2.andThen(asExpr2),
      s3.andThen(asExpr3),
      s4.andThen(asExpr4),
      s5.andThen(asExpr5)
    ))

  /** Select 6 expressions from the Dataset.
    *
    * For detailed usage see [[select]].
    */
  def select[
      R1,
      I1: AsExprOrExplode.Of[R1] as asExpr1,
      R2,
      I2: AsExprOrExplode.Of[R2] as asExpr2,
      R3,
      I3: AsExprOrExplode.Of[R3] as asExpr3,
      R4,
      I4: AsExprOrExplode.Of[R4] as asExpr4,
      R5,
      I5: AsExprOrExplode.Of[R5] as asExpr5,
      R6,
      I6: AsExprOrExplode.Of[R6] as asExpr6
  ](
      s1: Expr[T] => I1,
      s2: Expr[T] => I2,
      s3: Expr[T] => I3,
      s4: Expr[T] => I4,
      s5: Expr[T] => I5,
      s6: Expr[T] => I6
  ): Dataset[(R1, R2, R3, R4, R5, R6)] =
    selectN((
      s1.andThen(asExpr1),
      s2.andThen(asExpr2),
      s3.andThen(asExpr3),
      s4.andThen(asExpr4),
      s5.andThen(asExpr5),
      s6.andThen(asExpr6)
    ))

  /** Select 7 expressions from the Dataset.
    *
    * For detailed usage see [[select]].
    */
  def select[
      R1,
      I1: AsExprOrExplode.Of[R1] as asExpr1,
      R2,
      I2: AsExprOrExplode.Of[R2] as asExpr2,
      R3,
      I3: AsExprOrExplode.Of[R3] as asExpr3,
      R4,
      I4: AsExprOrExplode.Of[R4] as asExpr4,
      R5,
      I5: AsExprOrExplode.Of[R5] as asExpr5,
      R6,
      I6: AsExprOrExplode.Of[R6] as asExpr6,
      R7,
      I7: AsExprOrExplode.Of[R7] as asExpr7
  ](
      s1: Expr[T] => I1,
      s2: Expr[T] => I2,
      s3: Expr[T] => I3,
      s4: Expr[T] => I4,
      s5: Expr[T] => I5,
      s6: Expr[T] => I6,
      s7: Expr[T] => I7
  ): Dataset[(R1, R2, R3, R4, R5, R6, R7)] =
    selectN((
      s1.andThen(asExpr1),
      s2.andThen(asExpr2),
      s3.andThen(asExpr3),
      s4.andThen(asExpr4),
      s5.andThen(asExpr5),
      s6.andThen(asExpr6),
      s7.andThen(asExpr7)
    ))

  private def selectN[Result <: NonEmptyTuple](
      exprs: Tuple.Map[Result, [X] =>> Expr[T] => Expr[X] | ExplodeExpr[X]]
  ): Dataset[Result] = {
    val instances: K0.ProductInstances[CompiledExprOrExplode.From[T], Result] = tupleInstances(exprs)
      .mapK([t] => CompiledExprOrExplode[T, t](_))
    Dataset.SelectN[T, Result](this, instances.toTuple)
  }

  /** Projects the Dataset to a subset defined by the target type `To`.
    *
    * This is a shorthand for applying a projection on each element's `Expr`.
    * For detailed behavior, refer to [[Expr.project]].
    */
  def project[To: {Codec, Projector.From[T] as proj}]: Dataset[To] = select(_.project[To])

  /** Projects the Dataset to a product with the same fields.
    *
    * This is a shorthand for applying a `as` `Expr`. For detailed behavior,
    * refer to [[Expr.as]].
    */
  def as[To: {Codec, Projector.From[T] as proj}](using Projector[To, T]): Dataset[To] = select(_.as[To])

  /** Aggregate the values using a single [[AggregateExpr]].
    *
    * [[AggregateExpr]]s can be created using the methods in the [[aggregates]].
    *
    * Example usage:
    * ```scala
    * import com.choreograph.tyda.aggregates.min
    * val ds: Dataset[(Int, Int)] = ???
    * val agg = ds.aggregate(min(_._1))
    * ```
    *
    * For empty datasets the result will be None.
    */
  def aggregate[R, I: AggregateExpr.AsExpr.Of[R]](e: Expr[T] => I): Dataset.Single[Option[R]] = {
    val compiled = CompiledAggregateExpr(e.andThen(AggregateExpr.AsExpr(_)))
    Dataset.Single.unsafe(Dataset.Aggregate(this, compiled))
  }

  /** Aggregate the values using 2 [[AggregateExpr]]s.
    *
    * For detail usage see [[aggregate]].
    */
  def aggregate[R1, R2](
      e1: Expr[T] => AggregateExpr[R1],
      e2: Expr[T] => AggregateExpr[R2]
  ): Dataset[Option[(R1, R2)]] = aggregateN((e1, e2))

  /** Aggregate the values using 3 [[AggregateExpr]]s.
    *
    * For detail usage see [[aggregate]].
    */
  def aggregate[R1, R2, R3](
      e1: Expr[T] => AggregateExpr[R1],
      e2: Expr[T] => AggregateExpr[R2],
      e3: Expr[T] => AggregateExpr[R3]
  ): Dataset[Option[(R1, R2, R3)]] = aggregateN((e1, e2, e3))

  /** Aggregate the values using 4 [[AggregateExpr]]s.
    *
    * For detail usage see [[aggregate]].
    */
  def aggregate[R1, R2, R3, R4](
      e1: Expr[T] => AggregateExpr[R1],
      e2: Expr[T] => AggregateExpr[R2],
      e3: Expr[T] => AggregateExpr[R3],
      e4: Expr[T] => AggregateExpr[R4]
  ): Dataset[Option[(R1, R2, R3, R4)]] = aggregateN((e1, e2, e3, e4))

  /** Aggregate the values using 5 [[AggregateExpr]]s.
    *
    * For detail usage see [[aggregate]].
    */
  def aggregate[R1, R2, R3, R4, R5](
      e1: Expr[T] => AggregateExpr[R1],
      e2: Expr[T] => AggregateExpr[R2],
      e3: Expr[T] => AggregateExpr[R3],
      e4: Expr[T] => AggregateExpr[R4],
      e5: Expr[T] => AggregateExpr[R5]
  ): Dataset[Option[(R1, R2, R3, R4, R5)]] = aggregateN((e1, e2, e3, e4, e5))

  /** Aggregate the values using 6 [[AggregateExpr]]s.
    *
    * For detail usage see [[aggregate]].
    */
  def aggregate[R1, R2, R3, R4, R5, R6](
      e1: Expr[T] => AggregateExpr[R1],
      e2: Expr[T] => AggregateExpr[R2],
      e3: Expr[T] => AggregateExpr[R3],
      e4: Expr[T] => AggregateExpr[R4],
      e5: Expr[T] => AggregateExpr[R5],
      e6: Expr[T] => AggregateExpr[R6]
  ): Dataset[Option[(R1, R2, R3, R4, R5, R6)]] = aggregateN((e1, e2, e3, e4, e5, e6))

  /** Aggregate the values using 7 [[AggregateExpr]]s.
    *
    * For detail usage see [[aggregate]].
    */
  def aggregate[R1, R2, R3, R4, R5, R6, R7](
      e1: Expr[T] => AggregateExpr[R1],
      e2: Expr[T] => AggregateExpr[R2],
      e3: Expr[T] => AggregateExpr[R3],
      e4: Expr[T] => AggregateExpr[R4],
      e5: Expr[T] => AggregateExpr[R5],
      e6: Expr[T] => AggregateExpr[R6],
      e7: Expr[T] => AggregateExpr[R7]
  ): Dataset[Option[(R1, R2, R3, R4, R5, R6, R7)]] = aggregateN((e1, e2, e3, e4, e5, e6, e7))

  private def aggregateN[Result <: NonEmptyTuple](
      exprs: Tuple.Map[Result, [X] =>> Expr[T] => AggregateExpr[X]]
  ): Dataset[Option[Result]] = Aggregate(this, CompiledAggregateExpr(exprs))

  /** Collect the dataset this to as a sequence.
    */
  def collect: Expr[Seq[T]] = aggregate(aggregates.collect).value.getOrElse(seq())

  /** Count the number of elements in the Dataset.
    */
  def count: Expr[Long] = aggregate(aggregates.count).value.getOrElse(0L)

  /** Return new Dataset by applying a function to each element of the Dataset.
    *
    * If performance is important consider using [[select]] and the expression
    * directly instead.
    */
  def map[U: Codec](f: T => U): Dataset[U] = select(_.udf(f))

  /** Return new Dataset by applying a function to Iterator of the elements.
    *
    * This allows to perform expensive initialize once per partition and/or
    * perform batch logic.
    */
  def mapPartitions[U: Codec](f: Iterator[T] => Iterator[U]): Dataset[U] =
    Dataset.MapPartitions(this, f, Codec[U])

  /** Return new Dataset by applying a function to each element and flattening
    * the result.
    *
    * If performance is important consider using [[select]] with
    * [[functions.explode]] and the expression api directly instead.
    */
  def flatMap[U: Codec](f: T => Iterable[U]): Dataset[U] = {
    given Codec[Iterable[U]] = Codec.iterable
    select(explode(_.udf(f)))
  }

  /** Create a tuple Dataset of the key and the original value.
    *
    * This is a shorthand for `select(key, identity)`.
    */
  def keyBy[I: AsExpr.Of[K], K](key: Expr[T] => I): Dataset[(K, T)] = select(key, identity)

  /** Groups the Dataset to a [[GroupedDataset]] using the fields of the
    * specified named tuple.
    *
    * The [[GroupedDataset]] is used to perform aggregations and equi-joins.
    */
  def groupBy[N <: Tuple, K <: Tuple: Groupable, I <: AnyNamedTuple: AsExpr.Of[NamedTuple[N, K]]](
      key: Expr[T] => I
  ): GroupedDataset[N, K, T] = {
    given Codec[NamedTuple[N, K]] = CompiledExpr(key).codec
    GroupedDataset.FromDataset(this, key.andThen(AsExpr(_)), identity)
  }

  /** Groups the Dataset to a [[GroupedDataset]] using the specified expression.
    *
    * The [[GroupedDataset]] is used to perform aggregations and equi-joins.
    */
  def groupByKey[I: AsExpr.Of[K], K: Groupable](key: Expr[T] => I): GroupedDataset.For[(key: K), T] =
    groupBy(r => (key = key(r)))

  /** Returns a new Dataset without any duplicates.
    */
  def distinct(using Groupable[T]): Dataset[T] = Dataset.Distinct(this)

  /** Limit the number of rows returned from this Dataset.
    *
    * If using the spark backend, a warning will be raised if limit used with a
    * value of `n`.
    *
    * @param n
    *   The maximum number of rows to return. Must be non-negative.
    */
  def limit(n: Int): Dataset[T] = {
    require(n >= 0, s"Limit must be non-negative, got: $n")
    Dataset.Limit(this, n)
  }

  /** Perform a inner join with another [[Dataset]] using the given join
    * condition.
    */
  def join[U, R: AsExpr.Of[Boolean]](
      other: Dataset[U],
      condition: (Expr[T], Expr[U]) => R
  ): Dataset[(T, U)] = {
    given Codec[U] = other.codec
    Dataset.Join(this, other, CompiledExpr2(condition))
  }

  /** Perform a left outer join with another [[Dataset]] using the given join
    * condition.
    */
  def leftOuterJoin[U, R: AsExpr.Of[Boolean]](
      other: Dataset[U],
      p: (Expr[T], Expr[U]) => R
  ): Dataset[(T, Option[U])] = {
    given Codec[U] = other.codec
    Dataset.LeftOuterJoin(this, other, CompiledExpr2(p))
  }

  /** Perform a right outer join with another [[Dataset]] using the given join
    * condition.
    */
  def rightOuterJoin[U, R: AsExpr.Of[Boolean]](
      other: Dataset[U],
      p: (Expr[T], Expr[U]) => R
  ): Dataset[(Option[T], U)] = other.leftOuterJoin(this, (l, r) => p(r, l)).select(_._2, _._1)

  /** Perform a full outer join with another [[Dataset]] using the given join
    * condition.
    */
  def fullOuterJoin[U, R: AsExpr.Of[Boolean]](
      other: Dataset[U],
      p: (Expr[T], Expr[U]) => R
  ): Dataset[(Option[T], Option[U])] = {
    given Codec[U] = other.codec
    Dataset.FullOuterJoin(this, other, CompiledExpr2(p))
  }

  /** Perform a left anti join with another [[Dataset]] using the given
    * condition.
    *
    * A left anti join returns the rows from the left Dataset that have no rows
    * in the right side that match the join condition. In SQL left anti join are
    * usually created using a WHERE NOT EXISTS with a correlated subquery.
    */
  def leftAntiJoin[U, R: AsExpr.Of[Boolean]](other: Dataset[U], p: (Expr[T], Expr[U]) => R): Dataset[T] = {
    given Codec[U] = other.codec
    Dataset.LeftAntiJoin(this, other, CompiledExpr2(p))
  }

  /** Concatenates this Dataset with another Dataset of the same type. Returns a
    * new Dataset containing all elements from both Datasets.
    */
  def union(other: Dataset[T]): Dataset[T] = Dataset.Union(this, other)

  /** Returns a new Dataset containing the rows in this Dataset that are not
    * present in the other Dataset. Duplicate rows are eliminated.
    *
    * This is equivalent to SQL `EXCEPT DISTINCT`.
    */
  def except(other: Dataset[T])(using Groupable[T]): Dataset[T] = leftAntiJoin(other, _ == _).distinct

  /** Proivdes a hint that the Dataset should be cached.
    *
    * There is no corresponding uncache method it expected that the runner will
    * clean after the corresponding Dataset object is garbage collected.
    *
    * Note: This is a hint and the runner may choose to ignore it.
    */
  def cache(): Dataset[T] = Dataset.Cache(this)

  /** Write a Dataset to path using the specified format.
    *
    * Note: This is a lazy operation and the returned [[Action]] needs to be
    * passed to a runner for the data to be written.
    */
  def writeToPath(path: String, format: Format): Action =
    Action.Write(this, if path.endsWith("/") then path else path + "/", format)

  /** Returns an expression that evaluates to true if the dataset contains at
    * least one row, false otherwise. This can be used in subquery expressions.
    *
    * Example:
    * {{{
    *  val ds1: Dataset[Int] = ???
    *  val ds2: Dataset[Int] = ???
    *  ds1.where(_ => ds2.where(_ > 0).exists)
    * }}}
    */
  def exists: Expr[Boolean] = Expr.lift(ExprNode.ExistsSubquery(this.select(_ => 1)))
}

object Dataset {
  sealed trait Action
  private[tyda] object Action {
    final case class Write[T](input: Dataset[T], path: String, format: Format) extends Action

    private val datasetApi = {
      import ExprNode.DatasetLeafs.given
      TreeApi.coproductContainer[Action, Dataset]
    }
    private val exprApi = {
      import ExprNode.ExprNodeLeafs.given
      TreeApi.coproductContainer[Action, ExprNode]
    }

    extension (action: Action) {

      /** Transform the all expression trees from the top down.
        *
        * For details see [[com.choreograph.tyda.TreeApi.transformDown]]
        */
      private[tyda] def transformDownExprs(f: [t] => ExprNode[t] => Control[ExprNode[t]]): Action =
        exprApi.transformDown(action, f)

      /** Transform the expression tree from the bottom up.
        *
        * For details see [[com.choreograph.tyda.TreeApi.transformUp]]
        */
      private[tyda] def transformUpExprs(f: [t] => ExprNode[t] => StopOrContinue[ExprNode[t]]): Action =
        exprApi.transformUp(action, f)

      /** Transform the all expression trees from the bottom up.
        *
        * For details see [[com.choreograph.tyda.TreeApi.transformUp]]
        */
      private[tyda] def transformUp(f: [t] => Dataset[t] => StopOrContinue[Dataset[t]]): Action =
        datasetApi.transformUp(action, f)

      /** Transform the all expression trees from the top down.
        *
        * For details see [[com.choreograph.tyda.TreeApi.transformDown]]
        */
      private[tyda] def transformDown(f: [t] => Dataset[t] => Control[Dataset[t]]): Action =
        datasetApi.transformDown(action, f)

      /** Check if any node in the dataset tree satisfies the predicate `f`.
        *
        * For details see [[com.choreograph.tyda.TreeApi.exists]]
        */
      private[tyda] def exists(f: Dataset[?] => Boolean): Boolean = datasetApi.exists(action, [t] => f(_))
    }
  }

  /** Create a Dataset by reading from a table using its name */
  def readTable[P: Codec, T: Codec](
      identifier: String,
      location: TableLocation = TableLocation.Native
  ): Dataset.Read[(P, T)] = ReadTable(identifier, location, Codec[P], Codec[T])

  /** Create a Dataset by reading from the specified path. */
  def read[T: Codec](
      path: String,
      format: Format,
      unpivot: Boolean,
      filenameGlobFilter: String
  ): Dataset.Read[T] = ReadPath(path, format, unpivot, filenameGlobFilter, Codec[T])

  /** Create a Dataset by reading the specified path with a partition values P
    * and data values T.
    *
    * The expected schema on disk is flat and created by combining the schema of
    * P and T. This is useful for data that is written to fixed values of P, and
    * therefore only specifies T in the written data and P in the path. But when
    * reading the data the combined values are needed.
    *
    * The basePath should be to the start of the partitions in the path. For
    * example
    * ```scala
    * final case class P(p1: String, p2: String)
    * final case class Model(id: Int, value: String)
    * val basePath = "/path/to/data"
    * val path = "/path/to/data/p1={v1,v2}/p2=*"
    * Dataset.readWithHivePartitions[P, Model](
    *   basePath = basePath,
    *   path = path,
    *   format = Format.Parquet,
    *   filenameGlobFilter = "*"
    * )
    * ```
    *
    * Note: This currently does not have support implemented for unpivoting
    * during reading. If that functionality is needed one must use [[read]].
    */
  def readWithHivePartitions[P: Codec, T: Codec](
      basePath: String,
      path: String,
      format: Format,
      filenameGlobFilter: String
  ): Dataset.Read[(P, T)] =
    ReadPathWithHivePartitions[P, T](basePath, path, format, filenameGlobFilter, Codec[P], Codec[T])

  /** Read the partition paths from the specified path.
    *
    * The path is expected to be globbed path to the directory containing data
    * files. For example: calling with the glob path
    * "/data/mytable/year=2023/month=10/day=*" will return all matching
    * partitions paths like: "/data/mytable/year=2023/month=10/day=1",
    * "/data/mytable/year=2023/month=10/day=2", for each path where there the
    * partition directory exists.
    *
    * This is meant to be similar as a `SHOW PARTITIONS` command in Spark SQL,
    * with partition spec included.
    *
    * Note: That after partitions contains only empty data files will be
    * returned. So it is not possible to get the same output by doing
    * ```
    * val path: String = ???
    * readWithMetadata(path).select(_._1.file_path.udf(dirname)).distinct
    * ```
    * as that will not contain the partitions with only empty data files.
    */
  def readPartitionsPaths(path: String): Dataset[String] = ReadPartitionsPaths(path)

  /** Read the partition paths from a table using its name.
    *
    * The is a path with hive partitions. This is the same format as
    * [[readPartitionsPaths]]. But as the table might not be stored in a path
    * there might not have any further interpretation than that it contains the
    * partition values.
    */
  def readTablePartitions(identitifier: String, location: TableLocation): Dataset[String] =
    ReadTablePartitionsPaths(identitifier, location)

  /** Create a Dataset with a single value.
    */
  def single[T: Codec](value: T): Dataset.Single[T] = Dataset.Single.unsafe(FromSeq(Seq(value)))

  /** Create a Dataset from a sequence of values.
    */
  def from[T: Codec](values: Seq[T]): Dataset[T] = FromSeq(values)

  def empty[T: Codec]: Dataset[T] = FromSeq(Seq.empty)

  extension [K: Groupable, V](ds: Dataset[(K, V)]) {
    def grouped: GroupedDataset.For[(key: K), V] = {
      // This is to void this extension needing to have Codecs as context bounds.
      val (keyCodec, valueCodec) = ds.codec.elements
      given Codec[K] = keyCodec
      given Codec[V] = valueCodec
      GroupedDataset.FromDataset(ds, r => namedTuple((key = r._1)), _._2)
    }

    /** Reduce all the values in each group using the provided binary function.
      */
    def reduce(f: (V, V) => V): Dataset[(key: K, value: V)] = ds.grouped.aggregateValue(aggregates.reduce(f))
  }

  extension [K, V](ds: Dataset[(key: K, value: V)]) {

    /** Added for backwards compatablity and calls should likely be removed. */
    def pairs: Dataset[(K, V)] = ds.select(_.key, _.value)

    /** Return a Dataset of only the keys. */
    def keys: Dataset[K] = ds.select(_.key)

    /** Return a Dataset of only the values. */
    def values: Dataset[V] = ds.select(_.value)
  }

  extension [K, V](ds: Dataset[(K, V)]) {

    /** Added for backwards compatablity and calls to can be removed. */
    @targetName("pairsTuple")
    def pairs: Dataset[(K, V)] = ds

    /** Return a Dataset of only the keys. */
    @targetName("keysTuple")
    def keys: Dataset[K] = ds.select(_._1)

    /** Return a Dataset of only the values. */
    @targetName("valueTuple")
    def values: Dataset[V] = ds.select(_._2)

    /** Select a new value while keeping the key the same */
    def selectValues[U, I: AsExprOrExplode.Of[U]](f: Expr[V] => I): Dataset[(K, U)] =
      ds.select(_._1, v => f(v._2))

    /** Create a new [[Dataset]] by applying the provided function to each
      * value.
      *
      * Note: If performance is important consider using [[selectValues]] with
      * the expression api directly instead.
      */
    def mapValues[U: Codec](f: V => U): Dataset[(K, U)] = selectValues(_.udf(f))

    /** Create a new [[Dataset]] by applying the provided function to each value
      * and flattening the result.
      *
      * Note: If performance is important consider using [[selectValues]] with
      * [[functions.explode]] and the expression api directly instead.
      */
    def flatMapValues[U: Codec](f: V => Iterable[U]): Dataset[(K, U)] = {
      given Codec[Iterable[U]] = Codec.iterable
      selectValues(explode(_.udf(f)))
    }

    /** Perform an inner equi-join on the key with another GroupedDataset. */
    def join[U](rhs: Dataset[(K, U)]): Dataset[(K, (V, U))] =
      ds.join(rhs, _._1 == _._1).select(_._1._1, v => (v._1._2, v._2._2))

    /** Perform a left outer equi-join on the key with another GroupedDataset.
      */
    def leftOuterJoin[U](rhs: Dataset[(K, U)]): Dataset[(K, (V, Option[U]))] =
      ds.leftOuterJoin(rhs, _._1 == _._1).select(_._1._1, v => (v._1._2, v._2.map(_._2)))

    /** Perform a left anti equi-join on the key with another GroupedDataset.
      * Returns values from the left Dataset whose keys have no match in the
      * right Dataset.
      *
      * For more information on left anti joins see: [[Dataset.leftAntiJoin]].
      */
    def leftAntiJoin[U](other: Dataset[(K, U)]): Dataset[(K, V)] =
      ds.leftAntiJoin(other.select(_._1), (l, r) => l._1 == r)

    /** Perform a right outer equi-join on the key with another GroupedDataset.
      */
    def rightOuterJoin[U](rhs: Dataset[(K, U)]): Dataset[(K, (Option[V], U))] =
      ds.rightOuterJoin(rhs, _._1 == _._1).select(_._2._1, v => (v._1.map(_._2), v._2._2))

    /** Perform a full outer equi-join on the key with another GroupedDataset.
      */
    def fullOuterJoin[U](rhs: Dataset[(K, U)]): Dataset[(K, (Option[V], Option[U]))] =
      ds.fullOuterJoin(rhs, _._1 == _._1)
        .select(
          v => knownNotNull(coalesce(v._1.map(_._1), v._2.map(_._1))),
          v => (v._1.map(_._2), v._2.map(_._2))
        )

  }

  extension [T: Mirror.ProductOf as m](ds: Dataset[T]) {

    /** Perform a inner join and unnest the result into a single NamedTuple.
      *
      * This is meant to be similar to an SQL join where fields can be accessed
      * directly.
      *
      * Example:
      * ```scala
      * val ds1: Dataset[(id: Int, name: String)] = Dataset.from(Seq((1, "a"), (2, "b")))
      * val ds2: Dataset[(externalId: Int, value: Double)] = Dataset.from(Seq((1, 10.0), (3, 30.0)))
      * val joined: Dataset[(String, Double)] = ds1
      *   .joinFlat(ds2, (e1, e2) => e1.id == e2.externalId)
      *   .select(_.name, _.value)
      * // result: Seq(("a", 10.0))
      * ```
      */
    def joinFlat[U: Mirror.ProductOf as m2](
        other: Dataset[U],
        condition: (Expr[T], Expr[U]) => Expr[Boolean]
    )(using
        Tuple.Disjoint[m.MirroredElemLabels, m2.MirroredElemLabels] =:= true
    ): Dataset[NamedTuple.Concat[
      NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes],
      NamedTuple[m2.MirroredElemLabels, m2.MirroredElemTypes]
    ]] = ds.join(other, condition).unnest

    /** Perform a left outer join and unnest the result into a single
      * NamedTuple.
      *
      * All the fields from the `other` Dataset will be wrapped in Option and be
      * `None` where there was no match.
      *
      * Example:
      * ```scala
      * val ds1: Dataset[(id: Int, name: String)] = Dataset.from(Seq((1, "a"), (2, "b")))
      * val ds2: Dataset[(externalId: Int, value: Double)] = Dataset.from(Seq((1, 10.0), (3, 30.0)))
      * val joined: Dataset[(String, Option[Double])] = ds1
      *   .leftJoinFlat(ds2, (e1, e2) => e1.id == e2.externalId)
      *   .select(_.name, _.value)
      * // result: Seq(("a", Some(10.0)), ("b", None))
      * ```
      */
    def leftJoinFlat[U: Mirror.ProductOf as m2](
        other: Dataset[U],
        condition: (Expr[T], Expr[U]) => Expr[Boolean]
    )(using
        Tuple.Disjoint[m.MirroredElemLabels, m2.MirroredElemLabels] =:= true
    ): Dataset[NamedTuple.Concat[
      NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes],
      NamedTuple.Map[NamedTuple[m2.MirroredElemLabels, m2.MirroredElemTypes], Option]
    ]] =
      ds.leftOuterJoin(other, condition)
        .select { case Expr(p1, p2) => p1.toNamedTuple ++ p2.map(_.toNamedTuple).spreadOption }

    /** Perform a right outer join and unnest the result into a single
      * NamedTuple.
      *
      * All the fields from the this Dataset will be wrapped in Option and be
      * `None` where there was no match.
      *
      * Example:
      * ```scala
      * val ds1: Dataset[(id: Int, name: String)] = Dataset.from(Seq((1, "a"), (2, "b")))
      * val ds2: Dataset[(externalId: Int, value: Double)] = Dataset.from(Seq((1, 10.0), (3, 30.0)))
      * val joined: Dataset[(Option[String], Double)] = ds1
      *   .rightJoinFlat(ds2, (e1, e2) => e1.id == e2.externalId)
      *   .select(_.name, _.value)
      * // result: Seq((Some("a"), 10.0), (None, 30.0))
      * ```
      */
    def rightJoinFlat[U: Mirror.ProductOf as m2](
        other: Dataset[U],
        condition: (Expr[T], Expr[U]) => Expr[Boolean]
    )(using
        Tuple.Disjoint[m.MirroredElemLabels, m2.MirroredElemLabels] =:= true
    ): Dataset[NamedTuple.Concat[
      NamedTuple.Map[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes], Option],
      NamedTuple[m2.MirroredElemLabels, m2.MirroredElemTypes]
    ]] =
      ds.rightOuterJoin(other, condition)
        .select { case Expr(p1, p2) => p1.map(_.toNamedTuple).spreadOption ++ p2.toNamedTuple }

    /** Perform a full outer join and unnest the result into a single
      * NamedTuple.
      *
      * All the fields from both Datasets will be wrapped in Option and be
      * `None` where there was no match.
      *
      * Example:
      * ```scala
      * val ds1: Dataset[(id: Int, name: String)] = Dataset.from(Seq((1, "a"), (2, "b")))
      * val ds2: Dataset[(externalId: Int, value: Double)] = Dataset.from(Seq((1, 10.0), (3, 30.0)))
      * val joined: Dataset[(Option[String], Option[Double])] = ds1
      *   .fullJoinFlat(ds2, (e1, e2) => e1.id == e2.externalId)
      *   .select(_.name, _.value)
      * // result: Seq((Some("a"), Some(10.0)), (Some("b"), None), (None, Some(30.0)))
      * ```
      */
    def fullJoinFlat[U: Mirror.ProductOf as m2](
        other: Dataset[U],
        condition: (Expr[T], Expr[U]) => Expr[Boolean]
    )(using
        Tuple.Disjoint[m.MirroredElemLabels, m2.MirroredElemLabels] =:= true
    ): Dataset[NamedTuple.Concat[
      NamedTuple.Map[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes], Option],
      NamedTuple.Map[NamedTuple[m2.MirroredElemLabels, m2.MirroredElemTypes], Option]
    ]] =
      ds.fullOuterJoin(other, condition)
        .select { case Expr(p1, p2) =>
          p1.map(_.toNamedTuple).spreadOption ++ p2.map(_.toNamedTuple).spreadOption
        }
  }

  extension [V1: Mirror.ProductOf as m1, V2: Mirror.ProductOf as m2](
      ds: Dataset[(V1, V2)]
  )(using Tuple.Disjoint[m1.MirroredElemLabels, m2.MirroredElemLabels] =:= true)
    /** Unnest the Tuple2 by converting the elements to NamedTuples and
      * combining them.
      */
    private def unnest: Dataset[NamedTuple.Concat[
      NamedTuple[m1.MirroredElemLabels, m1.MirroredElemTypes],
      NamedTuple[m2.MirroredElemLabels, m2.MirroredElemTypes]
    ]] = ds.select { case Expr(v1, v2) => v1.toNamedTuple ++ v2.toNamedTuple }

  extension [T, R](compiled: CompiledExprOrExplode[T, R]) {
    def codec: Codec[R] =
      compiled match {
        case c: CompiledExpr[T, R] => c.codec
        case c: CompiledExplodeExpr[T, R] => c.codec
      }
  }

  /** Trait for Dataset that is created directly from reading from storage.
    *
    * This allows to call `withMetadata` to get a Dataset that includes the
    * FileMetadata for the read files.
    */
  type Read[T] = Dataset[T] & ReadMixin[T]

  trait ReadMixin[T] {
    self: Dataset[T] =>
    def withMetadata: Dataset[(FileMetadata, T)] = ReadWithMetadata(this)
  }

  opaque type Single[T] <: Dataset[T] = Dataset[T]

  object Single {

    /** It up to the caller to ensure that the Dataset only contains a single
      * value.
      *
      * THis should never be expose outside of Tyda.
      */
    private[Dataset] def unsafe[T](ds: Dataset[T]): Single[T] = ds
    extension [T](ds: Single[T]) { def value: Expr[T] = Expr.lift(ExprNode.ScalarSubquery(ds)) }
  }

  private[tyda] final case class ReadPathWithHivePartitions[P, T](
      basePath: String,
      path: String,
      format: Format,
      filenameGlobFilter: String,
      partitionCodec: Codec[P],
      modelCodec: Codec[T]
  ) extends ReadMixin[(P, T)], Dataset[(P, T)](using tuple2Codec(using partitionCodec, modelCodec))
  private[tyda] final case class ReadPath[T](
      path: String,
      format: Format,
      unpivot: Boolean,
      filenameGlobFilter: String,
      override val codec: Codec[T]
  ) extends ReadMixin[T], Dataset(using codec)

  private[tyda] final case class ReadTable[P, T](
      identifier: String,
      location: TableLocation,
      partitionCodec: Codec[P],
      modelCodec: Codec[T]
  ) extends ReadMixin[(P, T)], Dataset[(P, T)](using tuple2Codec(using partitionCodec, modelCodec))

  private[tyda] final case class ReadWithMetadata[T](read: Dataset.Read[T])
      extends Dataset[(FileMetadata, T)](using tuple2Codec(using summon, read.codec))
  private[tyda] final case class FromSeq[T](values: Seq[T], override val codec: Codec[T])
      extends Dataset[T](using codec)
  private[tyda] object FromSeq {
    def apply[T: Codec](values: Seq[T]): Dataset[T] = FromSeq(values, Codec[T])
  }
  private[tyda] final case class ReadPartitionsPaths(path: String) extends Dataset[String]
  private[tyda] final case class ReadTablePartitionsPaths(identifier: String, location: TableLocation)
      extends Dataset[String]
  private[tyda] final case class Filter[T](input: Dataset[T], p: CompiledExpr[T, Boolean])
      extends Dataset[T](using input.codec)
  private[tyda] final case class Select1[T, R](input: Dataset[T], expr: CompiledExprOrExplode[T, R])
      extends Dataset[R](using expr.codec)
  private[tyda] final case class SelectN[T, R <: Tuple](
      input: Dataset[T],
      exprs: Tuple.Map[R, CompiledExprOrExplode.From[T]]
  ) extends Dataset[R](using Codec.tuple(tupleInstances(exprs).mapK([t] => _.codec)))
  private[tyda] final case class MapPartitions[T, U](
      input: Dataset[T],
      f: Iterator[T] => Iterator[U],
      override val codec: Codec[U]
  ) extends Dataset[U](using codec)
  private[tyda] final case class Distinct[T](input: Dataset[T]) extends Dataset[T](using input.codec)
  private[tyda] final case class Join[T, U](
      left: Dataset[T],
      right: Dataset[U],
      p: CompiledExpr2[T, U, Boolean]
  ) extends Dataset[(T, U)](using tuple2Codec(using left.codec, right.codec))

  private[tyda] final case class LeftOuterJoin[T, U](
      left: Dataset[T],
      right: Dataset[U],
      p: CompiledExpr2[T, U, Boolean]
  ) extends Dataset[(T, Option[U])](using leftOuterCodec(using left.codec, right.codec))

  private[tyda] final case class FullOuterJoin[T, U](
      left: Dataset[T],
      right: Dataset[U],
      p: CompiledExpr2[T, U, Boolean]
  ) extends Dataset[(Option[T], Option[U])](using fullOuterCodec(using left.codec, right.codec))

  private[tyda] final case class LeftAntiJoin[T, U](
      left: Dataset[T],
      right: Dataset[U],
      p: CompiledExpr2[T, U, Boolean]
  ) extends Dataset[T](using left.codec)

  private[tyda] final case class Aggregate[V, R](input: Dataset[V], agg: CompiledAggregateExpr[V, R])
      extends Dataset[Option[R]](using Codec.option(using agg.codec))

  private[tyda] final case class GroupedAggregate[V, K <: AnyNamedTuple, R <: AnyNamedTuple](
      input: Dataset[V],
      key: CompiledExpr[V, K],
      agg: CompiledAggregateExpr[V, R]
  ) extends Dataset[(K, R)](using tuple2Codec(using key.codec, agg.codec))

  private[tyda] final case class Union[T](left: Dataset[T], right: Dataset[T])
      extends Dataset[T](using left.codec)
  private[tyda] final case class Cache[T](input: Dataset[T]) extends Dataset[T](using input.codec)
  private[tyda] final case class Limit[T](input: Dataset[T], n: Int) extends Dataset[T](using input.codec)

  private def tuple2Codec[A: Codec, B: Codec]: Codec[(A, B)] = summon
  private def leftOuterCodec[A: Codec, B: Codec]: Codec[(A, Option[B])] = summon
  private def fullOuterCodec[A: Codec, B: Codec]: Codec[(Option[A], Option[B])] = summon

  extension [T](ds: Dataset[T]) {

    /** Transform the all expression trees from the top down.
      *
      * For details see [[com.choreograph.tyda.TreeApi.transformDown]]
      */
    private[tyda] def transformDownExprs(f: [t] => ExprNode[t] => Control[ExprNode[t]]): Dataset[T] =
      exprApi.transformDown(ds, f)

    /** Transform the expression tree from the bottom up.
      *
      * For details see [[com.choreograph.tyda.TreeApi.transformUp]]
      */
    private[tyda] def transformUpExprs(f: [t] => ExprNode[t] => StopOrContinue[ExprNode[t]]): Dataset[T] =
      exprApi.transformUp(ds, f)

    /** Transform the all expression trees from the bottom up.
      *
      * For details see [[com.choreograph.tyda.TreeApi.transformUp]]
      */
    private[tyda] def transformUp(f: [t] => Dataset[t] => StopOrContinue[Dataset[t]]): Dataset[T] =
      api.transformUp(ds, f)

    /** Transform the all expression trees from the top down.
      *
      * For details see [[com.choreograph.tyda.TreeApi.transformDown]]
      */
    private[tyda] def transformDown(f: [t] => Dataset[t] => Control[Dataset[t]]): Dataset[T] =
      api.transformDown(ds, f)

    /** Check if any node in the dataset tree satisfies the predicate `f`.
      *
      * For details see [[com.choreograph.tyda.TreeApi.exists]]
      */
    private[tyda] def exists(f: Dataset[?] => Boolean): Boolean = api.exists(ds, [t] => f(_))
  }

  /* This is a val so that there is only one instance of TreeApi[Dataset[T], ExprNode] created and reused for
   * all T */
  private val cachedExprApi: TreeApi[Dataset[Any], ExprNode] = {
    import ExprNode.ExprNodeLeafs.given
    given compiledOrExplode[T, R]: TreeApi[CompiledExprOrExplode[T, R], ExprNode] = {
      import UnionMirror.derived
      TreeApi.coproductContainer
    }
    given selectN[T, R <: Tuple]: TreeApi[Tuple.Map[R, CompiledExprOrExplode.From[T]], ExprNode] =
      TreeApi.mappedTuple([t] => () => compiledOrExplode)
    TreeApi.coproductContainer
  }

  // TYPE SAFETY: The behavior of TreeApi[Dataset[T], ExprNode] does not depend on T
  private[tyda] given exprApi[T]: TreeApi[Dataset[T], ExprNode] = cachedExprApi.asInstanceOf

  private val cachedApi: TreeApi[Dataset[Any], Dataset] = {
    import ExprNode.DatasetLeafs.given
    given compiledOrExplode[T, R]: TreeApi[CompiledExprOrExplode[T, R], Dataset] = {
      import UnionMirror.derived
      TreeApi.coproductContainer
    }
    given selectN[T, R <: Tuple]: TreeApi[Tuple.Map[R, CompiledExprOrExplode.From[T]], Dataset] =
      TreeApi.mappedTuple([t] => () => compiledOrExplode)
    TreeApi.coproduct
  }

  // TYPE SAFETY: The behavior of TreeApi[Dataset[T], Dataset] does not depend on T
  private[tyda] given api[T]: TreeApi[Dataset[T], Dataset] = cachedApi.asInstanceOf
}
