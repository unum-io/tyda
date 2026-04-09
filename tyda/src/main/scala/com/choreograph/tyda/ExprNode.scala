package com.choreograph.tyda

import scala.NamedTuple.AnyNamedTuple

import shapeless3.deriving.Complete

import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Control
import com.choreograph.tyda.TreeApi.StopOrContinue
import com.choreograph.tyda.shapeless3extras.tupleInstances

private sealed trait ExprNode[T] extends Selectable {
  type Fields = NamedTuple.Map[NamedTuple.From[T], ExprNode]

  def selectDynamic(fieldName: String) = ExprNode.Select(this, fieldName)

  def codec: Codec[T]

  /** Check if any node in the expression tree satisfies the predicate `f`.
    *
    * For details see [[com.choreograph.tyda.TreeApi.exists]]
    */
  def exists(f: ExprNode[?] => Boolean): Boolean = ExprNode.api.exists(this, [t] => f(_))

  /** Check if all nodes in the expression tree satisfy the predicate `f`.
    *
    * For details see [[com.choreograph.tyda.TreeApi.forall]]
    */
  def forall(f: ExprNode[?] => Boolean): Boolean = ExprNode.api.forall(this, [t] => f(_))

  /** Fold over the expression tree.
    *
    * For details see [[com.choreograph.tyda.TreeApi.fold]]
    */
  def fold[U](z: U)(f: (U, ExprNode[?]) => Control[U]): U =
    ExprNode.api.fold(this)(z)([t] => (acc, node) => f(acc, node))

  /** Collect all nodes in the expression using a partial function.
    *
    * For details see [[com.choreograph.tyda.TreeApi.collect]]
    */
  def collect[A](f: PartialFunction[ExprNode[?], A]): Seq[A] = {
    val lifted = f.lift
    ExprNode.api.collect(this, [t] => lifted(_))
  }

  /** Transform the expression tree from the bottom up.
    *
    * For details see [[com.choreograph.tyda.TreeApi.transformUp]]
    */
  def transformUp(f: [t] => ExprNode[t] => StopOrContinue[ExprNode[t]]): ExprNode[T] =
    ExprNode.api.transformUp(this, f)

  /** Transform the expression tree from the top down.
    *
    * For details see [[com.choreograph.tyda.TreeApi.transformDown]]
    */
  def transformDown(f: [t] => ExprNode[t] => Control[ExprNode[t]]): ExprNode[T] =
    ExprNode.api.transformDown(this, f)

  /** Transform the expression tree from the bottom up while accumulating a
    * value of type `Acc`.
    *
    * For details see [[com.choreograph.tyda.TreeApi.transformAccumulateUp]]
    */
  def transformAccumulateDown[Acc](initial: Acc)(
      f: [t] => (Acc, ExprNode[t]) => Control[(Acc, ExprNode[t])]
  ): (Acc, ExprNode[T]) = ExprNode.api.transformAccumulateDown(initial, this)(f)

  /** Replace all occurrences of `from` with `to` in the expression tree.
    */
  def replace[U](from: ExprNode[U], to: ExprNode[U]): ExprNode[T] =
    ExprNode
      .api
      .transformUp(
        this,
        [t] =>
          _ match {
            case `from` => Continue(to)
            case other => Continue(other)
          }
      )
}

private object ExprNode extends ExprApi[ExprNode] {
  private[tyda] def lift[T](e: ExprNode[T]): ExprNode[T] = e
  private[tyda] def unlift[T](e: ExprNode[T]): ExprNode[T] = e

  final case class Reference[T](id: ReferenceId = ReferenceId(), override val codec: Codec[T])
      extends ExprNode[T]

  object Reference {
    def apply[T: Codec](): ExprNode.Reference[T] = new Reference(codec = Codec[T])
  }

  final case class Select[P, T](operand: ExprNode[P], name: String) extends ExprNode[T] {
    override def codec: Codec[T] =
      operand.codec match {
        case prod: Codec.Product[P] =>
          val field = prod
            .fields
            .foldLeft0[Option[Field[?]]](scala.None)([t] =>
              (acc: Option[Field[?]], f: Field[t]) => if f.name == name then Complete(Option(f)) else acc
            )
            .getOrElse(unreachable(s"Unable to resolve field $name on codec $prod"))
          // TYPE SAFETY: The names are checked at compile time using type Fields on ExprNode[T]
          field.codec.asInstanceOf[Codec[T]]
        case unsupported => unreachable(s"Select only supported on product types ${unsupported}")
      }
  }

  final case class MakeProduct[P, Elems <: Tuple](
      values: Tuple.Map[Elems, ExprNode],
      override val codec: Codec.Product[P]
  ) extends ExprNode[P]

  def makeTuple[T <: Tuple](values: Tuple.Map[T, ExprNode]): ExprNode[T] =
    new MakeProduct[T, T](values, Codec.tuple(tupleInstances(values).mapK([t] => _.codec)))

  def makeNamedTuple[NT <: AnyNamedTuple](values: NamedTuple.Map[NT, ExprNode])(using
      StringLiterals[NamedTuple.Names[NT]]
  ): ExprNode[NT] = new MakeProduct(values, Codec.namedTuple(tupleInstances(values).mapK([t] => _.codec)))

  def makeSumUnsafe[S, Repr <: AnyNamedTuple & Product](
      exprs: Seq[ExprNode[?]],
      codec: Codec.Sum[S, Repr]
  ): ExprNode[S] = ExprNode.FromRepr(makeProductUnsafe(exprs, codec.to), codec)

  def makeProductUnsafe[P](exprs: Seq[ExprNode[?]], codec: Codec.Product[P]): ExprNode[P] =
    // TYPE SAFETY: All the values in exprs are of type ExprNode[?]
    new MakeProduct(Tuple.fromArray(exprs.toArray).asInstanceOf, codec)

  final case class Range(start: ExprNode[Int], end: ExprNode[Int]) extends ExprNode[Seq[Int]] {
    override def codec: Codec[Seq[Int]] = Codec[Seq[Int]]
  }

  final case class MakeSeq[T](values: Seq[ExprNode[T]], valueCodec: Codec[T]) extends ExprNode[Seq[T]] {
    override val codec: Codec[Seq[T]] = {
      given Codec[T] = valueCodec
      Codec[Seq[T]]
    }
  }

  final case class ConcatSeq[T](lhs: ExprNode[Seq[T]], rhs: ExprNode[Seq[T]]) extends ExprNode[Seq[T]] {
    override def codec: Codec[Seq[T]] = lhs.codec
  }

  object MakeSeq {
    def apply[T: Codec](values: Seq[ExprNode[T]]): ExprNode[Seq[T]] = new MakeSeq(values, Codec[T])
  }

  final case class MapSeq[T, U](operand: ExprNode[Seq[T]], f: CompiledExpr[T, U]) extends ExprNode[Seq[U]] {
    override def codec: Codec[Seq[U]] = {
      given Codec[U] = f.codec
      Codec[Seq[U]]
    }
  }

  final case class FilterSeq[T](operand: ExprNode[Seq[T]], predicate: CompiledExpr[T, Boolean])
      extends ExprNode[Seq[T]] {
    override def codec: Codec[Seq[T]] = operand.codec
  }

  final case class AggregateSeq[T, U](
      operand: ExprNode[Seq[T]],
      onEmpty: ExprNode[U],
      agg: PrimitiveAggregate[T, U] & AggregateSeq.SupportedAggregates
  ) extends ExprNode[U] {
    override def codec: Codec[U] = agg.codec
  }
  object AggregateSeq {

    /** Currently we restrict the supported aggregates to what used in public
      * apis. For adding a new aggregate it must be possible transform it into a
      * fold like operation as used by Spark. This is currently done in
      * PrimitiveAggregateAsFold.
      */
    type SupportedAggregates = PrimitiveAggregate.BoolAnd | PrimitiveAggregate.BoolOr
  }

  final case class MakeMap[K, V](entries: ExprNode[Seq[(K, V)]]) extends ExprNode[Map[K, V]] {
    def codec: Codec[Map[K, V]] = {
      val codecs = entries.codec.element.elements
      given Codec[K] = codecs(0)
      given Codec[V] = codecs(1)
      Codec[Map[K, V]]
    }
  }
  final case class MapEntries[K, V](map: ExprNode[Map[K, V]]) extends ExprNode[Seq[(key: K, value: V)]] {
    def codec: Codec[Seq[(key: K, value: V)]] =
      map.codec match {
        case Codec.Map(given Codec[K], given Codec[V]) => summon
        case unsupported => unreachable(s"MapEntries only supported on map types ${unsupported}")
      }
  }
  final case class MapGet[K, V](map: ExprNode[Map[K, V]], key: ExprNode[K]) extends ExprNode[Option[V]] {
    def codec: Codec[Option[V]] =
      map.codec match {
        case Codec.Map(_, given Codec[V]) => Codec[Option[V]]
        case unsupported => unreachable(s"MapGet only supported on map types ${unsupported}")
      }
  }

  final case class Literal[T](value: T, override val codec: Codec.Primitive[T]) extends ExprNode[T]

  object Literal {
    def apply[T: Codec](value: T): ExprNode[T] = create(value, Codec[T])

    def create[T](literal: T, literalCodec: Codec[T]): ExprNode[T] =
      literalCodec match {

        case codec: Codec.Primitive[T] => ExprNode.Literal(literal, codec)

        case Codec.Map(given Codec[k], given Codec[v]) =>
          ExprNode.MakeMap(create(literal.toSeq, Codec.Seq[(k, v)](Codec[(k, v)])))

        case Codec.Seq(element) => ExprNode.MakeSeq(literal.map(create(_, element)), element)

        case Codec.Option(element) => literal match {
            case Some(value) => ExprNode.MakeSome(create(value, element))
            case scala.None => ExprNode.None(element)
          }

        case codec @ Codec.Product(_, fields, _) =>
          val values =
            fields.foldLeft(literal)(Seq.empty[ExprNode[?]])([s] => (acc, f, e) => acc :+ create(e, f.codec))
          ExprNode.makeProductUnsafe(values, codec)

        case injectionCodec @ Codec.FromInjection(inj, codec) =>
          ExprNode.FromRepr(create(inj(literal), codec), injectionCodec)
      }
  }

  final case class Not(operand: ExprNode[Boolean]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class Or(lhs: ExprNode[Boolean], rhs: ExprNode[Boolean]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class And(lhs: ExprNode[Boolean], rhs: ExprNode[Boolean]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class Equals[T](lhs: ExprNode[T], rhs: ExprNode[T]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class LessThan[T](comparable: Comparable[T], lhs: ExprNode[T], rhs: ExprNode[T])
      extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class LessThanOrEqual[T](comparable: Comparable[T], lhs: ExprNode[T], rhs: ExprNode[T])
      extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class Udf[T, U](operand: ExprNode[T], f: T => U, override val codec: Codec[U])
      extends ExprNode[U]

  object Udf {
    def apply[T, U: Codec](operand: ExprNode[T], f: T => U): ExprNode[U] = new Udf(operand, f, Codec[U])
  }

  /* The `?` in `Iterable[?]` is really T but that runs into this scala3 compiler bug:
   * https://github.com/scala/scala3/issues/23774 When that is fixed we should update the type upper bound of
   * `C` and removed the custom apply and unapply methods. */
  final case class UpcastToIterable[T, C <: Iterable[?]] private (operand: ExprNode[C])
      extends ExprNode[Iterable[T]] {
    override def codec: Codec[Iterable[T]] =
      // TYPE SAFETY: The public constructor enforces that C <: Iterable[T]
      Codec.iterable(using operand.codec.element.asInstanceOf[Codec[T]])
  }

  object UpcastToIterable {
    def apply[T, C <: Iterable[T]](operand: ExprNode[C]): ExprNode[Iterable[T]] =
      new UpcastToIterable(operand)
    def unapply[T, C <: Iterable[T]](expr: ExprNode.UpcastToIterable[T, C]): Tuple1[ExprNode[C]] =
      Tuple1(expr.operand)
  }

  final case class OptionToIterable[T](operand: ExprNode[Option[T]]) extends ExprNode[Iterable[T]] {
    override def codec: Codec[Iterable[T]] = Codec.iterable(using operand.codec.element)
  }

  final case class MakeSome[T](value: ExprNode[T]) extends ExprNode[Option[T]] {
    override def codec: Codec[Option[T]] = {
      given Codec[T] = value.codec
      Codec[Option[T]]
    }
  }

  final case class None[T](element: Codec[T]) extends ExprNode[Option[T]] {
    override def codec: Codec[Option[T]] = Codec.option(using element)
  }

  final case class Coalesce[T](operands: Seq[ExprNode[Option[T]]]) extends ExprNode[Option[T]] {
    override def codec: Codec[Option[T]] = operands.head.codec
  }

  final case class RaiseError[T](message: ExprNode[String], override val codec: Codec[T]) extends ExprNode[T]

  object RaiseError {
    def apply[T: Codec](message: ExprNode[String]): ExprNode[T] = new RaiseError(message, summon[Codec[T]])
  }

  final case class ScalarSubquery[T](ds: Dataset[T]) extends ExprNode[T] {
    override def codec: Codec[T] = ds.codec
  }

  final case class ExistsSubquery[T](ds: Dataset[T]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  /** An AggregateExpr that applies the primitive `agg` to `arg`.
    */
  private[tyda] final case class Aggregate[From, To](arg: ExprNode[From], agg: PrimitiveAggregate[From, To])
      extends ExprNode[To] {
    override def codec: Codec[To] = agg.codec
  }

  final case class WhenThen[T](whenExpr: ExprNode[Boolean], thenExpr: ExprNode[T]) {
    def asOption: WhenThen[Option[T]] = WhenThen(whenExpr, MakeSome(thenExpr))
  }

  final case class Cases[T](whenThenExpr: WhenThen[T], whenThenExprs: Seq[WhenThen[T]], elseExpr: ExprNode[T])
      extends ExprNode[T] {
    override def codec: Codec[T] = whenThenExpr.thenExpr.codec
  }

  object Cases {
    def apply[T](whenThen: WhenThen[T], whenThens: Seq[WhenThen[T]]): ExprNode[Option[T]] =
      Cases(whenThen.asOption, whenThens.map(_.asOption), None(whenThen.thenExpr.codec))

    def ternary[T](condition: ExprNode[Boolean], ifTrue: ExprNode[T], ifFalse: ExprNode[T]): ExprNode[T] =
      Cases(WhenThen(condition, ifTrue), Seq.empty, ifFalse)
  }

  /* Internal expr used to turn a `Option[T]` into a `T` when we know it's not None.
   *
   * This is used in GroupedDataset.fullOuterJoin where the `coalesce(l.key, r.key)` must not be null, but
   * it's not captured in types. */
  final case class KnownNotNull[T](expr: ExprNode[Option[T]]) extends ExprNode[T] {
    override def codec: Codec[T] = expr.codec.element
  }

  final case class Split(string: ExprNode[String], delimiter: ExprNode[String])
      extends ExprNode[Seq[String]] {
    override def codec: Codec[Seq[String]] = Codec[Seq[String]]
  }

  final case class StartsWith(string: ExprNode[String], prefix: ExprNode[String]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class Trim(string: ExprNode[String]) extends ExprNode[String] {
    override def codec: Codec[String] = Codec[String]
  }

  final case class EndsWith(string: ExprNode[String], suffix: ExprNode[String]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class ConcatString(strings: Seq[ExprNode[String]]) extends ExprNode[String] {
    override def codec: Codec[String] = Codec[String]
  }

  /** Generates a random Double in [0.0, 1.0). */
  final case class Rand() extends ExprNode[Double] {
    override def codec: Codec[Double] = Codec[Double]
  }

  final case class IsNaN[T <: Float | Double](operand: ExprNode[T]) extends ExprNode[Boolean] {
    override def codec: Codec[Boolean] = Codec[Boolean]
  }

  final case class ToJson[T](expr: ExprNode[T]) extends ExprNode[String] {
    override def codec: Codec[String] = Codec[String]
  }

  final case class FromJson[T](expr: ExprNode[String], valueCodec: Codec[T]) extends ExprNode[Option[T]] {
    override def codec: Codec[Option[T]] = Codec.option(using valueCodec)
  }

  final case class DistinctSeq[T](operand: ExprNode[Seq[T]]) extends ExprNode[Seq[T]] {
    override def codec: Codec[Seq[T]] = operand.codec
  }

  final case class SizeSeq[C <: Seq[?]](operand: ExprNode[C]) extends ExprNode[Int] {
    override def codec: Codec[Int] = Codec[Int]
  }

  final case class ElementSeq[T](array: ExprNode[Seq[T]], index: ExprNode[Int]) extends ExprNode[T] {
    override def codec: Codec[T] = array.codec.element
  }

  final case class Add[T](additive: AdditiveExpr[T], lhs: ExprNode[T], rhs: ExprNode[T]) extends ExprNode[T] {
    override def codec: Codec[T] = lhs.codec
  }

  final case class Quotient[T](integral: Integral[T], lhs: ExprNode[T], rhs: ExprNode[T])
      extends ExprNode[T] {
    override def codec: Codec[T] = lhs.codec
  }

  final case class Cast[T, U](expr: ExprNode[T], canCast: CanCast[T, U]) extends ExprNode[U] {
    override def codec: Codec[U] = canCast.codec
  }
  final case class TryCast[T, U](expr: ExprNode[T], canTryCast: CanTryCast[T, U])
      extends ExprNode[Option[U]] {
    override def codec: Codec[Option[U]] = {
      given Codec[U] = canTryCast.codec
      Codec[Option[U]]
    }
  }

  final case class TimestampToMicros(expr: ExprNode[Timestamp]) extends ExprNode[Long] {
    override def codec: Codec[Long] = Codec[Long]
  }

  final case class MicrosToTimestamp(expr: ExprNode[Long]) extends ExprNode[Timestamp] {
    override def codec: Codec[Timestamp] = Codec[Timestamp]
  }

  final case class DurationToMicros(expr: ExprNode[Duration]) extends ExprNode[Long] {
    override def codec: Codec[Long] = Codec[Long]
  }

  final case class MicrosToDuration(expr: ExprNode[Long]) extends ExprNode[Duration] {
    override def codec: Codec[Duration] = Codec[Duration]
  }

  final case class DateToDays(expr: ExprNode[Date]) extends ExprNode[Int] {
    override def codec: Codec[Int] = Codec[Int]
  }

  final case class DaysToDate(expr: ExprNode[Int]) extends ExprNode[Date] {
    override def codec: Codec[Date] = Codec[Date]
  }

  final case class ToRepr[P, Repr](expr: ExprNode[P], injectionCodec: Codec.FromInjection[P, Repr])
      extends ExprNode[Repr] {
    override def codec: Codec[Repr] = injectionCodec.to
  }

  final case class FromRepr[P, Repr](expr: ExprNode[Repr], override val codec: Codec.FromInjection[P, Repr])
      extends ExprNode[P]

  private[tyda] trait Leafs[N[_]] {
    given [T]: TreeApi[ExprNode.Reference[T], N] = TreeApi.leaf
    given [T]: TreeApi[ExprNode.Literal[T], N] = TreeApi.leaf
    given TreeApi[String, N] = TreeApi.leaf
    given TreeApi[Int, N] = TreeApi.leaf
    given [T]: TreeApi[Comparable[T], N] = TreeApi.leaf
    given [T]: TreeApi[Integral[T], N] = TreeApi.leaf
    given [T]: TreeApi[AdditiveExpr[T], N] = TreeApi.leaf
    given [T]: TreeApi[Codec[T], N] = TreeApi.leaf
    given [T]: TreeApi[Codec.Product[T], N] = TreeApi.leaf
    given [T, R]: TreeApi[Codec.FromInjection[T, R], N] = TreeApi.leaf
    given [T, R]: TreeApi[T => R, N] = TreeApi.leaf
    given [T, R]: TreeApi[CanCast[T, R], N] = TreeApi.leaf
    given [T, R]: TreeApi[CanTryCast[T, R], N] = TreeApi.leaf
    given [A <: PrimitiveAggregate[?, ?]]: TreeApi[A, N] = TreeApi.leaf

    given [T]: TreeApi[Dataset.FromSeq[T], N] = TreeApi.leaf
    given [T]: TreeApi[Dataset.Read[T], N] = TreeApi.leaf
    given TreeApi[Boolean, N] = TreeApi.leaf
    given TreeApi[Format, N] = TreeApi.leaf
    given TreeApi[TableLocation, N] = TreeApi.leaf
  }

  private[tyda] object ExprNodeLeafs extends Leafs[ExprNode]
  private[tyda] object DatasetLeafs extends Leafs[Dataset]

  /* This is a val so that there is only one instance of TreeApi[ExprNode[T], ExprNode] created and reused for
   * all T */
  private val cachedApi: TreeApi[ExprNode[Any], ExprNode] = {
    import ExprNodeLeafs.given
    given [T <: Tuple]: TreeApi[Tuple.Map[Tuple, ExprNode], ExprNode] = TreeApi.mappedTuple([t] => () => api)
    TreeApi.coproduct
  }
  // TYPE SAFETY: The behavior of TreeApi[ExprNode[T], ExprNode] does not depend on T
  given api[T]: TreeApi[ExprNode[T], ExprNode] = cachedApi.asInstanceOf

  private val cachedDatasetApi: TreeApi[ExprNode[Any], Dataset] = {
    import DatasetLeafs.given
    given [T <: Tuple]: TreeApi[Tuple.Map[Tuple, ExprNode], Dataset] =
      TreeApi.mappedTuple([t] => () => ExprNode.datasetApi)
    TreeApi.coproductContainer
  }
  // TYPE SAFETY: The behavior of TreeApi[ExprNode[T], Dataset] does not depend on T
  given datasetApi[T]: TreeApi[ExprNode[T], Dataset] = cachedDatasetApi.asInstanceOf
}
