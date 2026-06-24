package com.choreograph.tyda.fuzz

import scala.annotation.tailrec
import scala.collection.Factory
import scala.collection.immutable.ArraySeq
import scala.deriving.Mirror
import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.util.Random

import shapeless3.deriving.K0

import com.choreograph.tyda.AdditiveExpr
import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Arbitrary.Shrinkable
import com.choreograph.tyda.Arbitrary.Shrinkable.toShrinkable
import com.choreograph.tyda.Binary
import com.choreograph.tyda.CanCast
import com.choreograph.tyda.CanTryCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Comparable
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Field
import com.choreograph.tyda.PrimitiveAggregate
import com.choreograph.tyda.SumMagnet
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.rewrite.CollectionCodec
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.unreachable

trait GenExprNode[T] {
  def apply[To: Codec](state: GenExprNode.State)(using Random): Option[Shrinkable[ExprNode[To]]]

  /** Helper method for cases that only support a subset of codecs. */
  protected final def only[To](using
      Codec[To]
  )(f: PartialFunction[Codec[To], Shrinkable[ExprNode[To]]]): Option[Shrinkable[ExprNode[To]]] =
    f.lift(Codec[To])

}

object GenExprNode {
  final case class State(refs: Seq[ExprNode.Reference[?]], aggregate: Boolean, depth: Int)

  trait Fixed[T] {
    def apply(state: State)(using Random): Shrinkable[T]
  }
  object Fixed {
    given fixedSeq[T: GenExprNode.Fixed as fixed]: Fixed[Seq[T]] with {
      def apply(state: State)(using r: Random): Shrinkable[Seq[T]] =
        Seq.fill(r.nextInt(5))(fixed(state)).toShrinkable
    }
    given fixed[T: Codec]: Fixed[ExprNode[T]] with {
      def apply(state: State)(using Random): Shrinkable[ExprNode[T]] = unfailable(state)(using Codec[T])
    }
  }

  given reference[T]: GenExprNode[ExprNode.Reference[T]] with {
    def apply[To: Codec as toCodec](state: State)(using Random): Option[Shrinkable[ExprNode.Reference[To]]] =
      state
        .refs
        .flatMap(ref =>
          ref.codec match {
            case `toCodec` => Some(ref: ExprNode.Reference[To])
            case _ => None
          }
        )
        .headOption
        .map(Shrinkable.unshrinkable)
  }

  given literal[T]: GenExprNode[ExprNode.Literal[T]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      only { case primitive: Codec.Primitive[To] =>
        val shrinkableValue: Shrinkable[To] = primitive match {
          case Codec.Int => Arbitrary[Int].shrinkable(r)
          case Codec.Long => Arbitrary[Long].shrinkable(r)
          case Codec.Byte => Arbitrary[Byte].shrinkable(r)
          case Codec.Short => Arbitrary[Short].shrinkable(r)
          case Codec.Float => Arbitrary[Float].shrinkable(r)
          case Codec.Double => Arbitrary[Double].shrinkable(r)
          case Codec.Boolean => Arbitrary[Boolean].shrinkable(r)
          case Codec.String => Arbitrary[String].shrinkable(r)
          case Codec.TimestampMicros => Arbitrary[Timestamp].shrinkable(r)
          case Codec.DurationMicros => Arbitrary[Duration].shrinkable(r)
          case Codec.Date => Arbitrary[Date].shrinkable(r)
          case Codec.Bytes => Arbitrary[Binary].shrinkable(r)
          case codec: Codec.Decimal[p, s] =>
            given Decimal.Valid[p, s] = codec.valid
            Arbitrary[Decimal[p, s]].shrinkable(r)
        }
        shrinkableValue.map(ExprNode.Literal(_, primitive))
      }
  }

  given select[P, T]: GenExprNode[ExprNode.Select[P, T]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode.Select[Any, To]]] = {
      val fieldName = Arbitrary[String].filter(!_.isEmpty)()
      // TYPE SAFETY: We construct a synthetic product with a single field of type To. The Select
      // node reads back that field at runtime using the same codec, so the overall type is correct.
      val codec = Codec.unsafeNamedTuple(Seq(Field(fieldName, Codec[To]))).asInstanceOf[Codec[Any]]
      Some(unfailable(state)(using codec).map(ExprNode.Select(_, fieldName)))
    }
  }

  given makeProduct: GenExprNode[ExprNode.MakeProduct[Any, Tuple]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case prod @ Codec.Product(_, fields, _) =>
        given Mirror.ProductOf[To] = prod.mirror
        val exprs = fields.mapK[ExprNode]([t] => f => unfailable(state)(using f.codec).value).toTuple
        Shrinkable.unshrinkable(ExprNode.MakeProduct(exprs, prod))
      }
  }

  given makeMap: GenExprNode[ExprNode.MakeMap[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only {
        case Codec.Map(given Codec[k], given Codec[v]) if isGroupable(Codec[k]) =>
          unfailable[Seq[(k, v)]](state).map(ExprNode.MakeMap(_))
      }
  }

  given makeSeq: GenExprNode[ExprNode.MakeSeq[Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Seq(given Codec[?]) =>
        Seq.fill(r.nextInt(5))(unfailable(state)).toShrinkable.map(ExprNode.MakeSeq(_))
      }
  }

  object KeyAndValuePairCodec {
    def unapply[T](prod: Codec.Product[T]): Option[(key: Codec[?], value: Codec[?])] = {
      val fields = prod.fields.mapConst([t] => identity(_))
      fields.map(_.name) match {
        case Seq("key", "value") => Some((fields(0).codec, fields(1).codec))
        case _ => None
      }
    }
  }

  given mapEntries: GenExprNode[ExprNode.MapEntries[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Seq(KeyAndValuePairCodec(given Codec[k], given Codec[v])) =>
        // TYPE SAFETY: KeyAndValuePairCodec ensures that To is the correct type. TODO: move this information
        // to the type system.
        unfailable[Map[k, v]](state).map(e => ExprNode.MapEntries(e).asInstanceOf[ExprNode[To]])
      }
  }

  given mapGet: GenExprNode[ExprNode.MapGet[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Option(given Codec[v]) =>
        arbitraryCodec() match {
          case given Codec[k] =>
            val map = unfailable[Map[k, v]](state)
            val key = unfailable[k](state)
            map.flatMap(m => key.map(ExprNode.MapGet(m, _)))
        }
      }
  }

  given concatSeq: GenExprNode[ExprNode.ConcatSeq[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Seq(_: Codec[e]) =>
        val lhs = unfailable[Seq[e]](state)
        val rhs = unfailable[Seq[e]](state)
        val node = lhs.flatMap(r => rhs.map(ExprNode.ConcatSeq(r, _)))
        // Also try shrinking by removing the node fully.
        Shrinkable(node.value, lhs #:: rhs #:: node.shrinks)
      }
  }

  given distinctSeq: GenExprNode[ExprNode.DistinctSeq[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only {
        case Codec.Seq(element: Codec[e]) if isGroupable(element) =>
          val operand = unfailable[Seq[e]](state)
          val node = operand.map(ExprNode.DistinctSeq(_))
          // Also try shrinking by removing the node fully (distinct(xs) -> xs).
          Shrinkable(node.value, operand #:: node.shrinks)
      }
  }

  given sizeSeq: GenExprNode[ExprNode.SizeSeq[Seq[Any]]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Int => unfailable(state)(using arbitrarySeqCodec()).map(ExprNode.SizeSeq(_)) }
  }

  given elementSeq: GenExprNode[ExprNode.ElementSeq[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = {
      val array = unfailable[Seq[To]](state)
      val index = unfailable[Int](state)
      Some(array.flatMap(a => index.map(ExprNode.ElementSeq(a, _))))
    }
  }

  def genCompiledExpr2[T: Codec, U: Codec, R: Codec](
      refs: Seq[ExprNode.Reference[?]] = Seq.empty,
      depth: Int = 0
  )(using Random): Shrinkable[CompiledExpr2[T, U, R]] = {
    val left = ExprNode.Reference[T]()
    val right = ExprNode.Reference[U]()
    unfailable[R](State(left +: right +: refs, false, depth)).map(CompiledExpr2(left, right, _))
  }

  def genCompiledExpr[T: Codec, U: Codec](refs: Seq[ExprNode.Reference[?]] = Seq.empty, depth: Int = 0)(using
      Random
  ): Shrinkable[CompiledExpr[T, U]] = {
    val param = ExprNode.Reference[T]()
    unfailable[U](State(param +: refs, false, depth)).map(CompiledExpr(param, _))
  }

  def genCompiledExplodeExpr[T: Codec, U: Codec](
      refs: Seq[ExprNode.Reference[?]] = Seq.empty,
      depth: Int = 0
  )(using Random): Shrinkable[CompiledExplodeExpr[T, U]] = {
    given Codec[Iterable[U]] = Codec.iterable
    val param = ExprNode.Reference[T]()
    unfailable[Iterable[U]](State(param +: refs, false, depth)).map(CompiledExplodeExpr(param, _))
  }

  def genCompiledOrExplodeExpr[T: Codec, U: Codec](
      refs: Seq[ExprNode.Reference[?]] = Seq.empty,
      depth: Int = 0
  )(using r: Random): Shrinkable[CompiledExprOrExplode[T, U]] =
    if r.nextBoolean then genCompiledExpr[T, U](refs, depth) else genCompiledExplodeExpr[T, U](refs, depth)

  def genCompiledAggregateExpr[T: Codec, U: Codec](
      refs: Seq[ExprNode.Reference[?]] = Seq.empty,
      depth: Int = 0
  )(using r: Random): Shrinkable[CompiledAggregateExpr[T, U]] = {
    val param = ExprNode.Reference[T]()
    unfailable[U](State(param +: refs, true, depth)).map(CompiledAggregateExpr(param, _))
  }

  given mapSeq: GenExprNode[ExprNode.MapSeq[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Seq(given Codec[u]) =>
        arbitraryCodec() match {
          case given Codec[t] =>
            val operand = unfailable[Seq[t]](state)
            val fn = genCompiledExpr[t, u](state.refs, state.depth)
            operand.flatMap(o => fn.map(ExprNode.MapSeq(o, _)))
        }
      }
  }

  given filterSeq: GenExprNode[ExprNode.FilterSeq[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Seq(given Codec[e]) =>
        val operand = unfailable[Seq[e]](state)
        val predicate = genCompiledExpr[e, Boolean](state.refs, state.depth)
        val node = operand.flatMap(o => predicate.map(ExprNode.FilterSeq(o, _)))
        // Also try shrinking by removing the node fully (filter(xs, p) -> xs).
        Shrinkable(node.value, operand #:: node.shrinks)
      }
  }

  given aggregateSeq: GenExprNode[ExprNode.AggregateSeq[Any, Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Boolean =>
        val operand = unfailable[Seq[Boolean]](state)
        val (onEmpty, aggregate): (
            ExprNode[Boolean],
            PrimitiveAggregate[Boolean, Boolean] & ExprNode.AggregateSeq.SupportedAggregates
        ) =
          if r.nextBoolean then (ExprNode.lit(false), PrimitiveAggregate.BoolOr())
          else (ExprNode.lit(true), PrimitiveAggregate.BoolAnd())
        val node = operand.map(ExprNode.AggregateSeq(_, onEmpty, aggregate))
        // Also try shrinking by removing the node fully (aggregate -> onEmpty).
        Shrinkable(node.value, Shrinkable.unshrinkable(onEmpty) #:: node.shrinks)
      }
  }

  given equals: GenExprNode[ExprNode.Equals[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Boolean =>
        arbitraryGroupableCodec() match {
          case given Codec[t] =>
            unfailable[t](state).flatMap(a => unfailable[t](state).map(ExprNode.Equals(a, _)))
        }
      }
  }

  given lessThen: GenExprNode[ExprNode.LessThan[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Boolean =>
        comparableCodec() match {
          case ComparableAndCodec(ev, given Codec[t]) =>
            unfailable[t](state).flatMap(a => unfailable[t](state).map(ExprNode.LessThan(ev, a, _)))
        }
      }
  }

  given lessThenOrEqual: GenExprNode[ExprNode.LessThanOrEqual[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Boolean =>
        comparableCodec() match {
          case ComparableAndCodec(ev, given Codec[t]) =>
            unfailable[t](state).flatMap(a => unfailable[t](state).map(ExprNode.LessThanOrEqual(ev, a, _)))
        }
      }
  }

  private val iterableTag = classTag[Iterable[?]]

  given upcastToIterable: GenExprNode[ExprNode.UpcastToIterable[Any, Iterable[Any]]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Iterable(`iterableTag`, given Codec[t]) =>
        unfailable[Seq[t]](state).map(ExprNode.UpcastToIterable(_))
      }
  }

  given optionToIterable: GenExprNode[ExprNode.OptionToIterable[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Iterable(`iterableTag`, given Codec[t]) =>
        unfailable[Option[t]](state).map(ExprNode.OptionToIterable(_))
      }
  }

  given makeSome: GenExprNode[ExprNode.MakeSome[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Option(given Codec[t]) => unfailable[t](state).map(ExprNode.MakeSome(_)) }
  }

  given none: GenExprNode[ExprNode.None[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Option(given Codec[t]) => Shrinkable.unshrinkable(ExprNode.None(summon[Codec[t]])) }
  }

  given coalesce: GenExprNode[ExprNode.Coalesce[Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Option(_: Codec[t]) =>
        val operands = Seq.fill(r.nextInt(4) + 1)(unfailable[Option[t]](state))
        val node = operands
          .toShrinkable
          .filter(_.size > 0) // Requires at least one element
          .map(ExprNode.Coalesce(_))
        // Also try shrinking by removing the node fully (coalesce(a, b, ...) -> any operand).
        Shrinkable(node.value, operands.to(LazyList) #::: node.shrinks)
      }
  }

  given raiseError: GenExprNode[ExprNode.RaiseError[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      Some(unfailable(state)(using Codec.String).map(ExprNode.RaiseError(_, Codec[To])))
  }

  given cases: GenExprNode[ExprNode.Cases[Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] = {
      val branches = Seq.fill(r.nextInt(3) + 1)((unfailable[Boolean](state), unfailable[To](state)))
      val elseExpr = unfailable[To](state)

      val whenThens = branches.map((w, t) =>
        for {
          wv <- w
          tv <- t
        } yield ExprNode.WhenThen(wv, tv)
      )

      val node =
        for
          head <- whenThens.head
          tail <- whenThens.tail.toShrinkable
          elseE <- elseExpr
        yield ExprNode.Cases(head, tail, elseE)

      // Every branch's `then` expression and the else expression is an ExprNode[To], so
      // collapsing the Cases to any of them is a valid, strictly-smaller candidate.
      val collapseCandidates = (branches.map(_._2) :+ elseExpr).to(LazyList)
      Some(Shrinkable(node.value, collapseCandidates #::: node.shrinks))
    }
  }

  given knownNotNull: GenExprNode[ExprNode.KnownNotNull[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      // Would be nice we we could avoid forcing MakeSome here, but it would probably require that we pass
      // more contraints then just the Codec to the generator.
      Some(unfailable(state).map(child => ExprNode.KnownNotNull(ExprNode.MakeSome(child))))
  }

  given isNan: GenExprNode[ExprNode.IsNaN[Float | Double]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Boolean =>
        if r.nextBoolean() then unfailable[Float](state).map(ExprNode.IsNaN(_))
        else unfailable[Double](state).map(ExprNode.IsNaN(_))
      }
  }

  given toJson: GenExprNode[ExprNode.ToJson[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.String =>
        arbitraryProductSeqOrMap() match {
          case given Codec[t] => unfailable[t](state).map(ExprNode.ToJson(_))
        }
      }
  }

  given fromJson: GenExprNode[ExprNode.FromJson[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case Codec.Option(codec @ (CollectionCodec(_) | Codec.Product(_, _, _))) =>
        unfailable[String](state).map(ExprNode.FromJson(_, codec))
      }
  }

  given add: GenExprNode[ExprNode.Add[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only {
        case Codec.Int =>
          unfailable(state).flatMap(a => unfailable(state).map(ExprNode.Add(AdditiveExpr[Int], a, _)))
        case Codec.Long =>
          unfailable(state).flatMap(a => unfailable(state).map(ExprNode.Add(AdditiveExpr[Long], a, _)))
        case Codec.Double =>
          unfailable(state).flatMap(a => unfailable(state).map(ExprNode.Add(AdditiveExpr[Double], a, _)))
        case Codec.Float =>
          unfailable(state).flatMap(a => unfailable(state).map(ExprNode.Add(AdditiveExpr[Float], a, _)))
      }
  }

  given quotient: GenExprNode[ExprNode.Quotient[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only {
        case Codec.Int =>
          unfailable(state).flatMap(a => unfailable(state).map(ExprNode.Quotient(Integral[Int], a, _)))
        case Codec.Long =>
          unfailable(state).flatMap(a => unfailable(state).map(ExprNode.Quotient(Integral[Long], a, _)))
      }
  }

  given rand: GenExprNode[ExprNode.Rand] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = None
  }

  final case class CanCastAndCodec[From, To](canCast: CanCast[From, To], codec: Codec[From])

  private def castEntry[From: Codec, To](using c: CanCast[From, To]): CanCastAndCodec[From, To] =
    CanCastAndCodec(c, Codec[From])

  /* Could we do some metaprogramming here to extra the singletons that do cast to To and campture their input
   * type? TODO */
  def castsTo[To: Codec](): Seq[CanCastAndCodec[?, To]] =
    Codec[To] match {
      case Codec.Int => Seq(castEntry[Byte, Int])
      case _ => Seq.empty
    }

  given cast: GenExprNode[ExprNode.Cast[Any, Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      r.shuffle(castsTo[To]())
        .headOption
        .map { case CanCastAndCodec(canCast, given Codec[from]) =>
          unfailable[from](state).map(ExprNode.Cast(_, canCast))
        }
  }

  final case class CanTryCastAndCodec[From, To](canTryCast: CanTryCast[From, To], codec: Codec[From])

  private def tryEntry[From: Codec, To](using c: CanTryCast[From, To]): CanTryCastAndCodec[From, To] =
    CanTryCastAndCodec(c, summon[Codec[From]])

  // TODO
  def tryCastsTo[To: Codec as toCodec]: Seq[CanTryCastAndCodec[?, To]] =
    toCodec match {
      case Codec.Int => Seq(tryEntry[String, Int], tryEntry[Long, Int])
      case _ => Seq.empty
    }

  given tryCast: GenExprNode[ExprNode.TryCast[Any, Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      Codec[To] match {
        case Codec.Option(given Codec[u]) => r
            .shuffle(tryCastsTo[u])
            .headOption
            .map { case CanTryCastAndCodec(canTryCast, given Codec[from]) =>
              unfailable[from](state).map(ExprNode.TryCast(_, canTryCast))
            }
        case _ => None
      }
  }

  given fromRepr: GenExprNode[ExprNode.FromRepr[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case injectionCodec @ Codec.FromInjection(_, given Codec[repr]) =>
        unfailable[repr](state).map(ExprNode.FromRepr(_, injectionCodec))
      }
  }

  // TODO: More cases here
  def injectionsTo[Repr: Codec](): Seq[Codec.FromInjection[?, Repr]] =
    Codec[Repr] match {
      case Codec.Seq(codec: Codec[e]) =>
        given ClassTag[e] = codec.classTag
        def col[C <: Iterable[e]](using ClassTag[C], Factory[e, C]) = Codec.Iterable[e, C](summon, codec)
        Seq(col[List[e]], col[Vector[e]], col[ArraySeq[e]])
      case _ => Seq.empty
    }

  given toRepr: GenExprNode[ExprNode.ToRepr[Any, Any]] with {
    def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
      r.shuffle(injectionsTo[To]())
        .headOption
        .map { case injectionCodec @ (given Codec.FromInjection[p, To]) =>
          unfailable[p](state).map(ExprNode.ToRepr(_, injectionCodec))
        }
  }

  given scalarSubquery: GenExprNode[ExprNode.ScalarSubquery[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = None // TODO
  }

  given existsSubquery: GenExprNode[ExprNode.ExistsSubquery[Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = None // TODO
  }

  private trait GenAggregate[A] {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]]

    protected final def only[To](using
        Codec[To]
    )(f: PartialFunction[Codec[To], Shrinkable[ExprNode[To]]]): Option[Shrinkable[ExprNode[To]]] =
      f.lift(Codec[To])

  }

  private object GenAggregate {
    given collect: GenAggregate[PrimitiveAggregate.Collect[Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        only { case Codec.Seq(given Codec[e]) =>
          unfailable[e](state).map(arg => ExprNode.Aggregate(arg, PrimitiveAggregate.Collect()))
        }
    }

    given count: GenAggregate[PrimitiveAggregate.Count[Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        only { case Codec.Long =>
          arbitraryCodec() match {
            case given Codec[t] =>
              unfailable[t](state).map(arg => ExprNode.Aggregate(arg, PrimitiveAggregate.Count()))
          }
        }
    }

    given countSome: GenAggregate[PrimitiveAggregate.CountSome[Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        only { case Codec.Long =>
          arbitraryCodec() match {
            case given Codec[t] =>
              unfailable[Option[t]](state).map(arg => ExprNode.Aggregate(arg, PrimitiveAggregate.CountSome()))
          }
        }
    }

    given boolAnd: GenAggregate[PrimitiveAggregate.BoolAnd] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        only { case Codec.Boolean =>
          unfailable[Boolean](state).map(ExprNode.Aggregate(_, PrimitiveAggregate.BoolAnd()))
        }
    }

    given boolOr: GenAggregate[PrimitiveAggregate.BoolOr] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        only { case Codec.Boolean =>
          unfailable[Boolean](state).map(ExprNode.Aggregate(_, PrimitiveAggregate.BoolOr()))
        }
    }

    given min: GenAggregate[PrimitiveAggregate.Min[Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        comparableFor(Codec[To]).map { comparable =>
          unfailable[To](state).map(ExprNode.Aggregate(_, PrimitiveAggregate.Min(comparable)))
        }
    }

    given max: GenAggregate[PrimitiveAggregate.Max[Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        comparableFor(Codec[To]).map { comparable =>
          unfailable[To](state).map(ExprNode.Aggregate(_, PrimitiveAggregate.Max(comparable)))
        }
    }

    given minBy: GenAggregate[PrimitiveAggregate.MinBy[Any, Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        comparableCodec() match {
          case ComparableAndCodec(comp, given Codec[o]) =>
            given Codec[(To, o)] =
              Codec.unsafeNamedTuple(Seq(Field("_1", Codec[To]), Field("_2", Codec[o]))).asInstanceOf
            Some(unfailable[(To, o)](state).map(arg =>
              ExprNode.Aggregate(arg, PrimitiveAggregate.MinBy[To, o](comp))
            ))
        }
    }

    given maxBy: GenAggregate[PrimitiveAggregate.MaxBy[Any, Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
        comparableCodec() match {
          case ComparableAndCodec(comp, given Codec[o]) =>
            given Codec[(To, o)] =
              Codec.unsafeNamedTuple(Seq(Field("_1", Codec[To]), Field("_2", Codec[o]))).asInstanceOf
            Some(unfailable[(To, o)](state).map(arg =>
              ExprNode.Aggregate(arg, PrimitiveAggregate.MaxBy[To, o](comp))
            ))
        }
    }

    given reduce: GenAggregate[PrimitiveAggregate.Reduce[Any]] with {
      def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = None
    }

    private final case class SumEntry[T, R](magnet: SumMagnet.Aux[T, R], inputCodec: Codec[T])
    given sum: GenAggregate[PrimitiveAggregate.Sum[Any, Any]] with {

      private def sumsTo[R: Codec]: Seq[SumEntry[?, R]] =
        Codec[R] match {
          case Codec.Long => Seq(
              SumEntry(SumMagnet.byte, Codec.Byte),
              SumEntry(SumMagnet.short, Codec.Short),
              SumEntry(SumMagnet.int, Codec.Int),
              SumEntry(SumMagnet.long, Codec.Long)
            )
          case Codec.Double =>
            Seq(SumEntry(SumMagnet.float, Codec.Float), SumEntry(SumMagnet.double, Codec.Double))
          case Codec.Option(inner) => sumsTo(using inner).map(i =>
              SumEntry(SumMagnet.option(using i.magnet), Codec.Option(i.inputCodec))
            )
          case _ => Seq.empty
        }

      def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] =
        r.shuffle(sumsTo[To])
          .headOption
          .map { case SumEntry(magnet, given Codec[t]) =>
            unfailable[t](state).map(arg => ExprNode.Aggregate(arg, PrimitiveAggregate.Sum(magnet)))
          }
    }

    private def mkAggregate(using
        instances: K0.CoproductInstances[GenAggregate, PrimitiveAggregate[Any, Any]]
    ) =
      new GenAggregate[PrimitiveAggregate[Any, Any]] {
        def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] = {
          @tailrec
          def loop(ids: List[Int]): Option[Shrinkable[ExprNode[To]]] =
            ids match {
              case Nil => None
              case head :: tail =>
                val f = [t] => (gen: GenAggregate[t]) => gen(state)
                val v = instances.erasedInject(head)(f.asInstanceOf[Any => Any])
                v.asInstanceOf[Option[Shrinkable[ExprNode[To]]]] match {
                  case None => loop(tail)
                  case some => some
                }
            }
          loop(r.shuffle((0 until instances.arity).toList))
        }
      }

    private val singleton: GenAggregate[PrimitiveAggregate[Any, Any]] = mkAggregate
    def gen[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = singleton(state)
  }

  given aggregate: GenExprNode[ExprNode.Aggregate[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = {
      if !state.aggregate then return None
      GenAggregate.gen[To](state.copy(aggregate = false))
    }
  }

  given udf: GenExprNode[ExprNode.Udf[Any, Any]] with {
    def apply[To: Codec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] = None
  }

  given product[T: Codec as fixedCodec, E <: ExprNode[T]](using
      instances: K0.ProductInstances[GenExprNode.Fixed, E]
  ): GenExprNode[E] with {
    def apply[To: Codec as toCodec](state: State)(using Random): Option[Shrinkable[ExprNode[To]]] =
      only { case `fixedCodec` =>
        instances.constructA[Shrinkable]([t] => gen => gen(state))(
          Shrinkable.pure,
          Shrinkable.map,
          Shrinkable.ap
        )
      }
  }

  final case class ComparableAndCodec[T](comparable: Comparable[T], codec: Codec[T])
  val comparableAndCodecs: Seq[ComparableAndCodec[?]] = IndexedSeq(
    ComparableAndCodec(Comparable.Boolean, Codec.Boolean),
    ComparableAndCodec(Comparable.Byte, Codec.Byte),
    ComparableAndCodec(Comparable.Short, Codec.Short),
    ComparableAndCodec(Comparable.Int, Codec.Int),
    ComparableAndCodec(Comparable.Long, Codec.Long),
    ComparableAndCodec(Comparable.Float, Codec.Float),
    ComparableAndCodec(Comparable.Double, Codec.Double),
    ComparableAndCodec(Comparable.String, Codec.String),
    ComparableAndCodec(Comparable.Date, Codec.Date),
    ComparableAndCodec(Comparable.Timestamp, Codec.TimestampMicros),
    ComparableAndCodec(Comparable.Duration, Codec.DurationMicros)
  )
  def comparableCodec()(using r: Random): ComparableAndCodec[?] =
    comparableAndCodecs(r.nextInt(comparableAndCodecs.length))

  def comparableFor[T](codec: Codec[T]): Option[Comparable[T]] =
    comparableAndCodecs.collectFirst { case ComparableAndCodec(comp, `codec`) => comp }

  private val codecOptions: IndexedSeq[Random ?=> Codec[?]] = IndexedSeq(
    Codec.Boolean,
    Codec.Byte,
    Codec.Short,
    Codec.Int,
    Codec.Long,
    Codec.Float,
    Codec.Double,
    Codec.String,
    Codec.Date,
    Codec.TimestampMicros,
    Codec.DurationMicros,
    arbitraryOptionCodec(),
    arbitraryMapCodec(),
    arbitrarySeqCodec(),
    arbitraryProductCodec()
  )

  def arbitraryCodec()(using r: Random): Codec[?] = codecOptions(r.nextInt(codecOptions.length))

  def isGroupable(c: Codec[?]): Boolean =
    Codec
      .iterate(c)
      .forall {
        case Codec.Map(_, _) => false
        case _ => true
      }

  def arbitraryGroupableCodec()(using r: Random): Codec[?] =
    // TODO: Make more efficient
    Iterator.continually(arbitraryCodec()).filter(isGroupable).next()

  private val productSeqOrMapOptions: IndexedSeq[Random ?=> Codec[?]] =
    IndexedSeq(arbitraryMapCodec(), arbitrarySeqCodec(), arbitraryProductCodec())

  def arbitraryProductSeqOrMap()(using r: Random): Codec[?] =
    productSeqOrMapOptions(r.nextInt(productSeqOrMapOptions.length))

  def arbitraryMapCodec()(using Random): Codec[?] = Codec.Map(arbitraryGroupableCodec(), arbitraryCodec())

  def arbitrarySeqCodec()(using Random): Codec[? <: Seq[?]] = Codec.Seq(arbitraryCodec())

  def arbitraryOptionCodec()(using Random): Codec[?] = Codec.Option(arbitraryCodec())

  def arbitraryProductCodec()(using r: Random): Codec[?] = {
    def genField = Field(r.nextString(5), arbitraryCodec())
    val fields = Seq.fill(r.between(0, 3))(genField)
    Codec.unsafeNamedTuple(fields)
  }

  private def mkExprNode[T](using instances: K0.CoproductInstances[GenExprNode, ExprNode[T]]) =
    new GenExprNode[ExprNode[T]] {
      def apply[To: Codec](state: State)(using r: Random): Option[Shrinkable[ExprNode[To]]] = {
        @tailrec
        def loop(ids: List[Int]): Option[Shrinkable[ExprNode[To]]] =
          ids match {
            case Nil => None
            case head :: tail =>
              val f = [t] => (gen: GenExprNode[t]) => gen(state)
              // TYPE SAFETY: erasedInject is a shapeless3 internal that calls the GenExpr at runtime
              // using type-erased function dispatch; the outer cast recovers the expected
              // Option[Shrinkable[E[To]]].
              val v = instances.erasedInject(head)(f.asInstanceOf[Any => Any])
              // TYPE SAFETY: erasedInject returns Any; the actual value is Option[Shrinkable[E[To]]] by
              // construction.
              v.asInstanceOf[Option[Shrinkable[ExprNode[To]]]] match {
                case None => loop(tail)
                case some => some
              }
          }
        loop(r.shuffle((0 until instances.arity).toList))
      }
    }
  private val singleton: GenExprNode[ExprNode[Any]] = mkExprNode
  // TYPE SAFETY: The Codec guard on the output ensures correct values
  given exprNode[T]: GenExprNode[ExprNode[T]] = singleton.asInstanceOf

  def unfailable[To: Codec](state: State)(using Random): Shrinkable[ExprNode[To]] =
    if state.depth >= 10 then {
      literal(state)
        .orElse(reference(state))
        .getOrElse(Shrinkable.unshrinkable(
          ExprNode.RaiseError(ExprNode.Literal("depth limit", Codec.String), Codec[To])
        ))
    } else {
      val generated = exprNode(state.copy(depth = state.depth + 1)).getOrElse(unreachable(
        "All Codecs should be reachable from at least one expr node"
      ))
      val simpler: LazyList[Shrinkable[ExprNode[To]]] = (literal(state).toList ++ reference(state).toList).to(
        LazyList
      )
      Shrinkable(generated.value, simpler #::: generated.shrinks)
    }
}
