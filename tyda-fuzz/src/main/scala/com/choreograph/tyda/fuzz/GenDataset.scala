package com.choreograph.tyda.fuzz

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.tailrec
import scala.util.Random

import shapeless3.deriving.K0

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Arbitrary.Shrinkable
import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledAggregateExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.fuzz.GenExprNode.arbitraryCodec
import com.choreograph.tyda.fuzz.GenExprNode.isGroupable
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.unreachable

trait GenDataset[T] {
  def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]]

  /** Helper method for cases that only support a subset of codecs. */
  protected final def only[To](using
      Codec[To]
  )(f: PartialFunction[Codec[To], Shrinkable[Dataset[To]]]): Option[Shrinkable[Dataset[To]]] =
    f.lift(Codec[To])
}

object GenDataset {
  given codecToArbitrary[T: Codec]: Arbitrary[T] =
    Codec[T] match {
      case Codec.Byte => Arbitrary[Byte]
      case Codec.Short => Arbitrary[Short]
      case Codec.Int => Arbitrary[Int]
      case Codec.Long => Arbitrary[Long]
      case Codec.Float => Arbitrary[Float]
      case Codec.Double => Arbitrary[Double]
      case Codec.String => Arbitrary[String]
      case Codec.Boolean => Arbitrary[Boolean]
      case Codec.Bytes => Arbitrary[Binary]
      case Codec.TimestampMicros => Arbitrary[Timestamp]
      case Codec.DurationMicros => Arbitrary[Duration]
      case Codec.Date => Arbitrary[Date]
      case codec: Codec.Decimal[p, s] =>
        given Decimal.Valid[p, s] = codec.valid
        Arbitrary[Decimal[p, s]]
      case Codec.Option(given Codec[e]) => Arbitrary[Option[e]]
      case Codec.Map(given Codec[k], given Codec[v]) => Arbitrary[Seq[(k, v)]].map(_.toMap)

      case Codec.Seq(given Codec[e]) =>
        given Arbitrary[e] = codecToArbitrary
        Arbitrary[Seq[e]]
      case prod: Codec.Product[p] =>
        given K0.ProductInstances[Arbitrary, p] =
          prod.fields.mapK([t] => field => codecToArbitrary(using field.codec))
        Arbitrary[p]
      case Codec.FromInjection(inj, given Codec[to]) => codecToArbitrary[to].map(inj.invert(_))
    }

  given fromSeq: GenDataset[Dataset.FromSeq[Any]] with {
    def apply[To: Codec](depth: Int)(using r: Random): Option[Shrinkable[Dataset[To]]] =
      Some(codecToArbitrary[Seq[To]].map(Dataset.from(_)).shrinkable(r))
  }

  given readPathWithHivePartitions: GenDataset[Dataset.ReadPathWithHivePartitions[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = None
  }
  given readPath: GenDataset[Dataset.ReadPath[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = None
  }
  given readTable: GenDataset[Dataset.ReadTable[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = None
  }
  given readWithMetadata: GenDataset[Dataset.ReadWithMetadata[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = None
  }
  given readPartitionsPaths: GenDataset[Dataset.ReadPartitionsPaths[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = None
  }
  given readTablePartitionsPaths: GenDataset[Dataset.ReadTablePartitionsPaths[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = None
  }

  given select1: GenDataset[Dataset.Select1[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      GenExprNode.arbitraryCodec() match {
        case given Codec[t] =>
          val input = unfailable[t](depth)
          val expr = GenExprNode.genCompiledOrExplodeExpr[t, To]()
          Some(input.flatMap(in => expr.map(e => Dataset.Select1(in, e))))
      }
  }

  private def isTuple(prod: Codec.Product[?]): Boolean = {
    val names = prod.fields.mapConst([t] => identity(_)).map(_.name).toList
    names.nonEmpty && names == names.indices.map(i => s"_${i + 1}").toList
  }

  object TupleCodec {
    def unapply[T](prod: Codec.Product[T]): Option[Codec.Product[T & Tuple]] =
      // TYPE SAFETY: The check promises that T is also a Tuple
      Option.when(isTuple(prod))(prod.asInstanceOf[Codec.Product[T & Tuple]])
  }

  given selectN: GenDataset[Dataset.SelectN[Any, Tuple]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      only { case TupleCodec(prod) =>
        arbitraryCodec() match {
          case given Codec[in] =>
            val input = unfailable[in](depth)
            // One CompiledExpr per output field; reuses the expression fuzzer for each.
            val exprs = prod
              .fields
              .mapK[CompiledExprOrExplode.From[in]] { [t] => field =>
                given Codec[t] = field.codec
                GenExprNode.genCompiledOrExplodeExpr[in, t]().value
              }
              .toTuple
            // TYPE SAFETY: SelectN's output codec is Codec.tuple(fieldCodecs); `isTuple` ensures To is
            // itself a plain tuple with those exact field codecs, so the casts are sound.
            input.map(in => Dataset.SelectN(in, exprs).asInstanceOf[Dataset[To]])
        }
      }
  }

  given filter: GenDataset[Dataset.Filter[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = {
      val input = unfailable[To](depth)
      val predicate = GenExprNode.genCompiledExpr[To, Boolean]()
      val node = input.flatMap(in => predicate.map(p => Dataset.Filter(in, p)))
      // Also try shrinking by removing the filter entirely (filter(ds, p) -> ds).
      Some(Shrinkable(node.value, input #:: node.shrinks))
    }
  }

  given limit: GenDataset[Dataset.Limit[Any]] with {
    def apply[To: Codec](depth: Int)(using r: Random): Option[Shrinkable[Dataset[To]]] = {
      val input = unfailable[To](depth)
      val n = Arbitrary[Int].filter(_ >= 0)(r)
      val node = input.map(Dataset.Limit(_, n))
      // Also try shrinking by removing the limit entirely (limit(ds, n) -> ds).
      Some(Shrinkable(node.value, input #:: node.shrinks))
    }
  }

  given cache: GenDataset[Dataset.Cache[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = {
      val input = unfailable[To](depth)
      val node = input.map(Dataset.Cache(_))
      // Also try shrinking by removing the cache entirely (cache(ds) -> ds).
      Some(Shrinkable(node.value, input #:: node.shrinks))
    }
  }

  given distinct: GenDataset[Dataset.Distinct[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      only {
        case given Codec[t] if isGroupable(Codec[t]) =>
          val input = unfailable[t](depth)
          val node = input.map(Dataset.Distinct(_))
          // Also try shrinking by removing the distinct entirely (distinct(ds) -> ds).
          Shrinkable(node.value, input #:: node.shrinks)
      }
  }

  given union: GenDataset[Dataset.Union[Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] = {
      val left = unfailable[To](depth)
      val right = unfailable[To](depth)
      val node = left.flatMap(l => right.map(Dataset.Union(l, _)))
      // Also try shrinking by removing the node fully (union(a, b) -> a | b).
      Some(Shrinkable(node.value, left #:: right #:: node.shrinks))
    }
  }

  object Tuple2Codec {
    def unapply[T](prod: Codec.Product[T]): Option[(first: Codec[?], second: Codec[?])] = {
      val fields = prod.fields.mapConst([t] => identity(_))
      fields.map(_.name) match {
        case Seq("_1", "_2") => Some((fields(0).codec, fields(1).codec))
        case _ => None
      }
    }
  }

  given join: GenDataset[Dataset.Join[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      only { case Tuple2Codec(given Codec[t], given Codec[u]) =>
        // TYPE SAFETY: Tuple2Codec guarantees To = (t, u), matching Join's (T, U) output codec.
        for {
          l <- unfailable[t](depth)
          r <- unfailable[u](depth)
          cond <- GenExprNode.genCompiledExpr2[t, u, Boolean]()
        } yield Dataset.Join(l, r, cond).asInstanceOf[Dataset[To]]
      }
  }

  given leftOuterJoin: GenDataset[Dataset.LeftOuterJoin[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      only { case Tuple2Codec(given Codec[t], Codec.Option(given Codec[u])) =>
        // TYPE SAFETY: Tuple2Codec + Option guarantees To = (t, Option[u]), matching LeftOuterJoin.
        for {
          l <- unfailable[t](depth)
          r <- unfailable[u](depth)
          cond <- GenExprNode.genCompiledExpr2[t, u, Boolean]()
        } yield Dataset.LeftOuterJoin(l, r, cond).asInstanceOf[Dataset[To]]
      }
  }

  given fullOuterJoin: GenDataset[Dataset.FullOuterJoin[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      only { case Tuple2Codec(Codec.Option(given Codec[t]), Codec.Option(given Codec[u])) =>
        // TYPE SAFETY: Tuple2Codec + Option/Option guarantees To = (Option[t], Option[u]).
        for {
          l <- unfailable[t](depth)
          r <- unfailable[u](depth)
          cond <- GenExprNode.genCompiledExpr2[t, u, Boolean]()
        } yield Dataset.FullOuterJoin(l, r, cond).asInstanceOf[Dataset[To]]
      }
  }

  given leftAntiJoin: GenDataset[Dataset.LeftAntiJoin[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using Random): Option[Shrinkable[Dataset[To]]] =
      arbitraryCodec() match {
        case given Codec[r] =>
          val left = unfailable[To](depth)
          val node = for {
            l <- left
            r <- unfailable[r](depth)
            cond <- GenExprNode.genCompiledExpr2[To, r, Boolean]()
          } yield Dataset.LeftAntiJoin(l, r, cond)
          // Also try shrinking by removing the node fully (leftAntiJoin(a, b, p) -> a).
          Some(Shrinkable(node.value, left #:: node.shrinks))
      }
  }

  given mapPartitions: GenDataset[Dataset.MapPartitions[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using r: Random): Option[Shrinkable[Dataset[To]]] = None
  }

  given aggregate: GenDataset[Dataset.Aggregate[Any, Any]] with {
    def apply[To: Codec](depth: Int)(using r: Random): Option[Shrinkable[Dataset[To]]] =
      only { case Codec.Option(given Codec[t]) =>
        GenExprNode.arbitraryCodec() match {
          case given Codec[in] =>
            val input = unfailable[in](depth)
            val expr = GenExprNode.genCompiledAggregateExpr[in, t]()
            input.flatMap(in => expr.map(e => Dataset.Aggregate(in, e)))
        }
      }
  }

  given groupedAggregate: GenDataset[Dataset.GroupedAggregate[Any, AnyNamedTuple, AnyNamedTuple]] with {
    def apply[To: Codec](depth: Int)(using r: Random): Option[Shrinkable[Dataset[To]]] =
      only { case Tuple2Codec(given Codec[k], given Codec[v]) =>
        arbitraryCodec() match {
          case given Codec[in] =>
            for {
              input <- unfailable[in](depth)
              key <- GenExprNode.genCompiledExpr[in, k]().map(_.asInstanceOf[CompiledExpr[in, AnyNamedTuple]])
              agg <- GenExprNode.genCompiledAggregateExpr[in, v]().map(_.asInstanceOf[CompiledAggregateExpr[in, AnyNamedTuple]])
            } yield Dataset.GroupedAggregate(input, key, agg).asInstanceOf[Dataset[To]]
        }
      }
  }

  def empty[To: Codec]: Shrinkable[Dataset[To]] = Shrinkable.unshrinkable(Dataset.empty[To])

  def unfailable[To: Codec](depth: Int = 0)(using Random): Shrinkable[Dataset[To]] =
    if depth >= 10 then { empty }
    else {
      val generated = dataset(depth + 1).getOrElse(unreachable(
        "All Codecs should be reachable from at least one expr node"
      ))
      Shrinkable(generated.value, empty #:: generated.shrinks)
    }

  private def mkDataset[T](using instances: K0.CoproductInstances[GenDataset, Dataset[T]]) =
    new GenDataset[Dataset[T]] {
      def apply[To: Codec](depth: Int)(using r: Random): Option[Shrinkable[Dataset[To]]] = {
        @tailrec
        def loop(ids: List[Int]): Option[Shrinkable[Dataset[To]]] =
          ids match {
            case Nil => None
            case head :: tail =>
              val f = [t] => (gen: GenDataset[t]) => gen(depth)
              // TYPE SAFETY: erasedInject is a shapeless3 internal that calls the GenExpr at runtime
              // using type-erased function dispatch; the outer cast recovers the expected
              // Option[Shrinkable[E[To]]].
              val v = instances.erasedInject(head)(f.asInstanceOf[Any => Any])
              // TYPE SAFETY: erasedInject returns Any; the actual value is Option[Shrinkable[E[To]]] by
              // construction.
              v.asInstanceOf[Option[Shrinkable[Dataset[To]]]] match {
                case None => loop(tail)
                case some => some
              }
          }
        loop(r.shuffle((0 until instances.arity).toList))
      }
    }
  private val singleton: GenDataset[Dataset[Any]] = mkDataset
  // TYPE SAFETY: The Codec guard on the output ensures correct values
  given dataset[T]: GenDataset[Dataset[T]] = singleton.asInstanceOf
}
