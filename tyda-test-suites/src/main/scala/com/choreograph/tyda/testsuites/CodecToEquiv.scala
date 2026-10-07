package com.choreograph.tyda.testsuites

import shapeless3.deriving.Complete
import shapeless3.deriving.K0

import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Ord
import com.choreograph.tyda.Timestamp

object CodecToEquiv {
  def apply[T: Codec]: Equiv[T] =
    Codec[T] match {
      case Codec.Boolean => Ord[Boolean]
      case Codec.Byte => Ord[Byte]
      case Codec.Short => Ord[Short]
      case Codec.Int => Ord[Int]
      case Codec.Long => Ord[Long]
      case Codec.Float => Ord[Float]
      case Codec.Double => Ord[Double]
      case Codec.String => Ord[String]
      case Codec.Bytes => Ord[Binary]
      case Codec.TimestampMicros => Ord[Timestamp]

      case Codec.DurationMicros => Ord[Duration]
      case Codec.Date => Ord[Date]
      case _: Codec.Decimal[p, s] => Ord[Decimal[p, s]]
      case Codec.Option(given Codec[e]) =>
        given Equiv[e] = apply
        coproduct[Option[e]]
      case Codec.Map(_: Codec[k], given Codec[v]) =>
        val value = apply[v]
        new Equiv[Map[k, v]] {
          def equiv(x: Map[k, v], y: Map[k, v]): Boolean =
            x.size == y.size && x.iterator.forall((k, v) => y.get(k).forall(value.equiv(v, _)))
        }
      case Codec.Seq(given Codec[e]) =>
        val inner = apply
        new Equiv[Seq[e]] {
          def equiv(x: Seq[e], y: Seq[e]): Boolean = x.size == y.size && x.zip(y).forall(inner.equiv)
        }
      case prod: Codec.Product[p] =>
        given K0.ProductInstances[Equiv, p] = prod.fields.mapK([t] => field => apply(using field.codec))
        product
      case Codec.FromInjection(inj, given Codec[to]) =>
        val inner = apply[to]
        new Equiv[T] {
          def equiv(x: T, y: T): Boolean = inner.equiv(inj(x), inj(y))
        }
    }

  private given coproduct[A](using inst: K0.CoproductInstances[Equiv, A]): Equiv[A] with {
    def equiv(x: A, y: A): Boolean = inst.fold2(x, y)(false)([t] => (equiv, fx, fy) => equiv.equiv(fx, fy))
  }

  private given product[T: K0.ProductInstancesOf[Equiv] as instances]: Equiv[T] =
    (x, y) =>
      instances.foldLeft2(x, y)(true)([t] =>
        (_, equiv, fx, fy) => Complete(!equiv.equiv(fx, fy))(false)(true)
      )
}
