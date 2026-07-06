package com.choreograph.tyda

import scala.deriving.Mirror

import shapeless3.deriving.Complete
import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

/** Ord is an SQL compatible Ordering that allows automatic derivation for sum
  * and product types.
  *
  * It is intended to be used in the derives clause for case classes and enums
  * that should have orderings.
  *
  * The implementation is intended to be compatible with ordering used in common
  * SQL engines like Spark. This means that the ordering for floating point is
  * the one commonly used in sql engines where -0.0 == 0.0 and NaN is larger
  * than all values.
  *
  * For example in Spark:
  * {{{
  * scala> spark.sql("SELECT -0.0 == 0.0, -0.0 < 0.0").show()
  * +-----------+-----------+
  * |(0.0 = 0.0)|(0.0 < 0.0)|
  * +-----------+-----------+
  * |       true|      false|
  * +-----------+-----------+
  * }}}
  * or duckdb
  * {{{
  * duckdb> SELECT -0.0 == 0.0, -0.0 < 0.0;
  * ┌─────────────┬─────────────┐
  * │ (0.0 = 0.0) ┆ (0.0 < 0.0) │
  * ╞═════════════╪═════════════╡
  * │        true ┆       false │
  * └─────────────┴─────────────┘
  * }}}
  * or postgres
  * {{{
  * postgres=# SELECT -0.0 = 0.0, -0.0 < 0.0;
  * ?column? | ?column?
  * ----------+----------
  *  t        | f
  * (1 row)
  * }}}
  *
  * Sadly GoogleSQL used in BigQuery does not support this and instead have IEEE
  * semantics.
  */
trait Ord[T] extends Ordering[T] {
  override def reverse: Ord[T] = Ord.Reverse(this)
}

trait LowPriorityOrd {
  private[tyda] final case class FromOrdering[T](ordering: Ordering[T]) extends Ord[T] {
    def compare(x: T, y: T): Int = ordering.compare(x, y)
  }
  given fromOrdering[T: Ordering]: Ord[T] = FromOrdering[T](Ordering[T])
}

object Ord extends LowPriorityOrd {
  def apply[T: Ord as ord]: Ord[T] = ord

  private[tyda] final case class ProductOrd[T](inst: K0.ProductInstances[Ord, T]) extends Ord[T] {
    def compare(x: T, y: T): Int =
      inst.foldLeft2(x, y)(0)([t] =>
        (acc: Int, ord: Ord[t], x0: t, y0: t) =>
          val cmp = ord.compare(x0, y0)
          Complete(cmp != 0)(cmp)(acc)
      )
  }

  private[tyda] final case class Reverse[T](ord: Ord[T]) extends Ord[T] {
    def compare(x: T, y: T): Int = ord.compare(y, x)
    override def reverse: Ord[T] = ord
  }

  private[tyda] final case class SumOrd[T: Labelling as labels](inst: K0.CoproductInstances[Ord, T])
      extends Ord[T] {
    private val ordinalToLabelIndex = {
      val sortedLabels = labels.elemLabels.sorted
      IArray.from(labels.elemLabels.map(sortedLabels.indexOf).toArray)
    }
    def compare(x: T, y: T): Int =
      inst.fold2(x, y)((xOrdinal, yOrdinal) => ordinalToLabelIndex(xOrdinal) - ordinalToLabelIndex(yOrdinal))(
        [t <: T] => (ord: Ord[t], x0: t, y0: t) => ord.compare(x0, y0)
      )
  }

  /* This just delegates to the standard library, but exists to allow us to match on it as it compatible with
   * sparks array ordering */
  private[tyda] final case class SeqOrdering[T, S[X] <: Seq[X]](impl: Ordering[S[T]]) extends Ord[S[T]] {
    def compare(x: S[T], y: S[T]): Int = impl.compare(x, y)
  }

  given float: Ord[Float] with {
    def compare(x: Float, y: Float): Int = if x == y then 0 else java.lang.Float.compare(x, y)
  }
  given double: Ord[Double] with {
    def compare(x: Double, y: Double): Int = if x == y then 0 else java.lang.Double.compare(x, y)
  }

  given seq[T: Ord as ord, S[X] <: Seq[X]]: Ord[S[T]] = SeqOrdering[T, S](Ordering.Implicits.seqOrdering)
  given product[T: K0.ProductInstancesOf[Ord] as inst]: Ord[T] = ProductOrd[T](inst)
  given sum[T: {K0.CoproductInstancesOf[Ord] as inst, Labelling}]: Ord[T] = SumOrd[T](inst)
  given valueClass[T: ValueClassMirror as m](using Ord[m.MirroredElemType]): Ord[T] = product

  inline def derived[T](using m: Mirror.Of[T]): Ord[T] =
    inline m match {
      case given Mirror.SumOf[T] => sum
      case given Mirror.ProductOf[T] => product
    }
}
