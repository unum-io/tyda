package com.choreograph.tyda

import scala.annotation.implicitNotFound
import scala.compiletime.erasedValue
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.Mirror

/** Type class that serves as evidence that a type can be used in ordering
  * operations.
  *
  * All primitive types are orderable. Option, products, and sum types are
  * orderable if their element types are orderable. Seq and Map types are not
  * orderable, reflecting the fact that SQL engines (e.g. BigQuery) do not
  * support ORDER BY on array or map columns.
  */
@implicitNotFound(
  "Type ${T} is not orderable.\n" + "If ${T} is a type parameter add a context bound: [${T}: Orderable]\n" +
    "If ${T} is an opaque type add a given Orderable[${T}] = Orderable.derived to the companion object\n" +
    "Note: Seq and Map types cannot be used in orderBy."
)
trait Orderable[T] extends Serializable

object Orderable {
  def apply[T: Orderable as o]: Orderable[T] = o

  private case object Singleton extends Orderable[Any]

  private def unchecked[T]: Orderable[T] =
    // TYPE SAFETY: Orderable has no functionality and is just used as evidence
    Singleton.asInstanceOf[Orderable[T]]

  inline given derived[T]: Orderable[T] = {
    check[T]
    unchecked[T]
  }

  private inline def check[T]: Unit =
    inline erasedValue[T] match {
      case _: (EmptyTuple | Boolean | Byte | Short | Int | Long | Float | Double | String) => ()
      case _: Option[t] => check[t]
      case _: (h *: t) =>
        check[h]
        check[t]
      case _ => summonFrom {
          case m: Mirror.ProductOf[T] => check[m.MirroredElemTypes]
          case m: Mirror.SumOf[T] => check[m.MirroredElemTypes]
          case _ => summonInline[Orderable[T]]: Unit
        }
    }

  // In not possible to match opaque types with type parameters in inline match.
  // So we provide the given explicitly instead
  // Is is reported here https://github.com/scala/scala3/issues/20280
  given [P <: Int, S <: Int]: Orderable[Decimal[P, S]] = unchecked
}
