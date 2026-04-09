package com.choreograph.tyda

import scala.annotation.implicitNotFound
import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.Mirror

/** Type class used to retrict ExprApi.toJson and ExprApi.fromJson to only
  * support structs and arrays.
  *
  * This is mainly to make the intial support easier to implement and will
  * hopefully be lifted in the future.
  */
@implicitNotFound(
  "The methods Expr.toJson and Expr.fromJson currently only supports structs and arrays.\n" +
    "If ${T} is a type parameter add a context bound: [${T}: JsonArrayOrObject]\n" +
    "If ${T} is an opaque type the add a given JsonArrayOrObject[${T}] = JsonArrayOrObject.derived to the companion object\n"
)
trait JsonArrayOrObject[T] extends Serializable

object JsonArrayOrObject {
  def apply[T: JsonArrayOrObject as g]: JsonArrayOrObject[T] = g

  private case object Singleton extends JsonArrayOrObject[Any]

  inline given derived[T]: JsonArrayOrObject[T] = {
    check[T]

    // TYPE SAFETY: JsonArrayOrObject has no functionallity and is just used as evidence
    Singleton.asInstanceOf[JsonArrayOrObject[T]]
  }

  private inline def check[T]: Unit =
    inline erasedValue[T] match {
      case _: Option[?] => error("Option is not support as top level value in toJson/fromJson")
      case _: Seq[?] => ()
      case _: Map[?, ?] => ()
      case _ => summonFrom {
          case _: Mirror.ProductOf[T] => ()
          case _: Mirror.SumOf[T] => ()
          case _ => summonInline[JsonArrayOrObject[T]]: Unit
        }
    }
}
