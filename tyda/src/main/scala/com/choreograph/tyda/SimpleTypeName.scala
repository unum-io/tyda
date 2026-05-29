package com.choreograph.tyda

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes

/** Typeclass that produces the simple (unqualified) typename with type
  * parameters.
  */
opaque type SimpleTypeName[T] = String

object SimpleTypeName {
  def apply[T: SimpleTypeName]: SimpleTypeName[T] = summon
  def name[T: SimpleTypeName]: String = SimpleTypeName[T]

  inline given [T]: SimpleTypeName[T] = simpleTypeName[T]

  private inline def simpleTypeName[A]: String = ${ simpleTypeNameImpl[A] }

  private def replaceDollar(s: String): String = if (s.endsWith("$")) s.stripSuffix("$") + ".type" else s

  private def simpleTypeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*

    def containsTypeParam(t: TypeRepr): Boolean =
      t.typeSymbol.isTypeParam || t.typeArgs.exists(containsTypeParam)

    def show = Type.show[T]

    def simpleName(t: TypeRepr): String = {
      val leading = replaceDollar(t.typeSymbol.name)
      val args = t.typeArgs.map(simpleName)
      leading + (if args.nonEmpty then args.mkString("[", ",", "]") else "")
    }

    val typeRepr = TypeRepr.of[T].dealias

    if containsTypeParam(typeRepr) then
      report.errorAndAbort(
        s"Cannot create SimpleTypeName for $show since it contained type parameters, consider adding SimpleTypeName as a context bound where the parameter is defined."
      )

    Expr(simpleName(typeRepr))
  }
}
