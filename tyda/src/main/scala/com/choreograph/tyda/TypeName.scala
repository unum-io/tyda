package com.choreograph.tyda

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes

/* Typeclass that produces the fully qualified typename with type parameters
 *
 * This can not be an opaue type due us running some tests on 2.13 to this scala bug
 * https://github.com/scala/bug/issues/13072 */
final case class TypeName[T](value: String)

object TypeName {
  def apply[T: TypeName]: TypeName[T] = summon
  def name[T: TypeName]: String = TypeName[T].value

  inline given [T]: TypeName[T] = create(typeName[T])

  private def typeNameImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect.*

    def containsTypeParam(t: TypeRepr): Boolean =
      t.typeSymbol.isTypeParam || t.typeArgs.exists(containsTypeParam)

    if containsTypeParam(TypeRepr.of[T]) then
      report.errorAndAbort(
        s"Cannot create TypeName for ${Type.show[
            T
          ]} since it contained type parameters, consider adding TypeName as a context bound where the parameter is defined."
      )

    Expr(Type.show[T])
  }

  private inline def typeName[A]: String = ${ typeNameImpl[A] }

  private def create[T](typeName: String): TypeName[T] = TypeName(typeName)
}
