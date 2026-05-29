package com.choreograph.tyda

import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.ops.any.ToString
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes

package object compiletimeextras {
  transparent inline def assertCompileTimeError(
      inline code: String,
      message: String,
      messages: String*
  ): Unit = {
    val allMessages = message +: messages
    val errors = scala.compiletime.testing.typeCheckErrors(code)
    assert(
      allMessages.forall(msg => errors.exists(_.message.contains(msg))), {
        val allMessagesStr = allMessages.mkString("', '")
        val errorsStr = errors.map(_.message).mkString("\n")
        s"'$code' should not compile with message(s) '$allMessagesStr'. Found\n$errorsStr"
      }
    )
  }

  /** Returns the full type name of `T` as a string literal.
    *
    * Note: the typeName methods are marked transparent to work around scala3
    * limitations. When using a version with
    * https://github.com/scala/scala3/pull/24431 we can remove transparent.
    */
  transparent inline def typeName[T]: String = ${ typeNameImpl[T] }

  private def typeNameImpl[T: Type](using Quotes): Expr[String] = Expr(Type.show[T])

  /** Returns full type name of all the types in `T` that been joined using `, `
    * as seperator.
    */
  transparent inline def typeNameAll[T <: Tuple]: String =
    inline erasedValue[T] match {
      case _: EmptyTuple => ""
      case _: (head *: EmptyTuple) => typeName[head]
      case _: (head *: tail) => typeName[head] + ", " + typeNameAll[tail]
    }

  /** Returns the short type name of `T` as a string literal.
    */
  transparent inline def typeNameShort[T]: String = ${ typeNameShortImpl[T] }

  private def typeNameShortImpl[T: Type](using Quotes): Expr[String] = {
    import quotes.reflect.{TypeRepr, Printer}
    Expr(TypeRepr.of[T].show(using Printer.TypeReprShortCode))
  }

  /** Returns short type name of all the types in `T` that been joined using
    * `, ` as seperator.
    */
  transparent inline def typeNameShortAll[T <: Tuple]: String =
    inline erasedValue[T] match {
      case _: EmptyTuple => ""
      case _: (head *: EmptyTuple) => typeNameShort[head]
      case _: (head *: tail) => typeNameShort[head] + ", " + typeNameShortAll[tail]
    }

  /** Turn a constant value in to a string at compile time.
    *
    * Note: This should not be just toString as that will not be picked over the
    * member toString inside any class or object.
    */
  inline def constToString(
      t: Boolean | Byte | Short | Int | Long | Float | Double | Char | String
  ): ToString[t.type] = constValue[ToString[t.type]]
}
