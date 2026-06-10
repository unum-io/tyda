package com.choreograph.tyda.metadata

import scala.meta.internal.Scaladoc
import scala.meta.internal.parsers.ScaladocParser
import scala.quoted.*

/** Contains parsed Scaladoc descriptions for a specific type and its
  * parameters.
  */
final case class TypeDocs[T](description: Option[String], params: Map[String, String])

object TypeDocs:
  inline given [T]: TypeDocs[T] = ${ derivedImpl[T] }

  private def derivedImpl[T: Type](using Quotes): Expr[TypeDocs[T]] =
    import quotes.reflect.*

    val repr = TypeRepr.of[T]
    // For singleton enum cases (e.g. `case Foo`), typeSymbol points to the parent enum.
    val sym = if repr.isSingleton then repr.termSymbol else repr.typeSymbol

    val docstring = sym.docstring

    val descriptionExpr = Expr(docstring.map(parseDescription))
    val paramsExpr = Expr(docstring.fold(Map.empty[String, String])(parseParams))

    '{ TypeDocs[T]($descriptionExpr, $paramsExpr) }

  private def parseDescription(docstring: String): String =
    ScaladocParser
      .parse(docstring)
      .toSeq
      .flatMap(_.para)
      .flatMap(_.terms)
      .takeWhile {
        case _: Scaladoc.Tag => false
        case _ => true
      }
      .collect { case Scaladoc.Text(parts) => parts.map(_.part.syntax).mkString(" ") }
      .mkString(" ")
      .trim

  private def parseParams(docstring: String): Map[String, String] =
    ScaladocParser
      .parse(docstring)
      .toSeq
      .flatMap(_.para)
      .flatMap(_.terms)
      .collect { case Scaladoc.Tag(Scaladoc.TagType.Param, Some(label), desc) =>
        label.value ->
          desc
            .collect { case Scaladoc.Text(parts) => parts.map(_.part.syntax).mkString(" ") }
            .mkString(" ")
            .trim
      }
      .toMap
