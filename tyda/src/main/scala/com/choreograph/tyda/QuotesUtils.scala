package com.choreograph.tyda

import scala.quoted.*

private object QuotesUtils {

  def tupleLabels[Labels: Type](using Quotes): Seq[String] =
    import quotes.reflect.*
    tupleLabels(TypeRepr.of[Labels])

  private def tupleLabels(using Quotes)(tpe: quotes.reflect.TypeRepr): Seq[String] =
    import quotes.reflect.*
    tpe.dealias match {
      case AppliedType(_, Seq(ConstantType(StringConstant(label)), tl)) => label +: tupleLabels(tl)
      case _: TypeRepr if tpe =:= TypeRepr.of[EmptyTuple] => Vector.empty
      case other => report.errorAndAbort(s"Unexpected label tuple `${other.show}`")
    }

  def tupleElems[Elems: Type](using Quotes): Seq[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tupleElems(TypeRepr.of[Elems])

  private def tupleElems(using Quotes)(tpe: quotes.reflect.TypeRepr): Seq[quotes.reflect.TypeRepr] =
    import quotes.reflect.*
    tpe.dealias match {
      case AppliedType(_, Seq(h, t)) => h +: tupleElems(t)
      case _: TypeRepr if tpe =:= TypeRepr.of[EmptyTuple] => Vector.empty
      case other => report.errorAndAbort(s"Unexpected tuple type `${other.show}`")
    }

  def labelsToTupleType(using Quotes)(labels: Seq[String]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    labels.foldRight(TypeRepr.of[EmptyTuple]) { (label, acc) =>
      val labelType = ConstantType(StringConstant(label))
      TypeRepr.of[*:].appliedTo(List(labelType, acc))
    }

  def typesToTupleType(using Quotes)(types: Seq[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*
    types.foldRight(TypeRepr.of[EmptyTuple])((tpe, acc) => TypeRepr.of[*:].appliedTo(List(tpe, acc)))

  private def formatNamedTupleSyntax(using
      Quotes
  )(labels: Seq[String], types: Seq[quotes.reflect.TypeRepr], truncate: Boolean)(using
      quotes.reflect.Printer[quotes.reflect.TypeRepr]
  ): String =
    if labels.isEmpty then "()"
    else
      val fields = labels.zip(types).map { case (label, tpe) => s"$label: ${tpe.show}" }

      if truncate && fields.length > 5 then
        val shown = fields.take(3)
        val remaining = fields.length - 3
        s"(${shown.mkString(", ")}, ... and $remaining more fields)"
      else s"(${fields.mkString(", ")})"

  /** Same as `tpe.show(using Printer.TypeReprShortCode)`, but formats
    * NamedTuple types in sugared syntax.
    */
  def typeNameShort(using
      Quotes
  )(
      tpe: quotes.reflect.TypeRepr,
      labels: Seq[String],
      types: Seq[quotes.reflect.TypeRepr],
      truncate: Boolean = false
  ): String = typeName(tpe, labels, types, truncate)(using quotes.reflect.Printer.TypeReprShortCode)

  /** Same as `tpe.show`, but formats NamedTuple types in sugared syntax.
    */
  def typeName(using
      Quotes
  )(
      tpe: quotes.reflect.TypeRepr,
      labels: Seq[String],
      types: Seq[quotes.reflect.TypeRepr],
      truncate: Boolean = false
  )(using quotes.reflect.Printer[quotes.reflect.TypeRepr]): String =
    if tpe.typeSymbol.fullName == "scala.NamedTuple$.NamedTuple" then
      formatNamedTupleSyntax(labels, types, truncate)
    else tpe.show
}
