package com.choreograph.tyda

import scala.deriving.Mirror
import scala.quoted.*

import com.choreograph.tyda.QuotesUtils.tupleElems
import com.choreograph.tyda.QuotesUtils.tupleLabels
import com.choreograph.tyda.QuotesUtils.typeName
import com.choreograph.tyda.QuotesUtils.typeNameShort

sealed trait Projector[Source, Target] {
  def apply(from: Source): Target
}

object Projector {
  def apply[Source, Target](using projector: Projector[Source, Target]): Projector[Source, Target] = projector

  type From[Source] = [Target] =>> Projector[Source, Target]
  type To[Target] = [Source] =>> Projector[Source, Target]

  inline given derived[Source: Mirror.ProductOf as sourceMirror, Target: Mirror.ProductOf as targetMirror]
      : Projector[Source, Target] =
    makeUnsafe(
      projectorIndices[
        Source,
        Target,
        sourceMirror.MirroredElemLabels,
        sourceMirror.MirroredElemTypes,
        targetMirror.MirroredElemLabels,
        targetMirror.MirroredElemTypes
      ],
      targetMirror
    )

  /** Accessor for creating UnsafeProjector so that it can be private.
    * Constructing it directly in the line derive method causes compiler errors.
    */
  private def makeUnsafe[Source, Target](
      indices: Seq[Int],
      targetMirror: Mirror.ProductOf[Target]
  ): Projector[Source, Target] = UnsafeProjector[Source, Target](indices, targetMirror)

  private class ProjectedProduct(product: Product, indices: Seq[Int]) extends Product {
    def canEqual(that: Any): Boolean = product.canEqual(that)
    def productArity: Int = indices.length
    def productElement(n: Int): Any = product.productElement(indices(n))
  }

  private class UnsafeProjector[Source, Target](indices: Seq[Int], targetMirror: Mirror.ProductOf[Target])
      extends Projector[Source, Target] {
    def apply(source: Source): Target =
      /* TYPE SAFETY: Since we require a Mirror.ProductOf for Source that cast should be safe. But we can not
       * require an upper bounds of Product for Source since NamedTuples are only a subtype of Product at
       * runtime. */
      targetMirror.fromProduct(ProjectedProduct(source.asInstanceOf[Product], indices))
  }

  inline def projectorIndices[Source, Target, SourceLabels, SourceTypes, TargetLabels, TargetTypes]
      : Seq[Int] =
    ${ projectorIndicesImpl[Source, Target, SourceLabels, SourceTypes, TargetLabels, TargetTypes] }

  private def projectorIndicesImpl[
      Source: Type,
      Target: Type,
      SourceLabels: Type,
      SourceTypes: Type,
      TargetLabels: Type,
      TargetTypes: Type
  ](using Quotes): Expr[Seq[Int]] =
    import quotes.reflect.{TypeRepr, Printer, report}

    val sourceLabels = tupleLabels[SourceLabels]
    val sourceTypes = tupleElems[SourceTypes]

    val sourceFieldMap = sourceLabels.zip(sourceTypes.zipWithIndex).toMap

    val targetLabels = tupleLabels[TargetLabels]
    val targetTypes = tupleElems[TargetTypes]

    def showShortType(tpe: TypeRepr): String = tpe.show(using Printer.TypeReprShortCode)

    lazy val sourceTypeNameTruncated =
      typeNameShort(TypeRepr.of[Source], sourceLabels, sourceTypes, truncate = true)
    lazy val targetTypeNameTruncated =
      typeNameShort(TypeRepr.of[Target], targetLabels, targetTypes, truncate = true)

    val errors = targetLabels
      .zip(targetTypes)
      .flatMap { case (label, targetType) =>
        sourceFieldMap.get(label) match {
          case None => Some(s"no field named '$label' in $sourceTypeNameTruncated")
          case Some((sourceType, _)) if !(sourceType =:= targetType) =>
            val sourceShort = showShortType(sourceType)
            val targetShort = showShortType(targetType)
            Some(
              s"field '$label' has type $sourceShort in $sourceTypeNameTruncated but $targetShort in $targetTypeNameTruncated"
            )
          case _ => None
        }
      }

    if errors.nonEmpty then {
      val sourceTypeName = typeName(TypeRepr.of[Source], sourceLabels, sourceTypes)
      val targetTypeName = typeName(TypeRepr.of[Target], targetLabels, targetTypes)
      report.errorAndAbort(
        s"Cannot derive Projector from $sourceTypeName to $targetTypeName\n\n" + errors.mkString("\n")
      )
    }

    val indices = targetLabels.map(sourceFieldMap).map(_._2)
    Expr(indices)
}
