package com.choreograph.tyda

import scala.deriving.Mirror
import scala.quoted.*

/** A mirror for a value class.
  *
  * This supports deriving a `ValueClassMirror` for a case value class. For
  * example:
  * ```
  * final case class MyInt(i: Int) extends AnyVal
  * val mirror = summon[ValueClassMirror[MyInt]]
  * assert(mirror.fromValue(42) == MyInt(42))
  * ```
  */
trait ValueClassMirror[T] extends Mirror.Product, Serializable {
  type MirroredElemType
  type MirroredElemLabel <: String
  type MirroredType = T
  type MirroredMonoType = T
  type MirroredElemTypes = MirroredElemType *: EmptyTuple
  type MirroredElemLabels = MirroredElemLabel *: EmptyTuple

  def fromValue(u: MirroredElemType): T

  override def fromProduct(p: Product): T =
    // TYPE SAFETY: It on the caller to ensure that the Product has correct field types
    fromValue(p.productElement(0).asInstanceOf[MirroredElemType])
}

object ValueClassMirror {
  transparent inline given derive[T <: AnyVal]: ValueClassMirror[T] = ${ derivedMacro[T] }

  private def derivedMacro[T <: AnyVal: Type](using quotes: Quotes): Expr[ValueClassMirror[T]] = {
    import quotes.reflect.*

    val typeRepr = TypeRepr.of[T]
    val typeSymbol = typeRepr.typeSymbol
    val typeParams = Some(typeRepr).collect { case AppliedType(_, params) => params }

    val (fieldType, fieldName) =
      typeRepr.typeSymbol.caseFields.map(f => (typeRepr.memberType(f), f.name)) match {
        case head :: Nil => head
        case _ => report.errorAndAbort(s"${typeSymbol.name} did not have exactly one field")
      }

    val ctorRef = Select(New(TypeTree.of[T]), typeSymbol.primaryConstructor)
    val ctor = typeParams.fold(ctorRef)(ctorRef.appliedToTypes)

    val fieldNameType = ConstantType(StringConstant(fieldName)).asType

    // TODO: Could we avoid creating a new class at each call site?
    (fieldType.asType, fieldNameType) match {
      case ('[underlying], '[fieldName]) => '{
          new ValueClassMirror[T] {
            type MirroredElemType = underlying
            type MirroredElemLabel = fieldName & String
            def fromValue(u: MirroredElemType): T = ${ Apply(ctor, List('u.asTerm)).asExprOf[T] }
          }
        }
      case _ =>
        /* This might not be needed when https://github.com/scala/scala3/pull/24940 improves the exhaustivity
         * checks. */
        report.errorAndAbort(s"Failed to create mirror for values class with field type ${fieldType.asType}")
    }
  }
}
