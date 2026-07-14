package com.choreograph.tyda

import scala.deriving.Mirror
import scala.quoted.*

/** UnionMirror supports deriving a sum mirror for union types.
  *
  * Note: The ordinal method is implemented using pattern matching, which does
  * not work for all union types. Users are expected to turn on enough warnings
  * to detect problematic cases like `List[Int] | List[String]`. Because this
  * macro does not try to detect problematic cases on its own.
  *
  * Note: This private to tyda while we experiment with it and learn if a good
  * way of doing this.
  */
private[tyda] trait UnionMirror[U] extends Mirror.Sum {
  type MirroredType = U
  type MirroredMonoType = U
  type MirroredElemTypes <: Tuple
}

private[tyda] object UnionMirror {
  transparent inline given derived[U]: Mirror.SumOf[U] = ${ deriveMirror[U] }

  private def deriveMirror[U](using Quotes, Type[U]): Expr[UnionMirror[U]] =
    import quotes.reflect.*

    def unionTypes(t: TypeRepr): List[TypeRepr] =
      def flatten(t: TypeRepr): List[TypeRepr] =
        t.dealias match {
          case OrType(l, r) => flatten(l) ::: flatten(r)
          case other => List(other)
        }
      t.dealias match {
        case or @ OrType(_, _) => flatten(or)
        case other => report.errorAndAbort(s"Expected a union type, but found: ${other.show}")
      }

    def mkTupleTpe(elems: List[TypeRepr]): TypeRepr = {
      val consType = TypeRepr.of[*:].typeSymbol.typeRef
      elems.foldRight(TypeRepr.of[EmptyTuple.type])((h, acc) => AppliedType(consType, List(h, acc)))
    }

    val alts: List[TypeRepr] = unionTypes(TypeRepr.of[U]).distinct.sortBy(_.show)
    val labels = alts.map(_.show(using Printer.TypeReprShortCode))
    // Fold with value on the left so that show renders the union in the same
    // left-to-right order as the sorted alts list (e.g. "Int | String" for
    // alts = [Int, String]). Swapping the arguments would reverse the label.
    val normalizedUnion = alts.reduce((acc, value) => OrType(value, acc))

    val elemsType = mkTupleTpe(alts).asType
    val labelsType = mkTupleTpe(labels.map(l => ConstantType(StringConstant(l)))).asType

    val ordinalLambda = Lambda(
      Symbol.spliceOwner,
      MethodType(List("x"))(_ => List(TypeRepr.of[U]), _ => TypeRepr.of[Int]),
      (_, params) => {
        assert(params.length == 1, "Expected exactly one parameter for ordinal function")
        val arg = Ref(params.head.symbol)
        val cases: List[CaseDef] = alts
          .zipWithIndex
          .map { (altTpe, idx) =>
            CaseDef(Typed(arg, TypeTree.of(using altTpe.asType)), None, Literal(IntConstant(idx)))
          }
        Match(arg, cases)
      }
    )

    val mirroredLabelType =
      ConstantType(StringConstant(normalizedUnion.show(using Printer.TypeReprShortCode))).asType

    elemsType match {
      case '[elems] => labelsType match {
          case '[labels] => mirroredLabelType match {
              case '[label] => '{
                  new UnionMirror[U] {
                    type MirroredLabel = label & String
                    type MirroredElemTypes = elems & Tuple
                    type MirroredElemLabels = labels & Tuple
                    def ordinal(x: U): Int = ${ ordinalLambda.asExprOf[U => Int] }(x)
                  }
                }
            }
        }
    }
}
