package com.choreograph.tyda

import scala.compiletime.constValue
import scala.deriving.Mirror
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes

/** Type class that extracts default values from field definitions of a case
  * class.
  *
  * The default values are obtained using macros by looking for generated
  * methods of the companion object of the case class. This seems to be the only
  * way to obtain default values for case class fields in Scala 3. And is how it
  * is done in other libraries like circe [0], magnolia [1]. Hopefully, this can
  * be replace by some builin support in the future [3].
  *
  * [0]
  * https://github.com/circe/circe/blob/400433e44cf8821caec34568e7f9e1384febf329/modules/core/shared/src/main/scala-3/io/circe/derivation/Default.scala#L73
  *
  * [1]
  * https://github.com/softwaremill/magnolia/blob/08c224f4a55904bc94060f9bcf7fac0940e2949e/core/src/main/scala/magnolia1/macro.scala#L73
  *
  * [3] https://github.com/scala/scala3/discussions/17893
  */
trait Defaults[T] {
  type Out <: Tuple
  def defaults: Tuple.Map[Out, Option]
}

object Defaults {
  type Aux[T, Out0 <: Tuple] = Defaults[T] { type Out = Out0 }

  private final case class Impl[T, ElemTypes <: Tuple](defaults: Tuple.Map[ElemTypes, Option])
      extends Defaults[T] {
    type Out = ElemTypes
  }

  inline given derive[T](using m: Mirror.ProductOf[T]): Defaults.Aux[T, m.MirroredElemTypes] =
    // TYPE SAFETY: The defaults should types should match the types from the Mirror
    Impl[T, m.MirroredElemTypes](getDefaults[T](constValue[Tuple.Size[m.MirroredElemTypes]]).asInstanceOf)

  private inline def getDefaults[T](inline s: Int): Tuple = ${ getDefaultsImpl[T]('s) }

  private def getDefaultsImpl[T: Type](s: Expr[Int])(using Quotes): Expr[Tuple] =
    import quotes.reflect.*

    val companion = TypeRepr.of[T].typeSymbol.companionModule

    val typeArgs = TypeRepr.of[T] match {
      case AppliedType(_, args) => Some(args)
      case _ => None
    }

    val expressions: List[Expr[Option[Any]]] = List.tabulate(s.valueOrAbort) { i =>
      val termOpt = companion
        .declaredMethod(s"$$lessinit$$greater$$default$$${i + 1}")
        .headOption
        .map { defaultSymbol =>
          val select = Ref(companion).select(defaultSymbol)
          typeArgs.fold(select)(select.appliedToTypes)
        }

      termOpt match
        case None => Expr(None)
        case Some(et) => '{ Some(${ et.asExpr }) }
    }
    Expr.ofTupleFromSeq(expressions)
}
