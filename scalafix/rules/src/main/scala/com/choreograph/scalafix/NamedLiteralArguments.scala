package com.choreograph.scalafix

import scalafix.v1.*
import scala.meta.*
import scala.meta.Term.ArgClause

object NamedLiteralArguments {
  def isUsing(args: ArgClause): Boolean = args.mod.exists(_.is[Mod.Using])
}

class NamedLiteralArguments extends SemanticRule("NamedLiteralArguments") {
  import NamedLiteralArguments.*

  override def fix(using doc: SemanticDocument): Patch =
    doc
      .tree
      .collect {
        case apply @ Term.Apply.After_4_6_0(fun, args) if !isUsing(args) =>
          args
            .zipWithIndex
            .collect { case (t @ Lit.Boolean(_), i) =>
              fun.symbol.info match {
                case Some(info) if info.isScala =>
                  info.signature match {
                    case method: MethodSignature if method.parameterLists.nonEmpty =>
                      @scala.annotation.tailrec
                      def countOuterApplications(acc: Int, t: Tree): Int =
                        t.parent match {
                          case Some(p @ Term.Apply.After_4_6_0(inner, args)) if inner eq t =>
                            countOuterApplications(if isUsing(args) then acc else acc + 1, p)
                          case _ => acc
                        }
                      val explicitParamLists =
                        method.parameterLists.filter(_.forall(p => !p.isImplicit && !p.isGiven))
                      val paramListIndex = explicitParamLists.length - 1 - countOuterApplications(0, apply)
                      val parameter = explicitParamLists(paramListIndex)(i)
                      val parameterName = parameter.displayName
                      Patch.addLeft(t, s"$parameterName = ").atomic
                    case _ =>
                      // The symbol is not a method with matching signature, do nothing
                      Patch.empty
                  }
                case _ =>
                  // Do nothing, no information about this symbol.
                  Patch.empty
              }
            }
      }
      .flatten
      .asPatch
}
