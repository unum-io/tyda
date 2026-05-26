package com.choreograph.scalafix

import scalafix.v1.*
import scala.meta.*

/** Rewrites if-then-else expressions with None/Some branches to Option.when
  * calls.
  *
  * Examples:
  *   - `if condition then None else Some(value)` →
  *     `Option.when(!condition)(value)`
  *   - `if condition then Some(value) else None` →
  *     `Option.when(condition)(value)`
  */
class OptionWhen extends SemanticRule("OptionWhen") {
  import OptionWhen.{ScalaNone, ScalaSome, negateCondition, createOptionWhen}

  override def fix(using doc: SemanticDocument): Patch =
    doc
      .tree
      .collect {
        case ifExpr @ Term.If.After_4_4_0(condition, ScalaNone(), ScalaSome(value), List()) => Patch
            .replaceTree(ifExpr, createOptionWhen(negateCondition(condition), value))
        case ifExpr @ Term.If.After_4_4_0(condition, ScalaSome(value), ScalaNone(), List()) => Patch
            .replaceTree(ifExpr, createOptionWhen(condition.syntax, value))
      }
      .asPatch
}

object OptionWhen {
  private val NoneSymbol = SymbolMatcher.exact("scala/None.")
  private val SomeSymbol = SymbolMatcher.exact("scala/Some.")

  /** Extractor for None terms that resolve to scala.None */
  private object ScalaNone {
    def unapply(term: Term)(using doc: SemanticDocument): Boolean =
      term match {
        case none @ Term.Name(_) if NoneSymbol.matches(none.symbol) => true
        case _ => false
      }
  }

  /** Extractor for Some(value) terms that resolve to scala.Some */
  private object ScalaSome {
    def unapply(term: Term)(using doc: SemanticDocument): Option[Term] =
      term match {
        case some @ Term.Apply.After_4_6_0(Term.Name(_), Term.ArgClause(List(value), None))
            if SomeSymbol.matches(some.symbol) => Some(value)
        case _ => None
      }
  }

  // Negate a condition avoid unnecessary parentheses for some simple cases
  private def negateCondition(condition: Term): String =
    condition match {
      case Term.Name(_) | Term.Select(_, _) => s"!${condition.syntax}"
      case _ => s"!(${condition.syntax})"
    }

  private def createOptionWhen(condition: String, value: Term): String =
    s"Option.when($condition)(${value.syntax})"
}
