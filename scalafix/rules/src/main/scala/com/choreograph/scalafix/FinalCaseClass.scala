package com.choreograph.scalafix

import scalafix.v1.*
import scala.meta.*

/** Marks all case classes as final.
  *
  * Case classes are typically leaf types in inheritance hierarchies, and making
  * them final prevents unintended inheritance which can cause issues with
  * pattern matching and equality.
  *
  * Example:
  *   - `case class Foo(x: Int)` → `final case class Foo(x: Int)`
  */
class FinalCaseClass extends SyntacticRule("FinalCaseClass") {
  import FinalCaseClass.NonFinalCaseClass

  override def fix(using doc: SyntacticDocument): Patch =
    doc.tree.collect { case NonFinalCaseClass(patch) => patch }.asPatch
}

object FinalCaseClass {
  object NonFinalCaseClass {
    def unapply(cls: Defn.Class): Option[Patch] =
      if cls.mods.collectFirst { case m @ Mod.Final() => m }.nonEmpty then None
      else cls.mods.collectFirst { case m @ Mod.Case() => Patch.addLeft(m, "final ") }
  }
}
