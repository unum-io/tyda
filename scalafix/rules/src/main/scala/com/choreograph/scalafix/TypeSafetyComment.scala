package com.choreograph.scalafix

import scala.collection.mutable
import scalafix.v1.*
import scala.meta.*

object TypeSafetyComment {
  private val asInstanceOfMatcher: SymbolMatcher = SymbolMatcher.normalized("scala.Any#asInstanceOf")
  private val uncheckedMatcher: SymbolMatcher = SymbolMatcher.normalized("scala.unchecked")

  private def missingComment(pos: Position): Diagnostic =
    Diagnostic(
      "missingComment",
      "Unsafe operation requires comment containing a TYPE SAFETY: <explanation why it does not violate type safety>\n" +
        "Either refactor the code so the cast is not needed or add the comment.",
      pos
    )

  private def unusedDiagnostic(pos: Position): Diagnostic =
    Diagnostic("unused", "There is no unsafe operation following comment containing '// TYPE SAFETY:'", pos)

  private def ambiguousDiagnostic(pos: Position): Diagnostic =
    Diagnostic(
      "ambiguous",
      "There are multiple unsafe operations following comment containing '// TYPE SAFETY:'",
      pos
    )

  private def hasTypeSafetyComment(t: Tree): Boolean = (t.begComment.iterator ++ t.endComment.iterator)
    .flatMap(_.values)
    .exists(_.syntax.contains("TYPE SAFETY: "))

  private def unsafeOperation(using SemanticDocument): PartialFunction[Tree, Tree] = {
    case t @ Term.Select(_, _) if asInstanceOfMatcher.matches(t.symbol) => t
    case t: Pat.Typed if hasUncheckedAnnot(t) => t
  }

  private def hasUncheckedAnnot(tree: Tree)(using SemanticDocument): Boolean =
    tree.collect { case a: Mod.Annot if uncheckedMatcher.matches(a.init.tpe.symbol) => () }.nonEmpty

  /* When using Scala 3 optional braces syntax the first token of the block will be the actual code instead of
   * the opening brace. This means that when it starts with a comment it will be both a leading comment on the
   * block and the first contained Tree. This is because comments are just associated with the first token of
   * the tree. To avoid this causing ambiguous comment warnings treat the comment as just being on the first
   * contained Tree instead. */
  private def isBlockWithoutBrace(tree: Tree): Boolean =
    tree match {
      case Term.Block(head :: _) if head.tokens.headOption == tree.tokens.headOption => true
      case _ => false
    }
}

class TypeSafetyComment extends SemanticRule("TypeSafetyComment") {
  import TypeSafetyComment.*

  override def fix(using doc: SemanticDocument): Patch = {
    val patches = mutable.ArrayBuffer[Patch]()
    def walk(tree: Tree): Unit =
      tree match {
        case commented if hasTypeSafetyComment(commented) && !isBlockWithoutBrace(commented) =>
          val unsafeOps = commented.collect(unsafeOperation)
          if unsafeOps.size == 0 then { patches.append(Patch.lint(unusedDiagnostic(commented.pos))) }
          else if unsafeOps.size != 1 then { patches.append(Patch.lint(ambiguousDiagnostic(commented.pos))) }
        case t if unsafeOperation.isDefinedAt(t) => patches.append(Patch.lint(missingComment(t.pos)))
        case t => t.children.foreach(walk)
      }
    walk(doc.tree)
    patches.asPatch
  }
}
