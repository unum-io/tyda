package com.choreograph.scalafix

import scala.meta.*

import metaconfig.ConfDecoder
import metaconfig.Configured
import scalafix.patch.Patch
import scalafix.v1.*

class ExplicitTypeArgs(cfg: ExplicitTypeArgs.Config) extends SemanticRule("ExplicitTypeArgs") {

  def this() = this(ExplicitTypeArgs.Config())

  override def withConfiguration(configuration: Configuration): Configured[Rule] =
    configuration.conf.getOrElse("ExplicitTypeArgs")(cfg).map(newCfg => new ExplicitTypeArgs(newCfg))

  private val selectMatcher = SymbolMatcher.normalized(cfg.methods*)

  private def allOrNone[T](seq: Seq[Option[T]]): Option[Seq[T]] =
    seq.foldLeft(Option(Seq.empty[T])) {
      case (Some(acc), Some(value)) => Some(acc :+ value)
      case _ => None
    }

  private def prettyType(tpe: SemanticType): Option[(String, Seq[Symbol])] =
    tpe match {
      case TypeRef(_, symbol, args) =>
        val name = symbol.displayName
        if (args.nonEmpty) {
          allOrNone(args.map(prettyType)).map { typePairs =>
            val imports = typePairs.flatMap(_._2)
            val typeParams = typePairs.map(_._1).mkString(", ")
            (s"$name[$typeParams]", symbol +: imports)
          }
        } else Some(name, Seq(symbol))
      case SingleType(_, symbol) => Some(symbol.displayName, Seq(symbol))
      case _ => None
    }

  override def fix(using doc: SemanticDocument): Patch =
    doc
      .tree
      .collect { case selectMatcher(sel: Term.Select) =>
        sel.parent match {
          case Some(_: Term.ApplyType) | None => Patch.empty
          case _ =>
            sel.synthetics.collectFirst { case TypeApplyTree(_, args) if args.nonEmpty => args } match {
              case Some(args) => allOrNone(args.map(prettyType))
                  .map { result =>
                    val typeArgs = result.map(_._1).mkString(", ")
                    val newText = s"${sel.syntax}[$typeArgs]"
                    Patch.replaceTree(sel, newText) ++ result.flatMap(_._2).map(Patch.addGlobalImport)
                  }
                  .getOrElse(Patch.lint(Diagnostic(
                    id = "Explicit",
                    message =
                      s"type for ${sel.symbol} should be set explicitly, but linter failed to infer it",
                    position = sel.pos
                  )))
              case None => Patch.empty
            }
        }
      }
      .asPatch

}

object ExplicitTypeArgs {
  final case class Config(methods: Seq[String] = Seq.empty)

  object Config {
    given ConfDecoder[Config] = ConfDecoderDerivation.derived
  }
}
