package com.choreograph.scalafix

import scala.meta.*
import scalafix.v1.*
import metaconfig.ConfDecoder
import scala.collection.immutable.Seq
import metaconfig.Configured
import com.choreograph.scalafix.ConfDecoderDerivation

object Disallowed {

  final case class Method(symbol: String, message: String) {
    lazy val matcher: SymbolMatcher = SymbolMatcher.normalized(symbol)
  }

  object Method {
    given ConfDecoder[Method] = ConfDecoderDerivation.derived
  }

  final case class Config(methods: Seq[Method] = Seq.empty)

  object Config {
    given ConfDecoder[Config] = ConfDecoderDerivation.derived
  }
}

class Disallowed(cfg: Disallowed.Config) extends SemanticRule("Disallowed") {

  def this() = this(Disallowed.Config())

  override def withConfiguration(configuration: Configuration): Configured[Rule] =
    configuration.conf.getOrElse("Disallowed")(cfg).map(newCfg => new Disallowed(newCfg))

  override def description: String =
    "Reports an error for every symbol listed under Disallowed.methods " +
      "and shows the custom message supplied for that symbol."

  override def fix(using doc: SemanticDocument): Patch =
    doc
      .tree
      .collect {
        case t: Tree if !t.symbol.isNone =>
          cfg
            .methods
            .collect {
              case m if m.matcher.matches(t.symbol) =>
                Patch.lint(Diagnostic(id = "Method", message = m.message, position = t.pos))
            }
      }
      .flatten
      .asPatch
}
