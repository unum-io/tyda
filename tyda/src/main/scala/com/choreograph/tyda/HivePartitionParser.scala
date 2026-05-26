package com.choreograph.tyda

import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

import com.choreograph.tyda.shapeless3extras.EitherMonadicInstances.given
import com.choreograph.tyda.shapeless3extras.State
import com.choreograph.tyda.shapeless3extras.labelled

// Note: This mainly tested through PartitionerSpec
private[tyda] object HivePartitionParser {
  type FieldDecoder[T] = String => T

  def make[T: Labelling as labels](
      fieldDecoders: K0.ProductInstances[FieldDecoder, T]
  ): String => Either[String, T] = {
    type ParserState = (String, Int)
    type Result[T] = Either[String, T]
    val parser = fieldDecoders
      .labelled
      .constructM[State.For[Result, ParserState]]([t] =>
        labelAndValueDecoder =>
          val (label, decoder) = labelAndValueDecoder
          State { (path: String, pos: Int) =>
            val labelAtPos = path.length > pos + 1 && (pos == -1 || path.charAt(pos) == '/') &&
              path.startsWith(label, pos + 1) && path.charAt(pos + 1 + label.length) == '='
            if !labelAtPos then Left(s"Unable to extract hive partition ${label} from $path at $pos")
            else {
              val start = pos + label.length + 2
              val end = path.indexOf('/', start) match {
                case -1 => path.length
                case end => end
              }
              val value = path.substring(start, end)
              Right((path, end), decoder(value))
            }
          }
      )(summon, summon, summon)

    labels.elemLabels.headOption match {
      case None =>
        val singleton = parser.eval("", 0)
        _ => singleton
      case Some(firstPartition) => path => parser.eval((path, path.indexOf(s"/$firstPartition=")))
    }
  }
}
