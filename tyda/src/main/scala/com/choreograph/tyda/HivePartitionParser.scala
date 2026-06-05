package com.choreograph.tyda

import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

import com.choreograph.tyda.shapeless3extras.EitherMonadicInstances.given
import com.choreograph.tyda.shapeless3extras.State
import com.choreograph.tyda.shapeless3extras.labelled
import com.choreograph.tyda.shapeless3extras.mapConst

// Note: This mainly tested through PartitionerSpec
private[tyda] object HivePartitionParser {
  type FieldDecoder[T] = String => T

  def makeParser[P: Codec]: String => P =
    Codec[P] match {
      case Codec.Product(tag, fields, _) =>
        given Labelling[P] =
          Labelling(tag.getClass.getSimpleName, fields.mapConst([t] => _.name).toIndexedSeq)
        val fieldParsers = fields.mapK([t] => f => fieldParser(f.codec))
        val parser = make(fieldParsers)
        path =>
          parser(path) match {
            case Right(partition) => partition
            case Left(error) => throw new RuntimeException(error)
          }
      case Codec.FromInjection(inj, to) =>
        val toParser = makeParser(using to)
        path => inj.invert(toParser(path))
      case (_: Codec.Primitive[?]) | Codec.Bytes | Codec.TimestampMicros | Codec.DurationMicros |
          Codec.Seq(_) | Codec.Map(_, _) | Codec.Option(_) | Codec.Decimal(_, _) | Codec.Product(_, _, _) |
          Codec.FromInjection(_, _) =>
        throw new RuntimeException(s"Unsupported partition type for Hive partition decoding: ${Codec[
            P
          ]}. Only Product types are supported.")
    }

  private def fieldParser[T](codec: Codec[T]): FieldDecoder[T] =
    codec match {
      case Codec.Byte => _.toByte
      case Codec.Short => _.toShort
      case Codec.Int => _.toInt
      case Codec.Long => _.toLong
      case Codec.Boolean => _.toBoolean
      case Codec.Float => _.toFloat
      case Codec.Double => _.toDouble
      case Codec.String => PartitionEncoding.decode(_)
      case Codec.Date => str =>
          Date.fromIsoString(str).getOrElse(throw new RuntimeException(s"Unable to decode $str as a Date"))
      case Codec.FromInjection(inj, to) => fieldParser(to).andThen(inj.invert)
      case Codec.Bytes | Codec.TimestampMicros | Codec.DurationMicros | Codec.Seq(_) | Codec.Map(_, _) | Codec
            .Option(_) | Codec.Decimal(_, _) | Codec.Product(_, _, _) | Codec.FromInjection(_, _) =>
        throw new RuntimeException(s"Unsupported codec for hive partition decoding: $codec")
    }

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
