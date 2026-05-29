package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.NumericsReadMode.FailableReadAdapter
import com.choreograph.tyda.NumericsReadMode.buildFailableReadAdapterChildren
import com.choreograph.tyda.unreachable

/** Create an adapter from a all nullable version of a type to the original
  * type.
  */
object AsAllNullable {

  /** Create an adapter where all child nodes have been made nullable.
    *
    * The top level type is left as is, this is useful since Codec.Product and
    * Option(Codec.Product) will not have matching schemas.
    */
  def children[T](codec: Codec[T]): Option[FailableReadAdapter[T, ?]] =
    codec match {
      case Codec.Option(_) => ensureNullable(codec)
      case _ => ensureNullableChildren(codec)
    }

  private def ensureNullableChildren[T](codec: Codec[T]): Option[FailableReadAdapter[T, ?]] =
    buildFailableReadAdapterChildren(codec)([t] => ensureNullable(_))

  private def ensureNullable[T](codec: Codec[T]): Option[FailableReadAdapter[T, ?]] =
    codec match {
      case Codec.Option(element) =>
        val inner = element match {
          case Codec.Option(_) => ensureNullable(element)
          case _ => ensureNullableChildren(element)
        }
        inner.map { case FailableReadAdapter(readCodec, cast, maybeCheck) =>
          FailableReadAdapter(Codec.Option(readCodec), _.map(cast), maybeCheck.map(check => _.forall(check)))
        }
      case other => ensureNullableChildren(other) match {
          case None => Some(FailableReadAdapter(Codec.Option(other), _.get, Some(!_.isEmpty)))
          case Some(FailableReadAdapter(readCodec, cast, Some(check))) =>
            Some(FailableReadAdapter(Codec.Option(readCodec), read => cast(read.get), Some(_.exists(check))))
          case Some(FailableReadAdapter(_, _, None)) =>
            unreachable("AsAllNullable adapters always have valid checks")
        }
    }

}
