package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec

private[tyda] object ArrayCodec {

  def unapply[T](codec: Codec[T]): Option[Codec[?]] =
    codec match {
      case Codec.Seq(element) => Some(element)
      case Codec.Iterable(_, element) => Some(element)
      case Codec.FromInjection(_, to) => unapply(to)
      case _ => None
    }
}
