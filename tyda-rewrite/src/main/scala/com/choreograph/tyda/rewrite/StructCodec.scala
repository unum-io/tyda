package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec

private[tyda] object StructCodec {
  def unapply[T](codec: Codec[T]): Boolean =
    codec match {
      case Codec.Product(_, _, _) => true
      case Codec.Option(Codec.Option(_)) => true
      case Codec.FromInjection(_, to) => unapply(to)
      case _ => false
    }
}
