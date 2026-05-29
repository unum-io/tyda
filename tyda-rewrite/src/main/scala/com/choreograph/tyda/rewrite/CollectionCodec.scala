package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec

private[tyda] object CollectionCodec {
  def unapply[T](codec: Codec[T]): Option[Codec[T]] =
    codec match {
      case ArrayCodec(_) | Codec.Map(_, _) => Some(codec)
      case _ => None
    }
}
