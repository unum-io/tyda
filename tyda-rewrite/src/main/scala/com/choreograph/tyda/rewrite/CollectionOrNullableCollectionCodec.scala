package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec

private[tyda] object CollectionOrNullableCollectionCodec {
  def unapply[T](codec: Codec[T]): Option[Codec[T]] =
    codec match {
      case Codec.Option(CollectionCodec(_)) | CollectionCodec(_) => Some(codec)
      case _ => None
    }
}
