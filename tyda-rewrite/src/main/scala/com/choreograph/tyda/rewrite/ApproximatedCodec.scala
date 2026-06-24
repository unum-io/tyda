package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec

/** This represent a Codec[T] that been approximated from an external schema.
  *
  * `isExact` being false means that the external schema had extra fields that
  * were not possible to represent inside Codec.
  */
final case class ApproximatedCodec[T](codec: Codec[T], isExact: Boolean)
