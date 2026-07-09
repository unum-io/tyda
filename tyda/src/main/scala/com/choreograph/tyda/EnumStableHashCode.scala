package com.choreograph.tyda

import scala.util.hashing.MurmurHash3

/** Mixin that provides a stable hashCode for Enums.
  *
  * Singletons case for enums do not have stable hashCode (at least as of Scala
  * 3.7.0 but according to https://github.com/scala/scala3/issues/19177 will in
  * the future). This is a problem when an enum is used in a RDD that get
  * partitioned using the HashPartitioner. When it been fixed upstream, this
  * mixin can be removed.
  *
  * Note: That stable here only promises that the hashCode is stable across jvm
  * executions but the implementation might be changed in the future.
  */
@deprecated("No longer needed since Scala 3.7.3")
trait EnumStableHashCode {
  self: scala.reflect.Enum =>
  override def hashCode(): Int = MurmurHash3.productHash(this)
}
