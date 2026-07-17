package com.choreograph.tyda.testsuites

import scala.reflect.ClassTag

import org.scalactic.Equality

import com.choreograph.tyda.Codec

object CodecToEquality {
  given [T: Codec as codec] => Equality[T] = {
    val equiv = CodecToEquiv[T]
    given ClassTag[T] = codec.classTag
    new Equality[T] {
      def areEqual(a: T, b: Any): Boolean =
        b match {
          case b: T => equiv.equiv(a, b)
          case _ => false
        }
    }
  }
}
