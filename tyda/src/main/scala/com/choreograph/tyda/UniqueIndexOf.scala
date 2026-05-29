package com.choreograph.tyda

import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.error
import scala.compiletime.ops.int.+

import com.choreograph.tyda.compiletimeextras.constToString
import com.choreograph.tyda.compiletimeextras.typeNameShort

/** Given that provides the index of the unique occurrence of the type E in the
  * tuple T.
  *
  * If there is no ocurrence or multiple occurences no given is provided.
  */
opaque type UniqueIndexOf[T <: Tuple, E] <: Int = Int

object UniqueIndexOf {
  private inline def countOccurrences[T <: Tuple, E, N <: Int]: Int =
    inline erasedValue[T] match
      case _: EmptyTuple => constValue[N]
      case _: (E *: t) => countOccurrences[t, E, N + 1]
      case _: (_ *: t) => countOccurrences[t, E, N]

  private transparent inline def firstIndexOf[T <: Tuple, E, N <: Int]: UniqueIndexOf[T, E] =
    inline erasedValue[T] match
      case _: (E *: _) => constValue[N]
      case _: (_ *: t) => firstIndexOf[t, E, N + 1]

  private transparent inline def uniqueIndexOf[T <: Tuple, E]: UniqueIndexOf[T, E] =
    inline countOccurrences[T, E, 0] match {
      case 1 => firstIndexOf[T, E, 0]
      case n => error(
          "Type " + typeNameShort[E] + " must occur exactly once in " + typeNameShort[T] + " occured " +
            constToString(n) + " times."
        )
    }

  transparent inline given [T <: Tuple, E]: UniqueIndexOf[T, E] = uniqueIndexOf[T, E]
}
