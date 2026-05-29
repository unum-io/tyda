package com.choreograph.tyda

import scala.compiletime.constValueOpt
import scala.compiletime.erasedValue
import scala.compiletime.error

/** The collection of strings of a tuple of string literals.
  */
opaque type StringLiterals[T <: Tuple] <: IndexedSeq[String] = IndexedSeq[String]

object StringLiterals {
  def apply[T <: Tuple: StringLiterals]: StringLiterals[T] = summon[StringLiterals[T]]

  inline given [T <: Tuple]: StringLiterals[T] = getValues[T]

  private inline def getValues[T <: Tuple]: IndexedSeq[String] =
    inline erasedValue[T] match {
      case _: EmptyTuple => IndexedSeq.empty
      case _: (h *: t) => getValue[h] +: getValues[t]
    }

  private inline def getValue[H]: String =
    inline constValueOpt[H] match {
      case Some(head: String) => head
      case Some(_) => error("tuple has at least one element which is not a string.")
      case None => error("tuple has at least one element which is not a constant type")
    }
}
