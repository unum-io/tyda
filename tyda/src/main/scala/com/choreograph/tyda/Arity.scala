package com.choreograph.tyda

import scala.compiletime.constValue
import scala.deriving.Mirror

opaque type Arity[T] = Int

object Arity {
  def apply[T: Arity]: Arity[T] = summon

  def of[T: Arity]: Int = Arity[T]

  inline given [T](using m: Mirror.Of[T]): Arity[T] = constValue[Tuple.Size[m.MirroredElemTypes]]
}
