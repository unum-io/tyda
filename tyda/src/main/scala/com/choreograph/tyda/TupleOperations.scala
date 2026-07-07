package com.choreograph.tyda

import scala.compiletime.ops.int.Max
import scala.compiletime.ops.int.S

object TupleOperations:
  /** Removes the all occurrences of type V from tuple T
    *
    * Examples:
    * ```scala
    * //{
    * import com.choreograph.tyda.TupleOperations.-
    * //}
    * // Removing String from T1
    * type T1 = (Int, String, Boolean, String)
    * summon[T1 - String =:= (Int, Boolean)]
    * ```
    *
    * ```scala
    * //{
    * import com.choreograph.tyda.TupleOperations.-
    * //}
    * // Does nothing if V is not found in T
    * type T1 = (Int, Boolean)
    * summon[T1 - String =:= (Int, Boolean)]
    * ```
    */
  infix type -[T <: Tuple, V] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case V *: tail => tail - V
    case head *: tail => head *: (tail - V)

  /** N-arity tuple with all elements of type T */
  type TupleN[T, N <: Int] <: Tuple = Max[N, 0] match
    case 0 => EmptyTuple
    case S[n] => T *: TupleN[T, n]

  type EqualSize[T1 <: Tuple, T2 <: Tuple] = Tuple.Size[T1] =:= Tuple.Size[T2]
