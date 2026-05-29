package com.choreograph.tyda.shapeless3extras

import shapeless3.deriving.Ap
import shapeless3.deriving.MapF
import shapeless3.deriving.Pure

/** Instances for Pure, MapF and Ap for Either[E, V].
  *
  * This is used for working with Either in an applicative context instead of a
  * monadic one. This means that all errors will be collected and combined
  * instead of short-circuiting on the first error. For short-circuiting
  * behavior, use [[EitherMonadInstances]] instead.
  *
  * Requires that E has a given Combine[E] instance.
  */
private[tyda] object EitherApInstances {
  given [E]: Pure[[V] =>> Either[E, V]] = [t] => (a: t) => Right(a)
  given [E]: MapF[[V] =>> Either[E, V]] = [a, b] => (fa: Either[E, a], f: a => b) => fa.map(f)
  given [E: Combine as combine]: Ap[
    [V] =>> Either[
      E,
      V
    ]
  ] =
    [a, b] =>
      (_, _) match {
        case (Left(e1), Left(e2)) => Left(combine(e1, e2))
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
        case (Right(f), Right(a)) => Right(f(a))
      }
}
