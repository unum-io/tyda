package com.choreograph.tyda.shapeless3extras

import scala.annotation.tailrec

import shapeless3.deriving.MapF
import shapeless3.deriving.Pure
import shapeless3.deriving.TailRecM

/** Instances for Pure, MapF and TailRecM for Either[E, V].
  *
  * This is used for working with Either in a monadic context. This means that
  * errors will short-circuit on the first error instead of being collected and
  * combined. For applicative behavior, use [[EitherApInstances]] instead.
  */
private[tyda] object EitherMonadicInstances {
  given [E]: Pure[[V] =>> Either[E, V]] = [t] => (a: t) => Right(a)
  given [E]: MapF[[V] =>> Either[E, V]] = [a, b] => (fa: Either[E, a], f: a => b) => fa.map(f)
  given [E]: TailRecM[[V] =>> Either[E, V]] =
    [a, b] =>
      (a: a, f: a => Either[E, Either[a, b]]) =>
        @tailrec
        def loop(a: a): Either[E, b] =
          f(a) match {
            case Left(e) => Left(e)
            case Right(Left(a1)) => loop(a1)
            case Right(Right(b)) => Right(b)
          }
        loop(a)
}
