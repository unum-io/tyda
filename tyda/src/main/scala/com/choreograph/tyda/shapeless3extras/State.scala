package com.choreograph.tyda.shapeless3extras

import shapeless3.deriving.Ap
import shapeless3.deriving.MapF
import shapeless3.deriving.Pure
import shapeless3.deriving.TailRecM

/** A state monad transformer that works in a monadic context.
  *
  * This is used for working with state in a monadic context. For applicative
  * behavior, use [[StateAp]] instead.
  *
  * Based on State monads that exist in cats and scalaz but for use with
  * shapeless3's [[shapeless3.deriving.Pure]], [[shapeless3.deriving.MapF]] and
  * [[shapeless3.deriving.TailRecM]] typeclasses.
  */
private[tyda] opaque type State[F[_], S, A] >: S => F[(S, A)] = S => F[(S, A)]

private[tyda] object State {
  def apply[F[_], S, A](f: S => F[(S, A)]): State[F, S, A] = f

  type For[F[_], S] = [A] =>> State[F, S, A]

  extension [F[_], S, A](state: State[F, S, A]) {
    def run(s0: S): F[(S, A)] = state(s0)

  }

  extension [F[_]: MapF as map, S, A](state: State[F, S, A]) {
    def map[B](f: A => B): State[F, S, B] = s0 => map(state(s0), (s1, a) => (s1, f(a)))

    def eval(s0: S): F[A] = map(state.run(s0), _._2)
  }

  extension [F[_]: {TailRecM as tailRecM, MapF as map}, S, A](state: State[F, S, A]) {
    def flatMap[B](f: A => State[F, S, B]): State[F, S, B] =
      s0 =>
        tailRecM(
          None,
          {
            case None => map(state.run(s0), (s1, a) => Left(Some((s1, a))))
            case Some((s1, a)) => map(f(a).run(s1), (s2, b) => Right((s2, b)))
          }
        )
  }

  given [F[_]: Pure as pure, S] => Pure[For[F, S]] = [A] => value => state => pure((state, value))
  given [F[_]: MapF as mapF, S]
    => MapF[For[F, S]] = [A, B] => (state: State[F, S, A], f: A => B) => state.map(f)
  given [F[_]: {TailRecM as tailRecM, MapF as map}, S]
    => Ap[
      For[F, S]
    ] = [A, B] => (ff: State[F, S, A => B], fa: State[F, S, A]) => ff.flatMap(f => fa.map(a => f(a)))
  given [F[_]: {TailRecM as tailRecM, MapF as map}, S]
    => TailRecM[
      For[
        F,
        S
      ]
    ] =
    [A, B] =>
      (a: A, f: A => State[F, S, Either[A, B]]) =>
        s0 =>
          tailRecM(
            (s0, a),
            (s1, a1) =>
              map(
                f(a1)(s1),
                (s2, ab) =>
                  ab match {
                    case Left(b) => Left((s2, b))
                    case Right(b) => Right((s2, b))
                  }
              )
          )
}
