package com.choreograph.tyda.shapeless3extras

import shapeless3.deriving.Ap
import shapeless3.deriving.MapF
import shapeless3.deriving.Pure

/** StateAp is a state monad transformer that works in an applicative context.
  *
  * Defined such that we can accumulate errors if `F[_]` is for example
  * `Either[E, _]` and using [[EitherApInstances]]. Inspired by the common
  * StateT monad that exists in cats and others. Normally it would be defined
  * as: `S => F[(S, A)]` see [0], [1], but using that definition we could not
  * implement [[shapeless3.deriving.Ap]] properly; instead we would only get
  * monadic semantics.
  *
  * [0]
  * https://github.com/scalaz/scalaz/blob/e2cf558772d3f317906fbcdea27e715bceba4816/core/src/main/scala/scalaz/StateT.scala#L148
  * [1]
  * https://github.com/typelevel/cats/blob/main/core/src/main/scala/cats/data/IndexedStateT.scala#L39
  */
private[tyda] opaque type StateAp[F[_], S, A] >: S => (S, F[A]) = S => (S, F[A])

private[tyda] object StateAp {
  type For[F[_], S] = [A] =>> StateAp[F, S, A]

  extension [F[_], S, A](state: StateAp[F, S, A]) def run(s: S): (S, F[A]) = state(s)

  extension [F[_]: MapF as map, S, A](state: StateAp[F, S, A])
    def map[B](f: A => B): StateAp[F, S, B] =
      s0 => {
        val (s1, a) = state.run(s0)
        (s1, map(a, f))
      }

  given [F[_]: Pure as pure, S]: Pure[For[F, S]] = [t] => (a: t) => s => (s, pure(a))
  given [F[_]: MapF, S]: MapF[For[F, S]] = [a, b] => (fa: StateAp[F, S, a], f: a => b) => fa.map(f)
  given [F[_]: {Ap as ap, MapF as map}, S]: Ap[For[F, S]] =
    [a, b] =>
      (ff: StateAp[F, S, a => b], fa: StateAp[F, S, a]) =>
        s0 => {
          val (s1, f) = ff.run(s0)
          val (s2, a) = fa.run(s1)
          (s2, ap(f, a))
        }
}
