package com.choreograph.scalafix

import metaconfig.{ConfDecoder, Configured, ConfError}
import shapeless3.deriving.K0
import shapeless3.deriving.Pure
import shapeless3.deriving.Ap
import shapeless3.deriving.MapF
import shapeless3.deriving.Labelling
import metaconfig.Conf

/** Automatic derivation for ConfDecoder using Shapeless 3.
  *
  * This is implemented since the scalafix provided macros does not work in
  * Scala 3. See:
  * https://github.com/scalacenter/scalafix/pull/2034/files#r2182618708
  */
object ConfDecoderDerivation {
  private def error(message: String): Configured[Nothing] = ConfError.empty.copy(message).notOk

  inline def derived[A: {Labelling as labelling, K0.ProductInstancesOf[ConfDecoder] as instances}]
      : ConfDecoder[A] =
    conf =>
      conf match {
        case obj: Conf.Obj =>
          val labelIt = labelling.elemLabels.iterator
          instances.constructA[Configured]([t] =>
            decoder =>
              val label = labelIt.next()
              obj.get(label)(using decoder)
          )(pure, map, ap)
        case _ => error(s"Expected object with fields for ${labelling.label}, found $conf")
      }

  private val pure: Pure[Configured] = [a] => (a: a) => Configured.Ok(a)
  private val map: MapF[Configured] = [a, b] => (fa: Configured[a], f: a => b) => fa.map(f)
  private val ap: Ap[Configured] = [a, b] =>
    (ff: Configured[a => b], fa: Configured[a]) => ff.flatMap(f => fa.map(f))

  extension [A](fa: Configured[A]) {
    private def flatMap[B](f: A => Configured[B]): Configured[B] =
      fa match {
        case Configured.Ok(a) => f(a)
        case e: Configured.NotOk => e
      }
  }
}
