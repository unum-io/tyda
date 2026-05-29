package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.rewrite.ReplacementMap.Replacement
import com.choreograph.tyda.unreachable

/** Map from ExprNode to ExprNode that captures that key and value have the same
  * type.
  */
class ReplacementMap private (private val replacements: Seq[Replacement[?]]) {
  private val lookup: Map[ExprNode[?], Int] = replacements.map(_.key).zipWithIndex.toMap

  def getOrElse[T](key: ExprNode[T], default: => ExprNode[T]): ExprNode[T] = get(key).getOrElse(default)

  def get[T](key: ExprNode[T]): Option[ExprNode[T]] =
    lookup
      .get(key)
      .map(idx =>
        // The match here is used to infer a GADT bound that proves the the return value has correct type
        replacements(idx) match {
          case Replacement(`key`, value) => value
          case _ => unreachable("key was found in lookup but key did not match replacement key")
        }
      )
}

object ReplacementMap {
  final case class Replacement[T](key: ExprNode[T], value: ExprNode[T])
  def apply(pairs: Seq[Replacement[?]]): ReplacementMap = new ReplacementMap(pairs)
}
