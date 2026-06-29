package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.ExprNode

private[tyda] object SimplifyOptionIf {
  def unapply[T](expr: ExprNode[T]): Option[ExprNode[T]] =
    expr.codec match {
      case _: Codec.Option[t] => simplifyMapOption[t](expr)
      case Codec.Boolean => simplifyExistsForAll(expr)
      case _ => None
    }

  private object UselessTernary {

    /** Matches a ternary expression if(opt is null, null, expr) if expr is a
      * null-intolerant function of opt. In sql semantics, such an expression
      * will always evaluate to just expr.
      */
    def unapply[U](expr: ExprNode[Option[U]]): Option[ExprNode[Option[U]]] =
      expr match {
        case If(IsNone(opt), ExprNode.None(_), body) if NullIntolerant(body, opt) => Some(body)
        case _ => None
      }
  }

  private def simplifyMapOption[T](expr: ExprNode[Option[T]]): Option[ExprNode[Option[T]]] =
    expr match {
      case UselessTernary(simplified) => Some(simplified)
      case _ => None
    }

  private object RedundantNullGuard {

    /** Matches a boolean ternary `if(opt is null, lit, body)` where body
      * evaluates to null or false whenever opt is null, making the null-guard
      * branch redundant. Returns `NOT(opt is null) AND body` when lit is false
      * (exists pattern), or `(opt is null) OR body` when lit is true (forall
      * pattern).
      */
    def unapply(expr: ExprNode[Boolean]): Option[ExprNode[Boolean]] =
      expr match {
        case If(isNone @ IsNone(opt), ExprNode.Literal(false, _), body)
            if NullIntolerant.nullOrFalse(body, opt) => Some(ExprNode.And(ExprNode.Not(isNone), body))
        case If(isNone @ IsNone(opt), ExprNode.Literal(true, _), body)
            if NullIntolerant.nullOrFalse(body, opt) => Some(ExprNode.Or(isNone, body))
        case _ => None
      }
  }
  private def simplifyExistsForAll(expr: ExprNode[Boolean]): Option[ExprNode[Boolean]] =
    expr match {
      case RedundantNullGuard(simplified) => Some(simplified)
      case _ => None
    }
}
