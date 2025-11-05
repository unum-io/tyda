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

  private def simplifyMapOption[T](expr: ExprNode[Option[T]]): Option[ExprNode[Option[T]]] =
    expr match {
      case MapOption(arg, body) if NullIntollerant(ExprNode.MakeSome(body), arg) =>
        Some(ExprNode.MakeSome(body))
      case FlatMapOption(arg, body) if NullIntollerant(body, arg) => Some(body)
      case _ => None
    }

  private def simplifyExistsForAll(expr: ExprNode[Boolean]): Option[ExprNode[Boolean]] =
    expr match {
      case ExistsOption(isNone, arg, body) if NullIntollerant.nullOrFalse(body, arg) =>
        Some(ExprNode.And(ExprNode.Not(isNone), body))
      case ForallOption(isNone, arg, body) if NullIntollerant.nullOrFalse(body, arg) =>
        Some(ExprNode.Or(isNone, body))
      case _ => None
    }

  private object FlatMapOption {

    /** Matches an ExprNode[Option[U]] that is created by flatMapping over an
      * Option and extracts the argument and body of the flatMapping function.
      */
    def unapply[U](
        expr: ExprNode[Option[U]]
    ): Option[(arg: ExprNode.KnownNotNull[?], body: ExprNode[Option[U]])] =
      expr match {
        case If(IsNone(opt), ExprNode.None(_), body) => Some((ExprNode.KnownNotNull(opt), body))
        case _ => None
      }
  }

  private object ExistsOption {

    /** Matches an ExprNode[Boolean] that is created by checking existence over
      * an Option and extracts the argument and body of the existence function.
      */
    def unapply(
        expr: ExprNode[Boolean]
    ): Option[(isNone: ExprNode[Boolean], arg: ExprNode.KnownNotNull[?], body: ExprNode[Boolean])] =
      expr match {
        case If(isNone @ IsNone(opt), ExprNode.Literal(false, _), body) =>
          Some((isNone, ExprNode.KnownNotNull(opt), body))
        case _ => None
      }
  }

  private object ForallOption {

    /** Matches an ExprNode[Boolean] that is created by checking existence over
      * an Option and extracts the argument and body of the existence function.
      */
    def unapply(
        expr: ExprNode[Boolean]
    ): Option[(isNone: ExprNode[Boolean], arg: ExprNode.KnownNotNull[?], body: ExprNode[Boolean])] =
      expr match {
        case If(isNone @ IsNone(opt), ExprNode.Literal(true, _), body) =>
          Some((isNone, ExprNode.KnownNotNull(opt), body))
        case _ => None
      }
  }
}
