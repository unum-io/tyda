package com.choreograph.tyda.rewrite

import shapeless3.deriving.Complete

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.unreachable

/** This rewrites equals of products with nullable fields and arrays into equals
  * on the components.
  *
  * This with query engines like BigQuery that does not support equals with
  * arrays and does not support null equals semantics for structs.
  *
  * Note: This pass only removes one level. It should be applied inside a
  * transformDown to recusivly rewrite all desried equals.
  */
object DistributeProductAndSeqEquals extends ExprRule {
  def unapply[T](equals: ExprNode.Equals[T]): Option[ExprNode[Boolean]] = {
    given codec: Codec[T] = equals.lhs.codec
    Option.when(containsNullableFieldsOrArray(codec))(distribute(equals.lhs, equals.rhs))
  }

  def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      // Simple null checks do not need to be transformed
      case IsNone(_) => node
      case DistributeProductAndSeqEquals(equals) => equals
      case _ => node
    }

  private def containsNullableFieldsOrArray(codec: Codec[?]): Boolean =
    Codec
      .iterate(codec)
      .exists {
        case Codec.Seq(_) | Codec.Option(Codec.Option(_)) => true
        case Codec.Product(_, fields, _) => fields.foldLeft0(false)([t] =>
            (_, f) =>
              f.codec match {
                case Codec.Option(_) => Complete(true)
                case _ => false
              }
          )
        case _ => false
      }

  private def distribute[T](lhs: ExprNode[T], rhs: ExprNode[T]): ExprNode[Boolean] =
    lhs.codec match {
      case _: Codec.Primitive[T] => unreachable("The codec should have contained a product or array")
      case Codec.Map(_, _) => unreachable("Map does not support equality")
      case _: Codec.Option[e] => transform[Option[e]](
          lhs,
          rhs,
          (l, r) => l.isEmpty == r.isEmpty && l.zip(r).forall { case Expr(someL, someR) => someL == someR }
        )
      case _: Codec.Seq[e] => transform[Seq[e]](
          lhs,
          rhs,
          (l, r) => l.size == r.size && l.zip(r).forall { case Expr(elemL, elemR) => elemL == elemR }
        )
      case Codec.Product(_, fields, _) => fields.foldLeft0(ExprNode.Literal(true))([t] =>
          (acc, field) =>
            val l = ExprNode.Select(lhs, field.name)
            val r = ExprNode.Select(rhs, field.name)
            ExprNode.And(acc, ExprNode.Equals(l, r))
        )
      case inj @ Codec.FromInjection(_, _) => distribute(ExprNode.ToRepr(lhs, inj), ExprNode.ToRepr(rhs, inj))
    }

  private def transform[T](
      l: ExprNode[T],
      r: ExprNode[T],
      f: (Expr[T], Expr[T]) => Expr[Boolean]
  ): ExprNode[Boolean] = Expr.unlift(f(Expr.lift(l), Expr.lift(r)))

  override def applyOrder: Rule.ApplyOrder = Rule.ApplyOrder.TopDown
}
