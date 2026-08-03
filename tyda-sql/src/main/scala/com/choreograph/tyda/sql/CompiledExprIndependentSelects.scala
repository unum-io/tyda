package com.choreograph.tyda.sql

import scala.annotation.tailrec

import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.ExprNode.Reference
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.sql.SelectBuilder.FinalSelect
import com.choreograph.tyda.sql.SelectBuilder.finalSelect

private object CompiledExprIndependentSelects {

  extension [T](seq: NonEmpty[Seq[Option[T]]]) {
    def sequence: Option[NonEmpty[Seq[T]]] =
      Option.when(seq.forall(_.nonEmpty))(NonEmpty.from(seq.flatten)).flatten
  }

  def unapply[R, T](
      expr: CompiledExpr[T, R]
  ): Option[NonEmpty[Seq[(fieldName: String, fieldAccesses: NonEmpty[Seq[String]])]]] = {
    val exprs = finalSelect(expr.arg, expr.expr) match {
      case FinalSelect.Multiple(exprs) => exprs
      case _ => return None
    }

    val arg = expr.arg
    exprs
      .map { case (expr, fieldName) =>
        (expr.expr match {
          case ExprNodeNestedSelects(`arg`, selects) if selects.size == 1 => Some(selects)
          case _ => None
        }).map((fieldName = fieldName, fieldAccesses = _))
      }
      .sequence
      .filter { selects =>
        val firsts = selects.map(_.fieldAccesses.head)
        firsts.distinct.length == firsts.length
      }
  }
}

private object ExprNodeNestedSelects {

  @tailrec
  private def impl(
      node: ExprNode[?],
      acc: Seq[String]
  ): (Option[(ExprNode.Reference[?], NonEmpty[Seq[String]])]) =
    node match {
      case arg @ ExprNode.Reference(_, _) => NonEmpty.from(acc).map((arg, _))
      case ExprNode.Select(innerNode, name) => impl(innerNode, name +: acc)
      case ExprNode.MakeProduct(values, codec) => acc match {
          case head +: tail =>
            val idx = codec.fields.mapConst([t] => _.name).indexOf(head)
            tupleInstances(values).mapConst([t] => identity(_)).lift(idx) match {
              case Some(node) => impl(node, tail)
              case None => None
            }
          case Nil => None
        }
      case _ => None
    }

  def unapply(node: ExprNode[?]): Option[(ExprNode.Reference[?], NonEmpty[Seq[String]])] = impl(node, Seq())
}
