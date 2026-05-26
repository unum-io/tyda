package com.choreograph.tyda.rewrite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.functions.raiseError
import com.choreograph.tyda.functions.ternary
import com.choreograph.tyda.rewrite.Rule.ApplyOrder

/** Adds a check that throws an error if an attempt is made to construct a map
  * from a sequence with non-distinct keys.
  *
  * This is used with backends that do not implement such a check natively to
  * ensure consistent behavior across all backends.
  */
object CheckMapKeysDistinct extends ExprRule {
  private def getLiteral[K](k: ExprNode[K]): Option[K] =
    k match {
      case ExprNode.Literal(value, _) => Some(value)
      case _ => None
    }

  private def getKey[K, V](node: ExprNode[(K, V)]): Option[K] =
    node match {
      case ExprNode.MakeProduct(entries, _) =>
        // TYPE SAFETY: The relationship between P and Elems in MakeProduct[P, Elems]
        //              is that if m: Mirror.ProductOf[P] then then Elems = m.MirroredElemTypes.
        //              Therefore entries(0) is of type ExprNode[K], although this is not
        //              currently reflected in the type system.
        getLiteral(entries(0).asInstanceOf[ExprNode[K]])
      case _ => None
    }

  private def getKeys[K, V](node: ExprNode[Seq[(K, V)]]): Option[Seq[K]] =
    node match {
      case ExprNode.MakeSeq(entries, _) =>
        val keys = entries.map(getKey)
        Option.when(keys.forall(_.isDefined))(keys.flatten)
      case _ => None
    }

  def unapply[K, V](node: ExprNode.MakeMap[K, V]): Option[ExprNode[Map[K, V]]] = {
    val knownUniqueKeys = getKeys(node.entries).exists(keys => keys.distinct.size == keys.size)
    Option.when(!knownUniqueKeys) {
      given Codec[Map[K, V]] = node.codec
      val entries = Expr.lift(node.entries)
      val check = entries.size == entries.map(_._1).distinct.size
      Expr.unlift(ternary(check, Expr.lift(node), raiseError("Duplicate keys found")))
    }
  }

  override def apply[T](node: ExprNode[T]): ExprNode[T] =
    node match {
      case CheckMapKeysDistinct(checked) => checked
      case _ => node
    }

  override def applyOrder: ApplyOrder = ApplyOrder.BottomUp
}
