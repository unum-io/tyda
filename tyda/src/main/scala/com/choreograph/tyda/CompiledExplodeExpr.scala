package com.choreograph.tyda

import com.choreograph.tyda.CompiledExplodeExpr.Extracted
import com.choreograph.tyda.ExprNode.Explode
import com.choreograph.tyda.TreeApi.Continue
import com.choreograph.tyda.TreeApi.Skip
import com.choreograph.tyda.TreeApi.Stop
import com.choreograph.tyda.shapeless3extras.tupleInstances

/** Same as [[CompiledExpr]] but the result should be exploded.
  */
private[tyda] final case class CompiledExplodeExpr[T, R](arg: ExprNode.Reference[T], expr: ExprNode[R]) {
  def codec: Codec[R] = expr.codec

  /** Composes this compiled expression with another one, with this function
    * applied last.
    *
    * Same as [[Function1#compose]] but for compiled expressions.
    */
  def compose[A](g: CompiledExpr[A, T]): CompiledExplodeExpr[A, R] = {
    val newArg = ExprNode.Reference[A]()(using g.arg.codec)
    CompiledExplodeExpr(newArg, expr.replace(arg, g.expr.replace(g.arg, newArg)))
  }

  def combine[R2](g: CompiledExpr[T, R2]): CompiledExplodeExpr[T, (R, R2)] = {
    val newArg = ExprNode.Reference[T]()(using arg.codec)
    CompiledExplodeExpr(
      newArg,
      ExprNode.makeTuple[(R, R2)](expr.replace(arg, newArg), g.expr.replace(g.arg, newArg))
    )
  }

  def extractExplodes: Extracted[T, R] = {
    val explodesSeq = expr.fold(Vector.empty[CompiledExpr[T, ? <: Iterable[Any]]])((acc, node) =>
      node match {
        case ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_) => Skip(acc)
        case explode @ ExprNode.Explode(_) => Skip(acc :+ CompiledExpr(arg, explode.expr))
        case _ => Continue(acc)
      }
    )
    val explodeCodecs = explodesSeq.map(_.codec)

    if explodeCodecs.isEmpty then Extracted.NoExplodes(CompiledExpr(arg, expr))
    else {
      val argInOutput = expr.fold(false)((_, node) =>
        node match {
          case ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_) | ExprNode.Explode(_) => Skip(false)
          case `arg` => Stop(true)
          case _ => Continue(false)
        }
      )

      type Tup <: Tuple
      // TYPE SAFETY: explodeSeq is a sequence of elements with type CompiledExpr[T, Iterable[?]]
      val explodes = Tuple
        .fromArray(explodesSeq.toArray)
        .asInstanceOf[Tuple.Map[Tup, [t] =>> CompiledExpr[T, Iterable[t]]]]

      val explodeCodec = Codec.tuple(tupleInstances(explodes).mapK([t] => _.codec.element))

      def compiled[C](codec: Codec[C], offset: Int): CompiledExpr[C, R] = {
        val refOutput = ExprNode.Reference()(using codec)
        var idx = 0
        val exprWithoutExplodes = expr.transformDown([t] =>
          node =>
            node match {
              case subquery @ (ExprNode.ScalarSubquery(_) | ExprNode.ExistsSubquery(_)) => Skip(subquery)
              case ExprNode.Explode(_) =>
                idx += 1
                Skip(ExprNode.Select(refOutput, s"_${idx + offset}"))
              case `arg` => Skip(ExprNode.Select(refOutput, "_1"))
              case _ => Continue(node)
            }
        )
        CompiledExpr(refOutput, exprWithoutExplodes)
      }
      Extracted.Explodes(
        explodes,
        compiled(Codec.cons(arg.codec, explodeCodec), 1),
        Option.when(!argInOutput)(compiled(explodeCodec, 0))
      )
    }
  }
}

object CompiledExplodeExpr {
  enum Extracted[T, R] {
    case NoExplodes(compiled: CompiledExpr[T, R])
    case Explodes[T, Tup <: Tuple, R](
        explodes: Tuple.Map[Tup, [t] =>> CompiledExpr[T, Iterable[t]]],
        compiled: CompiledExpr[T *: Tup, R],
        onlyExplodesCompiled: Option[CompiledExpr[Tup, R]]
    ) extends Extracted[T, R]
  }

  type From[T] = [R] =>> CompiledExplodeExpr[T, R]

  def apply[T: Codec, R, I: AsExprOrExplode.Of[R]](f: Expr[T] => I): CompiledExplodeExpr[T, R] =
    apply(f.andThen(AsExprOrExplode(_)))

  def apply[T, R](expr: CompiledExpr[T, R]): CompiledExplodeExpr[T, R] = apply(expr.arg, expr.expr)

  def apply[T: Codec as codec, R](f: Expr[T] => Expr[R] | ExplodeExpr[R]): CompiledExplodeExpr[T, R] = {
    val arg = ExprNode.Reference[T]()
    val body = f(Expr.lift(arg)) match {
      case expr: Expr[R] => expr.node
      case explode: ExplodeExpr[R] => explode.asNode
    }
    apply(arg, body)
  }

}
