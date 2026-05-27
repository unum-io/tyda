package com.choreograph.tyda.rewrite

import com.choreograph.tyda.CompiledExplodeExpr
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExprOrExplode
import com.choreograph.tyda.CompiledExprOrExplode.compose
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.shapeless3extras.tupleInstances

/** Rewrites SelectN with multiple explodes into several selects.
  *
  * Will rewrite a select like
  * ```
  * SelectN: x => explode(x._1), x => explode(x._2), x => explode(x._3)
  * ```
  * into
  * ```
  * SelectN: x => x._2, x => x._3, x => explode(x._1._3)
  *   SelectN: x => x._1, x => x._2, explode(x._1._2)
  *     SelectN: x => x._1, x => explode(x._1._1)
  *      SelectN: x => Tuple1(x)
  * ```
  *
  * This rewrite is useful for Spark < 3.5.1 that does not support multiple
  * selects and for engines like postgres, datafusion where multiple unnests are
  * zipped instead of cartesian products.
  */
object RemoveMultipleExplodes extends DatasetRule {

  def unapply[T](ds: Dataset[T]): Option[Dataset[T]] =
    ds match {
      case selectN: Dataset.SelectN[u, r] if countExplodes(selectN) > 1 => Some(rewriteSelectN[u, r](selectN))
      case _ => None
    }

  def apply[T](ds: Dataset[T]): Dataset[T] = unapply(ds).getOrElse(ds)

  private def countExplodes(selectN: Dataset.SelectN[?, ?]): Int =
    tupleInstances(selectN.exprs).foldLeft0(0)([t] =>
      (acc, compiled) =>
        compiled match {
          case _: CompiledExplodeExpr[?, ?] => acc + 1
          case _: CompiledExpr[?, ?] => acc
        }
    )

  private def rewriteSelectN[T, R <: Tuple](selectN: Dataset.SelectN[T, R]): Dataset[R] = {
    val arity = tupleInstances(selectN.exprs).arity

    def step[U <: Tuple, E](
        ds: Dataset[U],
        compiled: CompiledExprOrExplode[T, E],
        index: Int
    ): Dataset[? <: Tuple] = {
      def column[T](ordinal: Int): CompiledExpr[U, T] = {
        val ref = ExprNode.Reference[U]()(using ds.codec)
        CompiledExpr(ref, ExprNode.Select[U, T](ref, s"_${ordinal}"))
      }

      // For final step exclude the row column
      val existingResults = if index == arity then (2 to index).map(column) else (1 to index).map(column)

      val row = column[T](1)
      val adaptedExpr = compiled.compose(row)

      /* TYPE SAFETY: All exprs are of type CompiledExprOrExplode.From[U] */
      val nextResults = Tuple
        .fromArray((existingResults :+ adaptedExpr).toArray)
        .asInstanceOf[Tuple.Map[Tuple, CompiledExprOrExplode.From[U]]]

      Dataset.SelectN(ds, nextResults)
    }

    val dsInitial: Dataset[T *: EmptyTuple] = selectN.input.select(_ *: EmptyTuple)
    val (result, _) =
      tupleInstances(selectN.exprs).foldLeft0((dsInitial: Dataset[? <: Tuple], 0)) { [t] => (acc, compiled) =>
        val (dsPrev, idx) = acc
        val nextDs = step(dsPrev, compiled, idx + 1)
        (nextDs, idx + 1)
      }

    /* TYPE SAFETY: In the final select each of the tuple elements is the same as the original select */
    result.asInstanceOf[Dataset[R]]
  }
}
