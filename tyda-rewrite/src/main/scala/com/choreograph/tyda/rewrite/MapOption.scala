package com.choreograph.tyda.rewrite

import com.choreograph.tyda.ExprNode

private[tyda] object MapOption {

  /** Matches an if-then-else expression and extracts the condition,
    * then-branch, and else-branch.
    */
  private object IfThenElse {
    def unapply[T](
        expr: ExprNode[T]
    ): Option[(cond: ExprNode[Boolean], thenBranch: ExprNode[T], elseBranch: ExprNode[T])] =
      expr match {
        case ExprNode.Cases(ExprNode.WhenThen(cond, thenBranch), Seq(), elseBranch) =>
          Some((cond, thenBranch, elseBranch))
        case _ => None
      }
  }

  /** Matches an ExprNode[Option[U]] that is created by mapping over an Option
    * and extracts the argument and body of the mapping function.
    */
  def unapply[U](expr: ExprNode[Option[U]]): Option[(arg: ExprNode.KnownNotNull[?], body: ExprNode[U])] =
    expr match {
      case IfThenElse(ExprNode.Not(IsNone(opt)), ExprNode.MakeSome(body), ExprNode.None(_)) =>
        Some((ExprNode.KnownNotNull(opt), body))
      case IfThenElse(IsNone(opt), ExprNode.None(_), ExprNode.MakeSome(body)) =>
        Some((ExprNode.KnownNotNull(opt), body))
      case _ => None
    }
}
