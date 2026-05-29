package com.choreograph.tyda

private[tyda] object Errors {
  def failUnexpectedReference(
      ref: ExprNode.Reference[?],
      expectedRefs: IterableOnce[ExprNode.Reference[?]]
  ): Nothing = {
    val errorMessage = s"Expr used $ref but expected one of ${expectedRefs.iterator.mkString(", ")}.\n" +
      "This is likely caused by capturing a reference to Expr and using it in a different context."
    throw RuntimeException(errorMessage)
  }
}
