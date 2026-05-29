package com.choreograph.tyda.rewrite

import com.choreograph.tyda.NonEmpty

extension [A](seq: NonEmpty[Seq[A]]) {

  /** Reduces a non-empty IndexedSeq by recursively splitting it in half,
    * producing a balanced binary tree of reductions (log₂(n) depth).
    */
  def reduceBalanced(op: (A, A) => A): A = reduce(seq.to(IndexedSeq), 0, seq.length, op)
}

private def reduce[A](seq: IndexedSeq[A], from: Int, until: Int, op: (A, A) => A): A = {
  val length = until - from
  length match {
    case 1 => seq(from)
    case _ =>
      val mid = from + length / 2
      val left = reduce(seq, from, mid, op)
      val right = reduce(seq, mid, until, op)
      op(left, right)
  }
}
