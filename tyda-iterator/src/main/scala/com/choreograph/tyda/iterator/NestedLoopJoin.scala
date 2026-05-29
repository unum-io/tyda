package com.choreograph.tyda.iterator

/* Implements a nested loop join with SQL join semantics */
private object NestedLoopJoin {

  def join[T, U](left: Iterator[T], right: Iterator[U], p: (T, U) => Boolean): Iterator[(T, U)] = {
    val rightAsSeq = right.toIndexedSeq
    left.flatMap(leftElem => rightAsSeq.iterator.filter(p(leftElem, _)).map((leftElem, _)))
  }

  def leftOuterJoin[T, U](
      left: Iterator[T],
      right: Iterator[U],
      p: (T, U) => Boolean
  ): Iterator[(T, Option[U])] = {
    val rightAsSeq = right.toIndexedSeq
    left.flatMap { leftElem =>
      val matches = rightAsSeq.iterator.filter(p(leftElem, _))
      if (matches.hasNext) matches.map(r => (leftElem, Some(r))) else Iterator.single((leftElem, None))
    }
  }

  def rightOuterJoin[T, U](
      left: Iterator[T],
      right: Iterator[U],
      p: (T, U) => Boolean
  ): Iterator[(Option[T], U)] = leftOuterJoin(right, left, (u, t) => p(t, u)).map(_.swap)

  def fullOuterJoin[T, U](
      left: Iterator[T],
      right: Iterator[U],
      p: (T, U) => Boolean
  ): Iterator[(Option[T], Option[U])] = {
    val rightAsSeq = right.toIndexedSeq
    val rightHadMatch = Array.fill(rightAsSeq.size)(elem = false)
    left.flatMap { leftElem =>
      val (matches, matchIndices) = rightAsSeq
        .iterator
        .zipWithIndex
        .filter { case (rightElem, _) => p(leftElem, rightElem) }
        .toSeq
        .unzip
      matchIndices.foreach(rightHadMatch(_) = true)
      if (matches.nonEmpty) matches.map(r => (Some(leftElem), Some(r)))
      else Iterator.single((Some(leftElem), None))
    } ++
      rightAsSeq
        .iterator
        .zip(rightHadMatch.iterator)
        .collect { case (rightElem, false) => (None, Some(rightElem)) }
  }

  def leftAntiJoin[T, U](left: Iterator[T], right: Iterator[U], p: (T, U) => Boolean): Iterator[T] = {
    val rightAsSeq = right.toIndexedSeq
    left.filter(leftElem => !rightAsSeq.iterator.exists(p(leftElem, _)))
  }
}
