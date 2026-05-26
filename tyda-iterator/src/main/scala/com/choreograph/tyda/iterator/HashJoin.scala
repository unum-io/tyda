package com.choreograph.tyda.iterator

/* Implements basic hash loop join with SQL join semantics.
 *
 * Note: The implementation always build the hashmap on the right side. */
private object HashJoin {
  def join[T, U, K](
      left: Iterator[T],
      right: Iterator[U],
      leftKey: T => K,
      rightKey: U => K,
      p: (T, U) => Boolean
  ): Iterator[(T, U)] = {
    val rightHashMap = right.toIndexedSeq.groupBy(rightKey)
    left.flatMap { leftElem =>
      rightHashMap.get(leftKey(leftElem)).iterator.flatten.filter(p(leftElem, _)).map((leftElem, _))
    }
  }

  def leftOuterJoin[T, U, K](
      left: Iterator[T],
      right: Iterator[U],
      leftKey: T => K,
      rightKey: U => K,
      p: (T, U) => Boolean
  ): Iterator[(T, Option[U])] = {
    val rightHashMap = right.toIndexedSeq.groupBy(rightKey)
    left.flatMap { leftElem =>
      val matches = rightHashMap.get(leftKey(leftElem)).iterator.flatten.filter(p(leftElem, _))
      if (matches.hasNext) matches.map(r => (leftElem, Some(r))) else Iterator.single((leftElem, None))
    }
  }

  def fullOuterJoin[T, U, K](
      left: Iterator[T],
      right: Iterator[U],
      leftKey: T => K,
      rightKey: U => K,
      p: (T, U) => Boolean
  ): Iterator[(Option[T], Option[U])] = {
    val rightAsSeq = right.toIndexedSeq
    val rightHadMatch = Array.fill(rightAsSeq.size)(elem = false)
    val rightHashMap = rightAsSeq.zipWithIndex.groupBy((v, _) => rightKey(v))
    left.flatMap { leftElem =>
      val (matches, matchIndices) = rightHashMap
        .get(leftKey(leftElem))
        .getOrElse(Seq.empty)
        .filter((rightElem, _) => p(leftElem, rightElem))
        .unzip
      matchIndices.foreach(rightHadMatch(_) = true)
      if (matches.nonEmpty) matches.map(r => (Some(leftElem), Some(r)))
      else Iterator.single((Some(leftElem), None))
    } ++
      rightHashMap
        .valuesIterator
        .flatten
        .collect { case (rightElem, idx) if !rightHadMatch(idx) => (None, Some(rightElem)) }
  }

  def leftAntiJoin[T, U, K](
      left: Iterator[T],
      right: Iterator[U],
      leftKey: T => K,
      rightKey: U => K,
      p: (T, U) => Boolean
  ): Iterator[T] = {
    val rightHashMap = right.toIndexedSeq.groupBy(rightKey)
    left.filter(leftElem => !rightHashMap.get(leftKey(leftElem)).iterator.flatten.exists(p(leftElem, _)))
  }
}
