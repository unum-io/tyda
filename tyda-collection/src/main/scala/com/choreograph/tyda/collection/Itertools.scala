package com.choreograph.tyda.collection

private object Itertools {
  extension [A: Equiv as equiv](it: Iterator[A]) {

    /** Remove duplicate consecutive of identical elements from the iterator.
      */
    def dedup: Iterator[A] =
      var prev: Option[A] = None
      it.filter { v =>
        val isDuplicate = prev.exists(equiv.equiv(_, v))
        prev = Some(v)
        !isDuplicate
      }
  }
}
