package com.choreograph.tyda

/** A type class for type-safe bidirectional conversion between two types.
  *
  * The requirements for this trait are that it should be reversible i.e
  * `invert(apply(x)) == x` and the `To` representation should be consistent i.e
  * `x == y` then `apply(x) == apply(y)`.
  *
  * So for example the following is not a valid implementation for a Set:
  * ```
  * new Injection[Set[T], List[T]] {
  *   def apply(a: Set[T]): List[T] = a.toList
  *   def invert(i: List[T]): Set[T] = a.toSet
  * }
  * ```
  * since the iteration order of the Set is undefined. Which means that it will
  * not fulfill the consistency requirement. A valid implementation would be:
  * ```
  * new Injection[Set[T], List[T]] {
  *   def apply(a: Set[T]): List[T] = a.toList.sorted
  *   def invert(i: List[T]): Set[T] = a.toSet
  * }
  * ```
  */
private[tyda] trait Injection[From, To] extends Serializable {
  def apply(from: From): To
  def invert(to: To): From
}
