package com.choreograph.tyda

import scala.deriving.Mirror

private[tyda] def unreachable(msg: String): Nothing = {
  val unreachableMessage = "Unreachable code. This is a bug in Tyda. Please report it. Details: "
  assert(assertion = false, unreachableMessage + msg)
}

/** A compile-time assertion that a condition encoded as a boolean type is true.
  */
private[tyda] inline def staticAssert[Condition <: true, msg <: String]: Unit = ()

/** A compile-time checked cast.
  *
  * Will fail at compile-time if the value is not safely castable to the target
  * type.
  */
private[tyda] inline def staticCast[T](inline value: Any): T = inline value match { case v: T => v }

private[tyda] def pascalCaseToCamelCase(name: String): String = {
  assert(name.nonEmpty)
  s"${name.charAt(0).toLower}${name.substring(1)}"
}

extension [T: Mirror.ProductOf as m](p: T) {

  /** Cast T to Product.
    *
    * The current understanding that the Mirror.ProductOf[T] guarantees that the
    * cast is safe.
    */
  inline def asProduct: Product =
    // TYPE SAFETY: The Mirror.ProductOf[T] guarantees that p <: Product.
    //              See https://github.com/scala/scala3/issues/22382#issuecomment-2613187822
    p.asInstanceOf[Product]

  /** Convert T into the corresponding tuple of field values.
    *
    * In generic code, it can be convenient to treat a product type as the
    * underlying tuple of values. Any type for which a product mirror is
    * available can be safely cast to Product. This goes also for named tuples
    * which do define a product mirror, but do not inherit from product.
    */
  private[tyda] inline def toMirroredElemTypes: m.MirroredElemTypes =
    // TYPE SAFETY: The Mirror.ProductOf[T] guarantees that p <: Product.
    //              See https://github.com/scala/scala3/issues/22382#issuecomment-2613187822
    val asProduct = p.asInstanceOf[Product]
    // TYPE SAFETY: The Mirror.ProductOf[T] guarantees that the fields of
    //              T correspond precisely to m.MirroredElemTypes
    Tuple.fromProduct(asProduct).asInstanceOf[m.MirroredElemTypes]
}
