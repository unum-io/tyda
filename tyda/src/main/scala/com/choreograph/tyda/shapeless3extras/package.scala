package com.choreograph.tyda.shapeless3extras

import scala.deriving.Mirror
import scala.runtime.TupleMirror

import shapeless3.deriving.Const
import shapeless3.deriving.K0
import shapeless3.deriving.Labelling
import shapeless3.deriving.internals.ErasedProductInstances1
import shapeless3.deriving.internals.ErasedProductInstancesN

/** This file contains some extensions and utilities for working with shapeless
  * 3
  */
private[tyda] type Labelled[F[_]] = [T] =>> (String, F[T])
private[tyda] type Combine[E] = (E, E) => E

extension [T, F[_]](inst: K0.ProductInstances[F, T]) {

  /** Use the labelling to add the field name to each F[_] instance */
  private[tyda] def labelled(using labels: Labelling[T]): K0.ProductInstances[Labelled[F], T] = {
    val labelIt = labels.elemLabels.iterator
    inst.mapK[Labelled[F]]([t] => (labelIt.next(), _))
  }

  /** Map each instance to a constant type and collect it to a Seq. */
  private[tyda] def mapConst[C](f: [t] => F[t] => C): Seq[C] =
    inst.mapK[Const[C]](f).foldLeft0[Vector[C]](Vector.empty)([_] => _ :+ _)
}

extension [T <: Tuple, F[_]](inst: K0.ProductInstances[F, T]) {
  def toTuple: Tuple.Map[T, F] =
    // TYPE SAFETY: For tuples the type classes in K0.ProductInstances[F, T] is exactly Tuple.Map[T, F]
    Tuple.fromIArray(IArray.from(inst.mapConst[Any]([t] => v => v: Any))).asInstanceOf[Tuple.Map[T, F]]
}

extension [T: Mirror.ProductOf as m, F[_]](inst: K0.ProductInstances[F, T]) {
  def toTuple: Tuple.Map[m.MirroredElemTypes, F] =
    /* TYPE SAFETY: For product types the type classes in K0.ProductInstances[F, T] is exactly
     * Tuple.Map[m.MirroredElemTypes, F] */
    Tuple
      .fromIArray(IArray.from(inst.mapConst[Any]([t] => v => v: Any)))
      .asInstanceOf[Tuple.Map[m.MirroredElemTypes, F]]
}

extension [T, F[_]](inst: K0.CoproductInstances[F, T]) {

  /** Use the labelling to add the field name to each F[_] instance */
  private[tyda] def labelled(using labels: Labelling[T]): K0.CoproductInstances[Labelled[F], T] = {
    val labelIt = labels.elemLabels.iterator
    inst.mapK[Labelled[F]]([t] => (labelIt.next(), _))
  }

  /** Map each instance to a constant type and collect it to a Seq. */
  private[tyda] def mapConst[C](f: [t <: T] => F[t] => C): Seq[C] = (0 until inst.arity).map(i =>
    inst.inject[C](i)(f)
  )
}

/** Wrapper for product instances that implements comparison.
  *
  * This is intended to be used if the instances are stored in a case class
  * where equals is expected to work.
  */
private[tyda] final case class WrappedProductInstances[F[_], T](value: K0.ProductInstances[F, T]) {
  override def equals(that: Any): Boolean =
    that match {
      // TYPE SAFETY: This is probably not generally safe, it only being used inside Codec where we also check
      // a ClassTag probably helps a bit. But we should prbably figure out something better.
      case that: WrappedProductInstances[F @unchecked, T @unchecked] => isEqual(value, that.value)
      case _ => false
    }

  override def hashCode(): Int = value.mapConst([t] => _.hashCode()).sum
}

/** Wrapper for product instances that implements comparison.
  *
  * This is intended to be used if the instances are stored in a case class
  * where equals is expected to work.
  */
private[tyda] final case class WrappedCoproductInstances[F[_], T](value: K0.CoproductInstances[F, T]) {
  override def equals(that: Any): Boolean =
    that match {
      // TYPE SAFETY: This is probably not generally safe, it only being used inside Codec where we also check
      // a ClassTag probably helps a bit. But we should prbably figure out something better.
      case that: WrappedCoproductInstances[F @unchecked, T @unchecked] => isEqual(value, that.value)
      case _ => false
    }

  override def hashCode(): Int = value.mapConst([t] => _.hashCode()).sum
}

/** Compares two product instances for equality */
private def isEqual[T, F[_]](a: K0.ProductInstances[F, T], b: K0.ProductInstances[F, T]): Boolean = {
  def collect(inst: K0.ProductInstances[F, T]): Seq[Any] = inst.mapConst([t] => v => v: Any)
  collect(a) == collect(b)
}

/** Compares two coproduct instances for equality */
private def isEqual[T, F[_]](a: K0.CoproductInstances[F, T], b: K0.CoproductInstances[F, T]): Boolean = {
  def collect(inst: K0.CoproductInstances[F, T]): Seq[Any] = inst.mapConst([t] => v => v: Any)
  collect(a) == collect(b)
}

/** Create a K0.ProductInstances from a tuple containing the instances. */
private[tyda] def tupleInstances[T <: Tuple, F[_]](t: Tuple.Map[T, F]): K0.ProductInstances[F, T] = {
  // TYPE SAFETY: For tuples the mirror only depends on the arity and the element is just the tuple itself
  val mirror = new TupleMirror(t.size).asInstanceOf[Mirror.ProductOf[T] { type MirroredElemTypes = T }]
  productInstances(using mirror)(t)
}

/** Create a K0.ProductInstances from a case class containing the instances. */
private[tyda] def productInstances[T: Mirror.ProductOf as m, F[_]](
    values: Tuple.Map[m.MirroredElemTypes, F]
): K0.ProductInstances[F, T] =
  values.size match {
    case 1 => new ErasedProductInstances1(m, () => values.head)
    case _ => new ErasedProductInstancesN(m, () => values.toArray.map(v => v: Any))
  }
