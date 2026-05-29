package com.choreograph.tyda.testsuites

import org.scalactic.Equality

import com.choreograph.tyda.Ord

object FloatingPointEquality {
  // Custom Equality that considers NaN == NaN and -0.0 == 0.0
  given float: Equality[Float] =
    new Equality[Float] {
      override def areEqual(a: Float, b: Any): Boolean =
        b match {
          case b: Float => Ord[Float].equiv(a, b)
          case _ => false
        }
    }
  given double: Equality[Double] =
    new Equality[Double] {
      override def areEqual(a: Double, b: Any): Boolean =
        b match {
          case b: Double => Ord[Double].equiv(a, b)
          case _ => false
        }
    }

  given seqFloat: Equality[Seq[Float]] =
    new Equality[Seq[Float]] {
      override def areEqual(a: Seq[Float], b: Any): Boolean =
        b match {
          case b: Seq[?] => a.length == b.length && a.zip(b).forall((x, y) => float.areEqual(x, y))
          case _ => false
        }
    }

  given seqDouble: Equality[Seq[Double]] =
    new Equality[Seq[Double]] {
      override def areEqual(a: Seq[Double], b: Any): Boolean =
        b match {
          case b: Seq[?] => a.length == b.length && a.zip(b).forall((x, y) => double.areEqual(x, y))
          case _ => false
        }
    }
}
