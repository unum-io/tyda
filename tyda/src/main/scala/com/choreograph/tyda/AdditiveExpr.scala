package com.choreograph.tyda

sealed trait AdditiveExpr[T] {
  def plus(lhs: T, rhs: T): T
}

object AdditiveExpr {

  def apply[T](using expr: AdditiveExpr[T]): AdditiveExpr[T] = expr

  given AdditiveExpr[Int] with {
    def plus(lhs: Int, rhs: Int): Int = Math.addExact(lhs, rhs)
  }

  given AdditiveExpr[Long] with {
    def plus(lhs: Long, rhs: Long): Long = Math.addExact(lhs, rhs)
  }

  given AdditiveExpr[Double] with {
    def plus(lhs: Double, rhs: Double): Double = lhs + rhs
  }

  given AdditiveExpr[Float] with {
    def plus(lhs: Float, rhs: Float): Float = lhs + rhs
  }
}
