package com.choreograph.tyda.iterator

import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*

import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.iterator.ExprEvaluation.lambda2

/** Suite that compares that the NestedLoopJoin and HashJoin implementations
  * produce the same result for a variety of join conditions.
  */
class JoinImplementationSuite extends AnyFunSuite {
  def test(p: (Expr[Byte], Expr[Byte]) => Expr[Boolean]): Unit = {
    val lhs = Seq.fill(100)(Random.nextInt.toByte)
    val rhs = Seq.fill(100)(Random.nextInt.toByte)
    val lRef = ExprNode.Reference[Byte]()
    val rRef = ExprNode.Reference[Byte]()
    val cond = CompiledExpr2(lRef, rRef, p(Expr.lift(lRef), Expr.lift(rRef)).node)
    val (leftKey, rightKey, remainingCond) = JoinSelection.extractEquijoinPredicates(cond).toKeysAndPredicate
    test(s"inner join $cond") {
      val loopJoinResult = NestedLoopJoin.join(lhs.iterator, rhs.iterator, lambda2(cond)).toSeq
      val hashJoinResult = HashJoin.join(lhs.iterator, rhs.iterator, leftKey, rightKey, remainingCond).toSeq
      loopJoinResult should contain theSameElementsAs hashJoinResult
    }
    test(s"left outer join $cond") {
      val loopJoinResult = NestedLoopJoin.leftOuterJoin(lhs.iterator, rhs.iterator, lambda2(cond)).toSeq
      val hashJoinResult = HashJoin
        .leftOuterJoin(lhs.iterator, rhs.iterator, leftKey, rightKey, remainingCond)
        .toSeq
      loopJoinResult should contain theSameElementsAs hashJoinResult
    }
    test(s"full outer join $cond") {
      val loopJoinResult = NestedLoopJoin.fullOuterJoin(lhs.iterator, rhs.iterator, lambda2(cond)).toSeq
      val hashJoinResult = HashJoin
        .fullOuterJoin(lhs.iterator, rhs.iterator, leftKey, rightKey, remainingCond)
        .toSeq
      loopJoinResult should contain theSameElementsAs hashJoinResult
    }
    test(s"left anti join $cond") {
      val loopJoinResult = NestedLoopJoin.leftAntiJoin(lhs.iterator, rhs.iterator, lambda2(cond)).toSeq
      val hashJoinResult = HashJoin
        .leftAntiJoin(lhs.iterator, rhs.iterator, leftKey, rightKey, remainingCond)
        .toSeq
      loopJoinResult should contain theSameElementsAs hashJoinResult
    }
  }

  test(_ == _)
  test((a, _) => a == 0.toByte)
  test((a, b) => a == b || a == 0.toByte)
  test((a, b) => a == b && a == 0.toByte)
}
