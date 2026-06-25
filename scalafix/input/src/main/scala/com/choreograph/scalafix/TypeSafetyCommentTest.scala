/* rule = TypeSafetyComment */
package com.choreograph.scalafix

class TypeSafetyCommentTest {
  val _ = 1.asInstanceOf[String] // assert: TypeSafetyComment.missingComment

  // TYPE SAFETY: Allow comments on expr
  "1".asInstanceOf[String]: Unit

  /** Long comment with more explanations
    *
    * TYPE SAFETY: Allow type safety comments inside doc style comments
    */
  val _ = "1".asInstanceOf[String]

  // TYPE SAFETY: Allow comment on cast inside other expression
  Some("1".asInstanceOf[String]): Unit

  // TYPE SAFETY: Ambiguous which cast is documented
  val _ = "1".asInstanceOf[String].asInstanceOf[Int] // assert: TypeSafetyComment.ambiguous

  // TYPE SAFETY: We should warn on comments that is not followed by any unsafe operation
  val _: String = "1" // assert: TypeSafetyComment.unused

  val _ = "1".asInstanceOf[String] // TYPE SAFETY: Support trailing comments

  val _ = 1.asInstanceOf[String] // assert: TypeSafetyComment.missingComment
  // TYPE SAFETY: This comment is for the next line not the preceding
  val _ = 1.asInstanceOf[String]

  // TYPE SAFETY: Allow multiple comments
  // Some unrelated information
  val _ = 1.asInstanceOf[String]

  def methodWithTypeSafetyComment(): String =
    // TYPE SAFETY: This is a leading comment on both the block and the expression but should not be considered ambiguous
    val x = 1.asInstanceOf[String]
    // TYPE SAFETY: This is also safe
    x.asInstanceOf[String]

  val xs: Any = List(1, 2, 3)

  val _ = xs match {
    case list: List[Int @unchecked] => list.sum // assert: TypeSafetyComment.missingComment
    case _ => 0
  }

  // TYPE SAFETY: xs is known to be List[Int]
  val _ = xs match {
    case list: List[Int @unchecked] => list.sum
    case _ => 0
  }

  val _ = xs match {
    case list: List[Int @unchecked] => list.sum // TYPE SAFETY: xs is List[Int]
    case _ => 0
  }
}
