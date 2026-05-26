/* rule = OptionWhen */
package com.choreograph.scalafix

object OptionWhenTest {

  def inner(s: String): String = s.toUpperCase
  def computeValue(): String = "value"

  val x: String = "test"
  val emptyString: String = ""
  val someCondition: Boolean = true
  val valid: Boolean = true
  val result: String = "result"
  val condition: Boolean = true
  val otherCondition: Boolean = false
  val value: String = "value"
  val list: List[String] = List("a", "b")
  val emptyList: List[String] = List.empty
  val option: Option[Data] = Some(Data("test"))

  // Additional variables for edge case testing
  val a: Int = 1
  val b: Int = 2
  val z: String = "z"
  val y: Int = 42
  def func(x: String, y: Int): Boolean = true

  case class Data(value: String)

  // Test cases for if condition then None else Some(value)
  val test1 = if (x.isEmpty) None else Some(x)
  val test2 = if emptyString.isEmpty then None else Some(inner(emptyString))
  val test3 = if (someCondition) None else Some(computeValue())
  val test4 = if (!valid) None else Some(result)

  // Test cases for if condition then Some(value) else None
  val test5 = if (option.isDefined) Some(option.get) else None
  val test6 = if valid then Some(result) else None
  val test7 = if (condition && otherCondition) Some(value) else None

  // Lambda expressions
  val mapper1 = (fv: String) => if fv.isEmpty then None else Some(fv.toUpperCase)
  val mapper2 = (x: Int) => if x > 0 then Some(x * 2) else None

  // Nested cases
  def processData(data: String) = if (data.isBlank) None else Some(data.trim)

  // More complex conditions
  val test8 = if (emptyList.isEmpty || emptyList.headOption.isEmpty) None else Some(emptyList.head.toString)
  val test9 = if (option.isDefined) Some(option.get.value) else None

  // Edge cases for parentheses handling
  val edgeCase1 = if (!valid) None else Some(result)
  val edgeCase2 = if (a > b) None else Some(result)
  val edgeCase3 = if list.nonEmpty then Some(list.head) else None
  val edgeCase4 = if (func(x, y)) None else Some(z)

  // Semantic tests - these should be transformed (standard library None/Some)
  val semantic1 = if (condition) None else Some(value)
  val semantic2 = if condition then Some(result) else None

  // These should NOT be transformed (custom None/Some)
  object CustomTypes {
    val None = "not the real None"
    def Some(x: String) = x + "_custom"

    val shouldNotTransform1 = if (condition) None else Some(value)
    val shouldNotTransform2 = if condition then Some(value) else None
  }

  // This should be transformed again (back to standard library)
  import scala.{None as ScalaNone, Some as ScalaSome}
  // The line below is only to avoid unused warnings after rewrite
  val usageOfImports = ScalaSome("imported").toString ++ ScalaNone.toString
  val aliased = if (condition) ScalaNone else ScalaSome(value)

  // Cases that should NOT be transformed
  val notTransformed1 = if condition then Some(x) else Some(x) // Both branches have Some
  val notTransformed2 = if condition then None else None // Both branches have None
  val notTransformed3 = if condition then result else value // No Option wrappers
  val notTransformed4 = Some(if condition then x else value) // Option wrapping the entire if
}
