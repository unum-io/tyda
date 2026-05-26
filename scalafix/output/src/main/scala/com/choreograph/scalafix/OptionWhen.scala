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
  val test1 = Option.when(!x.isEmpty)(x)
  val test2 = Option.when(!emptyString.isEmpty)(inner(emptyString))
  val test3 = Option.when(!someCondition)(computeValue())
  val test4 = Option.when(!(!valid))(result)

  // Test cases for if condition then Some(value) else None
  val test5 = Option.when(option.isDefined)(option.get)
  val test6 = Option.when(valid)(result)
  val test7 = Option.when(condition && otherCondition)(value)

  // Lambda expressions
  val mapper1 = (fv: String) => Option.when(!fv.isEmpty)(fv.toUpperCase)
  val mapper2 = (x: Int) => Option.when(x > 0)(x * 2)

  // Nested cases
  def processData(data: String) = Option.when(!data.isBlank)(data.trim)

  // More complex conditions
  val test8 = Option.when(!(emptyList.isEmpty || emptyList.headOption.isEmpty))(emptyList.head.toString)
  val test9 = Option.when(option.isDefined)(option.get.value)

  // Edge cases for parentheses handling
  val edgeCase1 = Option.when(!(!valid))(result)
  val edgeCase2 = Option.when(!(a > b))(result)
  val edgeCase3 = Option.when(list.nonEmpty)(list.head)
  val edgeCase4 = Option.when(!(func(x, y)))(z)

  // Semantic tests - these should be transformed (standard library None/Some)
  val semantic1 = Option.when(!condition)(value)
  val semantic2 = Option.when(condition)(result)

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
  val aliased = Option.when(!condition)(value)

  // Cases that should NOT be transformed
  val notTransformed1 = if condition then Some(x) else Some(x) // Both branches have Some
  val notTransformed2 = if condition then None else None // Both branches have None
  val notTransformed3 = if condition then result else value // No Option wrappers
  val notTransformed4 = Some(if condition then x else value) // Option wrapping the entire if
}
