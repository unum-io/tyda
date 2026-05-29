package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError

object GroupableSpec {
  private final case class Person(name: String, age: Int)
  private final case class Address(street: String, city: String)
  private final case class User(person: Person, address: Address)

  private final case class NestedData(
      id: Int,
      name: String,
      optional: Option[String],
      sequence: Seq[Int],
      tuple: (String, Int),
      boolean: Boolean,
      numbers: (Byte, Short, Long, Float, Double)
  )

  private enum Status {
    case Active
    case Inactive(reason: String)
    case Pending(since: Long, priority: Int)
  }

  private final case class WithMap(data: Map[String, Int])
  private final case class WithNestedMap(user: Person, metadata: Map[String, String])
  private final case class WithOptionMap(optMap: Option[Map[Int, String]])
  private final case class WithSeqMap(maps: Seq[Map[String, Int]])
  private final case class WithTupleMap(data: (String, Map[Int, Boolean]))
  private enum EnumWithMap {
    case Singleton
    case WithoutMap(value: String)
    case CaseWithMap(data: Map[String, Int])
  }
}

class GroupableSpec extends AnyFunSuite {
  import GroupableSpec.*

  def checkExists[T: Groupable as g: TypeName]: Unit =
    test(s"${TypeName.name[T]} should be groupable") { assert(g != null) }

  checkExists[Boolean]
  checkExists[Byte]
  checkExists[Short]
  checkExists[Int]
  checkExists[Long]
  checkExists[Float]
  checkExists[Double]
  checkExists[String]
  checkExists[Timestamp]
  checkExists[Duration]
  checkExists[Date]

  checkExists[Option[Int]]
  checkExists[Option[String]]
  checkExists[Option[Boolean]]
  checkExists[Option[Person]]
  checkExists[Option[Option[Int]]]
  checkExists[Option[Option[String]]]

  checkExists[Seq[Int]]
  checkExists[Seq[String]]
  checkExists[Seq[Person]]
  checkExists[List[Int]]
  checkExists[Vector[String]]
  checkExists[Seq[Seq[Int]]]
  checkExists[Seq[Option[String]]]
  checkExists[Option[Seq[Int]]]

  checkExists[(Int, String)]
  checkExists[(Int, String, Boolean)]
  checkExists[(Person, Address)]
  checkExists[(Option[Int], Seq[String])]
  checkExists[((Int, String), (Boolean, Double))]
  checkExists[(Int, (String, (Boolean, Double)))]

  checkExists[Person]
  checkExists[Address]
  checkExists[User]
  checkExists[NestedData]

  checkExists[Status]
  checkExists[Status.Active.type]
  checkExists[Status.Inactive]
  checkExists[Status.Pending]

  checkExists[Seq[Option[(Person, (Address, Seq[String]))]]]

  inline def checkDoesNotExist(inline typeName: String, problemType: String): Unit =
    checkDoesNotExistImpl(typeName, s"Type $problemType is not groupable")

  inline def checkDoesNotExist(inline typeName: String): Unit = checkDoesNotExistImpl(typeName)

  inline def checkDoesNotExistImpl(inline typeName: String, message: String*): Unit =
    test(s"$typeName should NOT be groupable") {
      assertCompileTimeError("Groupable.derived[" + typeName + "]", "is not groupable", message*)
    }

  checkDoesNotExist("Map[String, Int]")
  checkDoesNotExist("Map[Int, String]")
  checkDoesNotExist("scala.collection.immutable.Map[String, Int]", "Map[String, Int]")

  checkDoesNotExist("WithMap", "Map[String, Int]")
  checkDoesNotExist("WithNestedMap", "Map[String, String]")
  checkDoesNotExist("WithOptionMap", "Map[Int, String]")
  checkDoesNotExist("WithSeqMap", "Map[String, Int]")
  checkDoesNotExist("WithTupleMap", "Map[Int, Boolean]")
  checkDoesNotExist("EnumWithMap", "Map[String, Int]")
  checkDoesNotExist("(Int, EnumWithMap)", "Map[String, Int]")
  checkDoesNotExist("Int *: String *: EnumWithMap *: EmptyTuple", "Map[String, Int]")

  checkDoesNotExist("Option[Map[String, Int]]", "Map[String, Int]")
  checkDoesNotExist("Seq[Map[String, Int]]", "Map[String, Int]")
  checkDoesNotExist("(String, Map[Int, String])", "Map[Int, String]")
  checkDoesNotExist("(Map[String, Int], String)", "Map[String, Int]")
}
