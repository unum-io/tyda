package com.choreograph.tyda.metadata

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.EnumStableHashCode

object ExtractedMetadataSpec {

  /** person scaladoc
    *
    * @param name
    *   person name description
    * @param age
    *   person age description
    */
  final case class Person(name: String, age: Int)

  /** parent scaladoc
    *
    * @param name
    *   parent name description
    * @param age
    *   parent age description
    * @param child
    *   child description
    */
  final case class Parent(name: String, age: Int, child: Person)

  /** enum scaladoc
    */
  enum Enum extends EnumStableHashCode {

    /** singleton scaladoc
      */
    case Singleton

    /** product scaladoc
      *
      * @param a
      *   a description
      * @param b
      *   b description
      */
    case Product(a: Int, b: String)
  }

  /** containers scaladoc
    *
    * @param arr
    *   arr description
    * @param opt
    *   opt description
    * @param op
    *   op description
    */
  final case class Containers(arr: List[Person], opt: Option[Seq[Person]], op: Option[OpaquePerson])

  opaque type OpaquePerson = Person

  object OpaquePerson {
    given extracted: ExtractedMetadata[OpaquePerson] = ExtractedMetadata[Person]
  }

  /** sealed trait scaladoc
    */
  sealed trait Sealed

  /** case class scaladoc
    * @param field1
    *   int
    * @param field2
    *   string
    */
  final case class CaseClass(field1: Int, field2: String) extends Sealed

  /** singleton scaladoc
    */
  case object SingletonObject extends Sealed
}

class ExtractedMetadataSpec extends AnyFunSuite {
  import ExtractedMetadataSpec.*

  def checkPersonFields(fields: Seq[FieldMetadata]): Unit = {
    val fieldsMap = fields.map(field => field.name -> field.fieldDescription).toMap

    assert(fieldsMap.size == 2)
    assert(fieldsMap.get("name").flatten.contains("person name description"))
    assert(fieldsMap.get("age").flatten.contains("person age description"))
    ()
  }

  def checkPerson(person: FieldMetadata): Unit = {
    assert(person.typeDescription.contains("person scaladoc"))
    checkPersonFields(person.fields)
  }

  test("extract metadata for primitive fields") {
    val metadata = summon[ExtractedMetadata[Person]]
    assert(metadata.description.contains("person scaladoc"))
    checkPersonFields(metadata.fields)
  }

  test("extract metadata for nested classes") {
    val metadata = summon[ExtractedMetadata[Parent]]
    val fields = metadata.fields.map(field => field.name -> field).toMap
    assert(metadata.description.contains("parent scaladoc"))
    assert(fields.size == 3)
    assert(fields.get("name").flatMap(_.fieldDescription).contains("parent name description"))
    assert(fields.get("age").flatMap(_.fieldDescription).contains("parent age description"))
    assert(fields.get("child").flatMap(_.fieldDescription).contains("child description"))
    fields.get("child").foreach(checkPerson)
  }

  test("extract metadata for enum") {
    val metadata = summon[ExtractedMetadata[Enum]]
    val fields = metadata.fields.map(field => field.name -> field).toMap
    assert(metadata.description.contains("enum scaladoc"))
    assert(fields.size == 2)
    assert(fields.get("singleton").flatMap(_.typeDescription).contains("singleton scaladoc"))
    assert(fields.get("product").flatMap(_.typeDescription).contains("product scaladoc"))
    val productFields = fields
      .get("product")
      .map(_.fields)
      .getOrElse(Seq.empty)
      .map(field => field.name -> field.fieldDescription)
      .toMap
    assert(productFields.size == 2)
    assert(productFields.get("a").flatten.contains("a description"))
    assert(productFields.get("b").flatten.contains("b description"))
  }

  test("extract metadata for collections") {
    val metadata = summon[ExtractedMetadata[Containers]]
    val fields = metadata.fields.map(field => field.name -> field).toMap
    assert(metadata.description.contains("containers scaladoc"))
    assert(fields.size == 3)
    assert(fields.get("arr").flatMap(_.fieldDescription).contains("arr description"))
    assert(fields.get("opt").flatMap(_.fieldDescription).contains("opt description"))
    assert(fields.get("op").flatMap(_.fieldDescription).contains("op description"))
    fields.get("arr").foreach(checkPerson)
    fields.get("opt").foreach(checkPerson)
    fields.get("op").foreach(checkPerson)
  }

  test("extract metadata for sealed trait") {
    val metadata = summon[ExtractedMetadata[Sealed]]
    val fields = metadata.fields.map(field => field.name -> field).toMap
    assert(metadata.description.contains("sealed trait scaladoc"))
    assert(fields.size == 2)
    assert(fields.get("singletonObject").flatMap(_.typeDescription).contains("singleton scaladoc"))
    assert(fields.get("caseClass").flatMap(_.typeDescription).contains("case class scaladoc"))
    val productFields = fields
      .get("caseClass")
      .map(_.fields)
      .getOrElse(Seq.empty)
      .map(field => field.name -> field.fieldDescription)
      .toMap
    assert(productFields.size == 2)
    assert(productFields.get("field1").flatten.contains("int"))
    assert(productFields.get("field2").flatten.contains("string"))
  }
}
