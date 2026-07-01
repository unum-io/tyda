package com.choreograph.tyda

import scala.reflect.ClassTag
import scala.util.NotGiven

import org.scalatest.compatible.Assertion
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.*

import com.choreograph.tyda.compiletimeextras.assertCompileTimeError
import com.choreograph.tyda.shapeless3extras.mapConst

object CodecSpec {
  final case class Person(name: String, age: Int) derives Codec
  final case class User(person: Person) derives Codec

  enum AnimalEnum extends EnumStableHashCode derives Codec {
    case Dog(name: String)
    case Cat
  }

  enum AnimalEnum2 extends EnumStableHashCode derives Codec {
    case Dog(name: String)
    case Cat(age: Option[Int])
  }

  sealed trait AnimalSealed derives Codec
  object AnimalSealed {
    final case class Dog(name: String) extends AnimalSealed
    case object Cat extends AnimalSealed
  }

  sealed trait Hierarchy0
  sealed trait Hierarchy1 extends Hierarchy0

  object Hierarchy0 {
    case object Node0 extends Hierarchy0
    final case class Node1(id: Int) extends Hierarchy1
  }

  enum Color extends EnumStableHashCode derives Codec.EnumAsString {
    case Red, Green, Blue
  }

  final case class TransparentInt(eraseMePlease: Int) derives Codec.Transparent
  final case class TransparentString(value: String) derives Codec.Transparent

  enum EnumWithManyCase extends EnumStableHashCode derives Codec {
    case A
    case B(a: Int)
    case C(a: Int)
    case D
    case E(b: String)
    case F(a: Int, b: String, c: Double)
  }

}

class CodecSpec extends AnyFunSuite {
  // Some of the imports in this * is used at compiletime in assertCompiles/assertCompileTimeError
  /* because of this bug https://github.com/scala/scala3/issues/21805 in unused one needs to be careful if
   * changing it to explicit imports */
  import CodecSpec.*

  test("Primitive codecs should be available summon") {
    assert(summon[Codec[Int]] == Codec.Int)
    assert(summon[Codec[Long]] == Codec.Long)
    assert(summon[Codec[Double]] == Codec.Double)
    assert(summon[Codec[Float]] == Codec.Float)
    assert(summon[Codec[Boolean]] == Codec.Boolean)
    assert(summon[Codec[Byte]] == Codec.Byte)
    assert(summon[Codec[Short]] == Codec.Short)
    assert(summon[Codec[String]] == Codec.String)
  }

  test("Support collections") { assert(summon[Codec[Seq[Int]]] == Codec.Seq[Int](Codec.Int)) }

  test("not compare equal for different types") {
    assert(summon[Codec[Seq[Int]]] != summon[Codec[List[Int]]])
  }

  test("Product codec should be derived for case classes") {
    val codec = summon[Codec.Product[Person]]
    assert(codec.classTag == summon[ClassTag[Person]])
    val fields = codec.fields.mapConst[Field[?]]([t] => identity(_))
    assert(fields == List(Field("name", Codec.String), Field("age", Codec.Int)))
  }

  test("Product codec should handle nested case classes") {
    val codec = summon[Codec.Product[User]]
    assert(codec.classTag == summon[ClassTag[User]])
    val fields = codec.fields.mapConst[Field[?]]([t] => identity(_))
    assert(fields == List(Field("person", summon[Codec[Person]])))
  }

  test("support enum") {
    val codec = Codec[AnimalEnum] match {
      case sum: Codec.Sum[AnimalEnum, ?] => sum
      case _ => fail("Expected a sum type")
    }
    assert(codec.classTag == summon[ClassTag[AnimalEnum]])
    assert(
      codec.variants.mapConst[Variant[?]]([t] => identity(_)) == List(
        Variant.Product("dog", summon[Codec[AnimalEnum.Dog]]),
        Variant.Singleton("cat", AnimalEnum.Cat, summon[Codec.Product[AnimalEnum.Cat.type]])
      )
    )
  }

  test("support sealed trait") {
    val codec = Codec[AnimalSealed] match {
      case sum: Codec.Sum[AnimalSealed, ?] => sum
      case _ => fail("Expected a sum type")
    }
    assert(codec.classTag == summon[ClassTag[AnimalSealed]])
    assert(
      codec.variants.mapConst[Variant[?]]([t] => identity(_)) == List(
        Variant.Product("dog", summon[Codec[AnimalSealed.Dog]]),
        Variant.Singleton("cat", AnimalSealed.Cat, summon)
      )
    )
  }

  test("not allow currently unsupported nested sealed traits hierarchy") {
    assertCompileTimeError("summon[Codec[Hierarchy0]]", "trait Hierarchy1 is not a generic product")
  }

  test("not be available for Set") { summon[NotGiven[Codec[Set[Int]]]] }

  test("iterate Primitive Codec") { assert(Codec.iterate(Codec[Int]).toSeq == Seq(Codec[Int])) }

  test("iterate Product Codec") {
    Codec.iterate(Codec[Person]).toSeq must contain theSameElementsAs
      (Seq(Codec[Person], Codec[Int], Codec[String]))
  }

  test("iterate List Codec") {
    Codec.iterate(Codec[List[Person]]).toSeq must contain theSameElementsAs
      (Seq(Codec[List[Person]], Codec.Seq[Person](summon), Codec[Person], Codec[Int], Codec[String]))
  }

  test("iterate Map Codec") {
    Codec.iterate(Codec[Map[String, Person]]).toSeq must contain theSameElementsAs
      (Seq(Codec[Map[String, Person]], Codec[Person], Codec[Int], Codec[String], Codec[String]))
  }

  def testInjection[T: Arbitrary, R](inj: Injection[T, R]): Unit = {
    (0 to 100).foreach { _ =>
      val expected = Arbitrary[T]()
      val result = inj.invert(inj(expected))
      assert(result == expected, s"Failed round trip for value: $expected")
    }
  }

  def testSumAsInjection[T: {Arbitrary, Codec, TypeName}]: Unit = {
    test(s"test asInjection for ${TypeName.name[T]}") {
      Codec[T] match {
        case sum: Codec.Sum[T, ?] => testInjection(sum.inj)
        case sum: Codec.SumAsString[T] => testInjection(sum.inj)
        case _ => fail("Expected a sum type")
      }
    }
  }

  testSumAsInjection[AnimalEnum]
  testSumAsInjection[AnimalSealed]
  testSumAsInjection[Hierarchy1]
  testSumAsInjection[Color]
  testSumAsInjection[EnumWithManyCase]

  test("Transparent codec should encode single-field case class as inner type") {
    val codec = Codec[TransparentInt]
    codec match {
      case Codec.FromInjection(inj, innerCodec) =>
        assert(innerCodec == Codec.Int)
        assert(inj(TransparentInt(42)) == 42)
        val encoded = inj(TransparentInt(42))
        assert(inj.invert(encoded) == TransparentInt(42))
      case _ => fail("Expected a FromInjection codec")
    }
  }

  test("Transparent codec should roundtrip correctly") {
    val codec = Codec[TransparentInt] match {
      case inj: Codec.FromInjection[TransparentInt, ?] => inj
      case _ => fail("Expected a FromInjection codec")
    }
    val original = TransparentInt(123)
    assert(codec.inj.invert(codec.inj(original)) == original)
  }

  test("Transparent codec should work with String field") {
    val codec = Codec[TransparentString]
    codec match {
      case Codec.FromInjection(inj, innerCodec) =>
        assert(innerCodec == Codec.String)
        assert(inj(TransparentString("hello")) == "hello")
        val encoded = inj(TransparentString("hello"))
        assert(inj.invert(encoded) == TransparentString("hello"))
      case _ => fail("Expected a FromInjection codec")
    }
  }

  test("Transparent codec should work with named tuple") {
    Codec.Transparent.derived[(name: String)] match {
      case Codec.FromInjection(inj, innerCodec) =>
        assert(innerCodec == Codec.String)
        val value = (name = "hello")
        assert(inj(value) == "hello")
        val encoded = inj(value)
        assert(inj.invert(encoded) == value)
    }
  }

  test("Transparent codec should not compile for multi-field case class") {
    assertCompileTimeError(
      """case class Pair(a: Int, b: Int) derives Codec.Transparent""",
      "Codec.Transparent requires exactly one field"
    )
  }

  test("allow adding Option parameters to singletons") {
    def inj[T: Codec]: Injection[T, ?] = {
      Codec[T] match {
        case sum: Codec.Sum[T, ?] => sum.inj
        case _ => fail("Expected a sum type")
      }
    }
    val encoded = (discriminant = "cat", dog = None, cat = None)
    // TYPE SAFETY: encoded is following the Repr structure of AnimalEnum2
    assert(inj[AnimalEnum2].invert(encoded.asInstanceOf) == AnimalEnum2.Cat(None))
  }
}
