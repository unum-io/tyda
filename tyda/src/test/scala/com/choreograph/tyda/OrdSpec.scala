package com.choreograph.tyda

import org.scalatest.funsuite.AnyFunSuite

object OrdSpec {
  final case class MyProduct(x: Int, y: Int, s: IndexedSeq[Int]) derives Ord

  enum MyEnum derives Ord {
    case Singleton
    case Product(a: Int, y: Long)
    case NestedProduct(p: MyProduct)
  }

  // This corresponds to the schema that Codec derives for MyEnum
  final case class MyEnumRepr(
      discriminant: String,
      product: Option[MyEnum.Product] = None,
      nestedProduct: Option[MyEnum.NestedProduct] = None
  ) derives Ord
}

class OrdSpec extends AnyFunSuite {
  import OrdSpec.{MyProduct, MyEnum, MyEnumRepr}

  def testBasic[T: {Arbitrary, Ord as ord, TypeName}]: Unit = {
    test(s"test Ord[${TypeName.name[T]}]") {
      (0 to 100).foreach { _ =>
        val x = Arbitrary[T]()
        val y = Arbitrary[T]()
        assert(ord.compare(x, x) == 0)
        assert(ord.compare(y, y) == 0)
        assert(ord.compare(x, y) == -ord.compare(y, x))
      }
    }
  }
  def testBasicAndAgainstOrdering[T: {Arbitrary, Ord as ord, Ordering as ordering, TypeName}]: Unit = {
    assert(
      !ord.isInstanceOf[Ord.FromOrdering[?]],
      s"Ord for ${TypeName.name[T]} should not be wrapping an existing ordering"
    )
    testBasic[T]
    test(s"test Ord[${TypeName.name[T]}] against Ordering") {
      (0 to 100).foreach { _ =>
        val x = Arbitrary[T]()
        val y = Arbitrary[T]()
        assert(ord.compare(x, y).sign == ordering.compare(x, y).sign)
      }
    }
  }

  test("explicit test for MyProduct") {
    val x = MyProduct(1, 999, IndexedSeq.empty)
    val y = MyProduct(1, 0, IndexedSeq.empty)
    val ord = summon[Ord[MyProduct]]
    assert(ord.compare(x, y) > 0)
  }

  testBasic[Int]
  testBasic[Float]
  testBasic[Double]
  testBasic[String]
  testBasic[MyProduct]
  testBasic[Option[Int]]
  testBasic[IndexedSeq[Int]]

  testBasicAndAgainstOrdering[(Int, Int)]

  {
    import Ordering.Implicits.seqOrdering
    testBasicAndAgainstOrdering[Seq[Int]]
  }

  test("use Ord instead Ordering for tuples") { assert(Ord[(Int, Int)].isInstanceOf[Ord.ProductOrd[?]]) }

  {
    // Sum ordering should be compatible with the Product ordering on corresponding repr class
    given reprOrdering: Ordering[MyEnum] =
      Ordering.by[MyEnum, MyEnumRepr] {
        case MyEnum.Singleton => MyEnumRepr(MyEnum.Singleton.toString)
        case p: MyEnum.Product => MyEnumRepr(p.productPrefix, Some(p))
        case n: MyEnum.NestedProduct => MyEnumRepr(n.productPrefix, nestedProduct = Some(n))
      }
    testBasicAndAgainstOrdering[MyEnum]
  }
}
