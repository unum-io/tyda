package com.choreograph.tyda.spark

import scala.deriving.Mirror

import org.apache.spark.sql.Row
import org.apache.spark.sql.catalyst.expressions.GenericRow

import com.choreograph.tyda.Codec
import com.choreograph.tyda.asProduct

trait ProductToRowEncoder[P] extends SparkCodec[P, Row] {
  def encode(p: P): Row
  def decode(row: Row): P
}

object ProductToRowEncoder {
  def apply[T](
      p: Codec.Product[T],
      sparkCodecs: Array[Option[SparkCodec[Any, Any]]],
      rawNullToOption: Array[Boolean]
  ): ProductToRowEncoder[T] = {
    given mirror: Mirror.ProductOf[T] = p.mirror
    val className = p.classTag.runtimeClass.getSimpleName

    extension (a: Any) {
      private inline def orNull: Any =
        a match {
          case None => null
          case Some(value) => value
        }
    }

    new ProductToRowEncoder[T] {
      def encode(in: T): Row = {
        val prod = in.asProduct
        val values = Array.tabulate(sparkCodecs.size) { i =>
          val fieldValue = prod.productElement(i)
          val encoded = sparkCodecs(i) match {
            case None => fieldValue
            case Some(codec) => codec.encode(fieldValue)
          }
          if rawNullToOption(i) then encoded.orNull else encoded
        }
        new GenericRow(values)
      }
      def decode(out: Row): T = mirror.fromProduct(TransformingProduct(out, sparkCodecs, rawNullToOption))

      override def toString(): String = s"ProductToRowEncoder for $className"
    }
  }

  private final class TransformingProduct(
      product: Product,
      sparkCodecs: Array[Option[SparkCodec[Any, Any]]],
      rawNullToOption: Array[Boolean]
  ) extends Product {
    def canEqual(that: Any): Boolean = false
    def productArity: Int = product.productArity
    def productElement(n: Int): Any = {
      val fieldValue = product.productElement(n)
      val wrapped = if rawNullToOption(n) then Option(fieldValue) else fieldValue
      sparkCodecs(n) match {
        case None => wrapped
        case Some(codec) => codec.decode(wrapped)
      }
    }
  }
  private object TransformingProduct {
    def apply(
        row: Row,
        sparkCodecs: Array[Option[SparkCodec[Any, Any]]],
        rawNullToOption: Array[Boolean]
    ): TransformingProduct = new TransformingProduct(new RowProduct(row), sparkCodecs, rawNullToOption)
  }
}
