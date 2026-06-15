package com.choreograph.tyda.metadata

import scala.deriving.Mirror

import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal as TydaDecimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.pascalCaseToCamelCase
import com.choreograph.tyda.shapeless3extras.labelled
import com.choreograph.tyda.shapeless3extras.mapConst

trait ExtractedMetadata[T]:
  def fields: Seq[FieldMetadata]
  def description: Option[String]

object ExtractedMetadata:
  def apply[T: ExtractedMetadata as ef]: ExtractedMetadata[T] = ef

  private def empty[T]: ExtractedMetadata[T] =
    new ExtractedMetadata[T] {
      def description: Option[String] = None
      def fields = Seq.empty
    }

  given ExtractedMetadata[Byte] = empty
  given ExtractedMetadata[Short] = empty
  given ExtractedMetadata[Int] = empty
  given ExtractedMetadata[Long] = empty
  given ExtractedMetadata[Float] = empty
  given ExtractedMetadata[Double] = empty
  given ExtractedMetadata[Boolean] = empty
  given ExtractedMetadata[String] = empty
  given [P <: Int, S <: Int]: ExtractedMetadata[TydaDecimal[P, S]] = empty
  given ExtractedMetadata[Array[Byte]] = empty

  given ExtractedMetadata[Date] = empty
  given ExtractedMetadata[Timestamp] = empty
  given ExtractedMetadata[Duration] = empty

  given option[T: ExtractedMetadata as ef]: ExtractedMetadata[Option[T]] =
    new ExtractedMetadata[Option[T]] {
      def description: Option[String] = ef.description
      def fields = ef.fields
    }

  // we are not using maps now, but at some point this should probably also do something similar to seq
  given map[K, V]: ExtractedMetadata[Map[K, V]] = empty

  given seq[T: ExtractedMetadata as ef, C <: scala.Seq[T]]: ExtractedMetadata[C] =
    new ExtractedMetadata[C] {
      def description: Option[String] = ef.description
      def fields = ef.fields
    }

  private def product[T: Mirror.ProductOf: Labelling as labelling: TypeDocs as docs](using
      inst: K0.ProductInstances[ExtractedMetadata, T]
  ): ExtractedMetadata[T] =
    new ExtractedMetadata[T] {
      def description: Option[String] = docs.description
      def fields: Seq[FieldMetadata] =
        inst
          .labelled
          .mapConst[FieldMetadata] { [t] => (nameAndInst: (String, ExtractedMetadata[t])) =>
            val (name, fieldInst) = nameAndInst
            FieldMetadata(name, docs.params.get(name), fieldInst.description, fieldInst.fields)
          }
    }

  private def sum[T: Mirror.SumOf: Labelling as labelling: TypeDocs as docs](using
      inst: K0.CoproductInstances[ExtractedMetadata, T]
  ): ExtractedMetadata[T] =
    new ExtractedMetadata[T] {
      def description: Option[String] = docs.description
      def fields: Seq[FieldMetadata] =
        inst
          .labelled
          .mapConst[FieldMetadata] { [t] => (nameAndInst: (String, ExtractedMetadata[t])) =>
            val (rawName, fieldInst) = nameAndInst
            val camelCaseName = pascalCaseToCamelCase(rawName)
            FieldMetadata(camelCaseName, None, fieldInst.description, fieldInst.fields)
          }
    }

  inline given [T: Mirror.Of as m]: ExtractedMetadata[T] =
    inline m match {
      case given Mirror.SumOf[T] => sum
      case given Mirror.ProductOf[T] => product
    }
