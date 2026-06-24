package com.choreograph.tyda.rewrite

import scala.NamedTuple.AnyNamedTuple
import scala.deriving.Mirror
import scala.reflect.ClassTag

import shapeless3.deriving.K0
import shapeless3.deriving.Labelling
import shapeless3.deriving.internals.ErasedProductInstances1
import shapeless3.deriving.internals.ErasedProductInstancesN

import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Field
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.shapeless3extras.mapConst

private def getFields(c: Codec[?]): Seq[Field[?]] =
  c match {
    case Codec.Product(_, fields, _) => fields.mapConst[Field[?]]([t] => identity(_))
    case s @ Codec.Sum(_) => s.reprFields
    case other => Seq(Field("value", other))
  }

// This can be seens as a codec for NamedTuple.Concat[NamedTuple.From[P], NamedTuple.From[T]]
private def combinedFlatCodec[P: Codec, T: Codec]: Codec[?] = {
  val fields = getFields(Codec[P]) ++ getFields(Codec[T])
  assert(fields.map(_.name).distinct.size == fields.size, "Field names in combined codec must be unique")
  val tag: ClassTag[AnyNamedTuple] = Codec.tupleClassTag(fields.size)
  // TYPE SAFETY: NamedTuples mirror is just a Tuple mirror with different refinements
  val mirror = new scala.runtime.TupleMirror(fields.size).asInstanceOf[Mirror.ProductOf[AnyNamedTuple]]
  val labelling = Labelling[AnyNamedTuple](s"Tuple${fields.size}", fields.map(_.name).toIndexedSeq)
  val instances: K0.ProductInstances[Codec, AnyNamedTuple] = fields.size match {
    case 1 => new ErasedProductInstances1(mirror, () => fields.head.codec)
    case _ => new ErasedProductInstancesN(mirror, () => fields.toArray.map(_.codec: Any))
  }
  Codec.product(using tag, mirror, labelling, instances)
}

def unflatten[P, T](partitionCodec: Codec[P], modelCodec: Codec[T]): CompiledExpr[?, (P, T)] = {
  val arg = ExprNode.Reference(codec = combinedFlatCodec(using partitionCodec, modelCodec))
  def fields[C](codec: Codec[C]): ExprNode[C] =
    codec match {
      case codec @ (Codec.Product(_, _, _) | Codec.Sum(_, _)) =>
        val fieldsExprs = getFields(codec).map(f => ExprNode.Select(arg, f.name))
        def product[I](prod: Codec.Product[I]) =
          ExprNode.MakeProduct(
            // TYPE SAFETY: Each element in fieldsExprs has type ExprNode[?]
            values = Tuple.fromArray(fieldsExprs.toArray).asInstanceOf[Tuple.Map[Tuple, ExprNode]],
            codec = prod
          )
        codec match {
          case prod @ Codec.Product(_, _, _) => product(prod)
          case sum @ Codec.Sum(_, _) => ExprNode.FromRepr(product(sum.reprCodec), sum)
        }
      case _ => ExprNode.Select(arg, "value")
    }
  CompiledExpr(arg, ExprNode.makeTuple[(P, T)]((fields(partitionCodec), fields(modelCodec))))
}
