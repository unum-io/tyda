package com.choreograph.tyda.rewrite

import shapeless3.deriving.Complete

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Field
import com.choreograph.tyda.shapeless3extras.mapConst

private[tyda] final case class Cast[T, R](cast: Expr[T] => Expr[R]) {
  def apply(expr: Expr[T]): Expr[R] = cast(expr)
  def apply(expr: ExprNode[T]): ExprNode[R] = cast(Expr.lift(expr)).node
  def compose[A](g: Cast[A, T]): Cast[A, R] = Cast(cast.compose(g.cast))
  def resCodec(using Codec[T]): Codec[R] = cast(Expr.lift(ExprNode.Reference())).codec
}

private[tyda] object Cast {
  private def product[P](
      codec: Codec.Product[P],
      visitor: [t] => Codec[t] => Option[Cast[t, ?]]
  ): Option[Cast[P, ?]] = {
    val anyFieldNeedsCast = codec
      .fields
      .foldLeft0(false)([t] => (acc, f) => Complete(visitor(f.codec).isDefined)(true)(acc))
    if !anyFieldNeedsCast then return None

    val fieldsAndCasts = codec
      .fields
      .mapK[[X] =>> (Field[X], Cast[X, ?])]([t] => f => (f, visitor(f.codec).getOrElse(Cast(identity))))
    val convertedFields = fieldsAndCasts.mapConst[Field[?]] { [t] => fieldAndCast =>
      val (field, cast) = fieldAndCast
      val codec = cast(Expr.lift(ExprNode.Reference()(using field.codec))).codec
      Field(field.name, codec)
    }

    val convertedCodec = Codec.unsafeNamedTuple(convertedFields)
    Some(Cast(expr =>
      val node = Expr.unlift(expr)
      val elements = fieldsAndCasts.mapConst[ExprNode[?]] { [t] => fieldAndCast =>
        val (field, cast) = fieldAndCast
        Expr.unlift(cast(Expr.lift(ExprNode.Select(node, field.name))))
      }
      Expr.lift(ExprNode.makeProductUnsafe(elements, convertedCodec))
    ))
  }

  private def buildConverterChildren[T](
      codec: Codec[T],
      visitor: [t] => Codec[t] => Option[Cast[t, ?]]
  ): Option[Cast[T, ?]] =
    codec match {
      case _: Codec.Primitive[T] => None
      case Codec.Option(element: Codec[t]) =>
        visitor(element).map(cast => Cast((expr: Expr[Option[t]]) => expr.map(cast.cast)))
      case Codec.Seq(element: Codec[t]) =>
        visitor(element).map(cast => Cast((expr: Expr[Seq[t]]) => expr.map(cast.cast)))
      case Codec.Map(key, value) =>
        def error(keyOrValue: String): Nothing =
          throw new RuntimeException(s"Converting $keyOrValue is currently unsupported")
        if visitor(key).isDefined then error("map key")
        else if visitor(value).isDefined then error("map value")
        else None
      case codec @ Codec.Product(_, _, _) => product(codec, visitor)
      case codec @ Codec.FromInjection(_, to) => visitor(to).map(cast =>
          Cast((expr: Expr[T]) => cast(Expr.lift(ExprNode.ToRepr(Expr.unlift(expr), codec))))
        )
    }

  /** Build a converterd by walking he codec in a pre-order traversal. */
  def buildConverterDown[T](
      codec: Codec[T]
  )(rule: [t] => Codec[t] => Option[Cast[t, ?]]): Option[Cast[T, ?]] = {
    def visitor[T](codec: Codec[T]): Option[Cast[T, ?]] =
      rule(codec)
        .map(cast => visitor(cast.resCodec(using codec)).fold(cast)(_.compose(cast)))
        .orElse(buildConverterChildren(codec, [t] => visitor(_)))

    visitor(codec)
  }

  /** Build the converter by walking the codec in a post-order traversal. */
  def buildConverterUp[T](
      codec: Codec[T]
  )(rule: [t] => Codec[t] => Option[Cast[t, ?]]): Option[Cast[T, ?]] = {
    def visitor[T](codec: Codec[T]): Option[Cast[T, ?]] =
      buildConverterChildren(codec, [t] => visitor(_))
        .map(cast => rule(cast.resCodec(using (codec))).fold(cast)(_.compose(cast)))
        .orElse(rule(codec))
    visitor(codec)
  }
}
