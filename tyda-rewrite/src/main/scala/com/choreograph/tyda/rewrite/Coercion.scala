package com.choreograph.tyda.rewrite
import scala.compiletime.ops.int.-
import scala.compiletime.ops.int.>=

import com.choreograph.tyda.CanCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.Field
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.functions.some
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.unreachable

private[tyda] enum Coercion[+A] {
  case Exact
  case Widen(value: A)
  case Incompatible(errors: NonEmpty[Seq[Coercion.Error]]) extends Coercion[Nothing]

  def map[B](f: A => B): Coercion[B] =
    this match {
      case Exact => Exact
      case Widen(value) => Widen(f(value))
      case Incompatible(errors) => Incompatible(errors)
    }
}

private[tyda] object Coercion {
  enum PathSegment {
    case Field(name: String)
    case Element
    case Key
    case Value
  }

  final case class Error(path: Seq[PathSegment], tpe: ErrorType)
  enum ErrorType {
    case MissingRequired(expected: Codec[?])
    case Mismatch(from: Codec[?], to: Codec[?])
    case WideningMap
  }

  extension (errors: NonEmpty[Seq[Error]]) { def fmt: String = formatErrors(errors) }

  private def formatErrors(errors: NonEmpty[Seq[Error]]): String = {
    val formattedErrors = errors.map(e => s"  - ${formatError(e)}").mkString("\n")
    s"Could not coerce the value:\n${formattedErrors}"
  }

  private def formatError(error: Error): String = {
    val where = render(error.path)
    error.tpe match {
      case ErrorType.MissingRequired(expected) => s"required field $where (expected $expected) is missing"
      case ErrorType.Mismatch(from, to) => s"$where has type $from, which cannot be coerced to $to"
      case ErrorType.WideningMap => s"$where is a Map whose entries need widening, which is not supported yet"
    }
  }

  private def render(path: Seq[PathSegment]): String = {
    val sb = path.foldLeft(StringBuilder()) { (sb, seg) =>
      seg match {
        case PathSegment.Field(name) => sb.append('.').append(name)
        case PathSegment.Element => sb.append("[*]")
        case PathSegment.Key => sb.append("{key}")
        case PathSegment.Value => sb.append("{value}")
      }
    }
    if sb.isEmpty then "<root>" else sb.result().stripPrefix(".")
  }

  /** Try and create a coersion from one codec to another.
    *
    * Allowed coersions are promotion of numerical types and optional fields are
    * allowed to be mising.
    */
  def apply[From, To](from: ApproximatedCodec[From], to: Codec[To]): Coercion[Expr[From] => Expr[To]] =
    coerceWithValueFieldRemoval(from, to, Seq.empty)

  def exact[From, To](from: Codec[From], to: Codec[To]): Coercion[Expr[From] => Expr[To]] =
    apply(ApproximatedCodec(from, true), to)

  def apply[T, P, M](
      from: ApproximatedCodec[T],
      toPartition: Codec[P],
      toModel: Codec[M]
  ): Coercion[Expr[T] => Expr[(P, M)]] = {
    val makeTuple = unflatten(toPartition, toModel)
    apply(from, makeTuple.arg.codec).map(_.andThen(r =>
      Expr.lift(makeTuple.expr.replace(makeTuple.arg, r.node))
    ))
  }

  private def incompatible(path: Seq[PathSegment], tpe: ErrorType): Coercion[Nothing] =
    Incompatible(NonEmpty(Error(path, tpe)))

  private object SingleValueFieldStruct {
    def unapply(codec: Codec[?]): Option[Codec[?]] =
      codec match {
        case Codec.Product(_, fields, _) => fields.mapConst([t] => identity(_)) match {
            case Seq(Field("value", value)) => Some(value)
            case _ => None
          }
        case _ => None
      }
  }

  // We allow the from Codec to comes from an external schama and must therefore handle that we introdced
  // extra structs with a single field value for top level values of nested Options.
  private def coerceWithValueFieldRemoval[From, To](
      from: ApproximatedCodec[From],
      to: Codec[To],
      path: Seq[PathSegment],
      requireOptionValue: Boolean = false
  ): Coercion[Expr[From] => Expr[To]] =
    def isOption(c: Codec[?]) = c.isInstanceOf[Codec.Option[?]]
    (from.codec, to) match {
      case (SingleValueFieldStruct(inner), elem)
          if !requireOptionValue || (isOption(inner) && isOption(elem)) =>
        def select[T](e: Expr[From]): Expr[T] = Expr.lift(ExprNode.Select(Expr.unlift(e), "value"))
        coerce(from.copy(codec = inner), elem, path) match {
          case Exact => Widen(select)
          case Incompatible(_) => coerce(from, to, path)
          case Widen(coerceValue) => Widen(coerceValue.compose(select))
        }
      case _ => coerce(from, to, path)
    }

  private def coerce[From, To](
      approxFrom: ApproximatedCodec[From],
      to: Codec[To],
      path: Seq[PathSegment]
  ): Coercion[Expr[From] => Expr[To]] = {
    extension [T](c: Codec[T]) def asApprox = approxFrom.copy(codec = c)
    val from = approxFrom.codec
    to match {
      case `from` => return Exact
      case codec @ Codec.FromInjection(_, toInjected) => return coerce(approxFrom, toInjected, path).map(
          _.andThen(e => Expr.lift(ExprNode.FromRepr(Expr.unlift(e), codec)))
        )
      case Codec.Option(`from`) => return Widen((e: Expr[From]) => some(e))
      case Codec.Option(elem) if !from.isInstanceOf[Codec.Option[?]] =>
        return coerce(approxFrom, elem, path).map(_.andThen(some))
      case _ => ()
    }

    def mismatch = incompatible(path, ErrorType.Mismatch(from, to))

    from match {
      case Codec.Boolean | Codec.TimestampMicros | Codec.Date | Codec.DurationMicros | Codec.String | Codec
            .Bytes | Codec.Long | Codec.Double => mismatch
      case Codec.Byte => to match {
          case Codec.Short => Widen((e: Expr[Byte]) => e.cast[Short])
          case Codec.Int => Widen((e: Expr[Byte]) => e.cast[Int])
          case Codec.Long => Widen((e: Expr[Byte]) => e.cast[Long])
          case _ => mismatch
        }
      case Codec.Short => to match {
          case Codec.Int => Widen((e: Expr[Short]) => e.cast[Int])
          case Codec.Long => Widen((e: Expr[Short]) => e.cast[Long])
          case _ => mismatch
        }
      case Codec.Int => to match {
          case Codec.Long => Widen((e: Expr[Int]) => e.cast[Long])
          case _ => mismatch
        }
      case Codec.Float => to match {
          case Codec.Double => Widen((e: Expr[Float]) => e.cast[Double])
          case _ => mismatch
        }
      case d1: Codec.Decimal[p1, s1] => to match {
          case d2: Codec.Decimal[p2, s2]
              if d2.scale >= d1.scale && d2.precision - d2.scale >= d1.precision - d1.scale =>
            // TYPE SAFETY: Checked in the if guard above
            given scaleIncrease: (s2 >= s1 =:= true) = summon[true =:= true].asInstanceOf
            // TYPE SAFETY: Checked in the if guard above
            given maxIncrease: (p2 - s2 >= p1 - s1 =:= true) = summon[true =:= true].asInstanceOf
            given Decimal.Valid[p2, s2] = d2.valid
            Widen((e: Expr[Decimal[p1, s1]]) => e.cast[Decimal[p2, s2]])
          case _ => mismatch
        }

      case Codec.Option(elemFrom: Codec[a]) => to match {
          case Codec.Option(elemTo) =>
            coerceWithValueFieldRemoval(elemFrom.asApprox, elemTo, path, requireOptionValue = true).map(
              fixElem => (r: Expr[Option[a]]) => r.map(fixElem)
            )
          case _ => mismatch
        }
      case Codec.Seq(elemFrom: Codec[a]) => to match {
          case Codec.Seq(elemTo) => coerce(elemFrom.asApprox, elemTo, path :+ PathSegment.Element).map(
              fixElem => (r: Expr[Seq[a]]) => r.map(fixElem)
            )
          case _ => mismatch
        }
      case Codec.Map(keyFrom, valueFrom) => to match {
          case Codec.Map(keyTo, valueTo) => (
              coerce(keyFrom.asApprox, keyTo, path :+ PathSegment.Key),
              coerce(valueFrom.asApprox, valueTo, path :+ PathSegment.Value)
            ) match {
              case (Exact, Exact) => Exact
              // TODO: Support widening here after we support map on Maps
              case (Widen(_), _) | (_, Widen(_)) => incompatible(path, ErrorType.WideningMap)
              case (Incompatible(e1), Incompatible(e2)) => Incompatible(e1 ++ e2)
              case (e @ Incompatible(_), _) => e
              case (_, e @ Incompatible(_)) => e
            }
          case _ => mismatch
        }
      case Codec.Product(_, fromFields, _) =>
        val fromFieldsSeq = fromFields.mapConst([t] => identity(_))
        to match {
          case to @ Codec.Product(_, toFields, _) =>
            val fromNameToCodec = fromFieldsSeq.map(f => f.name -> f.codec).toMap
            val fieldAndCoercions = toFields.mapConst([t] =>
              f =>
                (
                  f,
                  fromNameToCodec
                    .get(f.name)
                    .map(_.asApprox)
                    .map(coerce(_, f.codec, path :+ PathSegment.Field(f.name)))
                )
            )

            val isExact = approxFrom.isExact && fieldAndCoercions.corresponds(fromFieldsSeq) {
              case ((toField, maybeCoersion), fromField) => fromField.name == toField.name &&
                maybeCoersion.contains(Exact)
            }
            if isExact then return Exact
            val allErrors = fieldAndCoercions
              .collect {
                case (Field(_, Codec.Option(_)), None) => Seq.empty
                case (Field(name, codec), None) =>
                  Seq(Error(path :+ PathSegment.Field(name), ErrorType.MissingRequired(codec)))
                case (_, Some(Incompatible(errors))) => errors.toSeq
              }
              .flatten
            NonEmpty.from(allErrors) match {
              case Some(errors) => return Incompatible(errors)
              case None => ()
            }

            Widen((expr: Expr[From]) =>
              val node = Expr.unlift(expr)
              val elements = fieldAndCoercions.map {
                case (field, Some(Coercion.Exact)) => ExprNode.Select(node, field.name)
                case (field, Some(Coercion.Widen(widen))) =>
                  // TYPE SAFETY: The compiler seems to infer `Nothing => Expr[?]` for widen, but we know it
                  // actually a Expr input
                  val widenFixed = widen.asInstanceOf[Expr[?] => Expr[?]]
                  widenFixed(Expr.lift(ExprNode.Select(node, field.name))).node
                case (Field(_, Codec.Option(nonNullCodec)), None) => ExprNode.none(using nonNullCodec)
                case (_, _) => unreachable("earlier guards handle Incompatible and mising required fields")
              }
              Expr.lift(ExprNode.makeProductUnsafe(elements, to))
            )
          case _ => mismatch
        }
      case codec @ Codec.FromInjection(_, fromInjected) => coerce(fromInjected.asApprox, to, path).map(
          _.compose(e => Expr.lift(ExprNode.ToRepr(Expr.unlift(e), codec)))
        )
    }
  }
}
