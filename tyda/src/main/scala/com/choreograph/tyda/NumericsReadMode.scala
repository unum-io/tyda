package com.choreograph.tyda

import shapeless3.deriving.Complete

import com.choreograph.tyda.shapeless3extras.mapConst

enum NumericsReadMode {

  /** The read schema used is the same as the scala code */
  case Exact

  /** The read schema is widened as if the data was written by BigQuery and then
    * cast to the correct type.
    *
    * This means that [[Byte]], [[Short]] and [[Int]] are read as if they are
    * [[Long]] and all floating point types are read as if they are [[Double]].
    * And `Decimal` types are read as if they are `Decimal(38, 9)` or
    * `Decimal(76, 38)` depending on the scale.
    */
  case WidenBigQuery
}

object NumericsReadMode {

  /** Create a widened codec and a cast to the original type.
    *
    * Note: This is currently only tested in the BigQuery integration in
    * DatasetReadWriteSuiteBigQuerySql.
    */
  private[tyda] def widenBigQuery[A](codec: Codec[A]): ReadAdapter[A, ?] = {
    extension [F](expr: Expr[F]) {
      def castOrError[T: SimpleTypeName](using CanTryCast[F, T]): Expr[T] =
        expr.tryCast[T].expect(s"Read value was out of bounds for ${SimpleTypeName.name[T]}")
    }

    def buildCast[Wide: Codec, T: SimpleTypeName](using CanTryCast[Wide, T]) =
      Some(ReadAdapter(Codec[Wide], _.castOrError[T]))

    buildReadAdapter(codec)([t] =>
      _ match {
        case Codec.Byte => buildCast[Long, Byte]
        case Codec.Short => buildCast[Long, Short]
        case Codec.Int => buildCast[Long, Int]
        case Codec.Float => Some(ReadAdapter(Codec[Double], _.cast[Float]))
        // Decimal[38, 9] is the default Decimal type in BigQuery
        case Codec.Decimal(38, 9) => None
        case dec: Codec.Decimal[precision, scale] if dec.scale <= 9 && dec.precision <= 29 + dec.scale =>
          given Decimal.Valid[precision, scale] = dec.valid
          buildCast[Decimal[38, 9], Decimal[precision, scale]]
        case _: Codec.Decimal[?, ?] =>
          // Spark will also fail here, but we we relax Decimal.MaxPrecision to 76 we could easily make
          // the iterator implementation read these values.
          throw RuntimeException("Reading decimal with higher precision than 38 is currently unsupported")
        case _ => None
      }
    ).getOrElse(ReadAdapter(codec, identity))
  }

  private[tyda] final case class ReadAdapter[T, Read](codec: Codec[Read], cast: Expr[Read] => Expr[T]) {
    def map[U](f: Expr[T] => Expr[U]): ReadAdapter[U, Read] = ReadAdapter(codec, cast.andThen(f))

    def asFailable: FailableReadAdapter[T, Read] = FailableReadAdapter(codec, cast, None)
  }

  private[tyda] final case class FailableReadAdapter[T, Read](
      codec: Codec[Read],
      cast: Expr[Read] => Expr[T],
      check: Option[Expr[Read] => Expr[Boolean]]
  ) {
    def map[U](f: Expr[T] => Expr[U]): FailableReadAdapter[U, Read] =
      FailableReadAdapter(codec, cast.andThen(f), check)

    def compose[U](f: FailableReadAdapter[U, T]): FailableReadAdapter[U, Read] = {
      val combinedCheck = (check, f.check.map(_.compose(cast))) match {
        case (Some(c1), Some(c2)) => Some((r: Expr[Read]) => c1(r) && c2(r))
        case (c @ Some(_), None) => c
        case (None, c @ Some(_)) => c
        case (None, None) => None
      }
      FailableReadAdapter(codec, cast.andThen(f.cast), combinedCheck)
    }
  }

  private[tyda] def buildReadAdapter[T](
      codec: Codec[T]
  )(rule: [t] => Codec[t] => Option[ReadAdapter[t, ?]]): Option[ReadAdapter[T, ?]] = {
    val failable = buildFailableReadAdapter(codec)([t] => rule(_).map(_.asFailable))
    assert(
      failable.forall(_.check.isEmpty),
      "Check should be empty since the rule doesn't return any failable adapters"
    )
    failable.map(adapter => ReadAdapter(adapter.codec, adapter.cast))
  }

  private[tyda] def buildFailableProductReadAdapter[T](
      codec: Codec.Product[T]
  )(rule: [t] => Codec[t] => Option[FailableReadAdapter[t, ?]]): Option[FailableReadAdapter[T, ?]] = {
    val anyFieldNeedsCast = codec
      .fields
      .foldLeft0(false)([t] => (acc, f) => Complete(rule(f.codec).isDefined)(true)(acc))
    if !anyFieldNeedsCast then return None

    val fields = codec.fields.mapConst[Field[?]]([t] => identity(_))
    val adapted = fields.map(f => f -> rule(f.codec).getOrElse(FailableReadAdapter(f.codec, identity, None)))
    val adaptedFields = adapted.map { case (field, adapter) => Field(field.name, adapter.codec) }
    val readCodec = Codec.unsafeNamedTuple(adaptedFields)
    val maybeCheck = adapted.foldLeft(Option.empty[Expr[?] => Expr[Boolean]]) {
      case (acc, (_, FailableReadAdapter(_, _, None))) => acc
      case (acc, (field, FailableReadAdapter(_, _, Some(check)))) =>
        val fieldCheck = (expr: Expr[?]) => check(Expr.lift(ExprNode.Select(Expr.unlift(expr), field.name)))
        Some(acc.fold(fieldCheck)(existing => expr => existing(expr) && fieldCheck(expr)))
    }
    Some(FailableReadAdapter(
      readCodec,
      expr =>
        val node = Expr.unlift(expr)
        val elements = adapted.map { case (field, adapter) =>
          Expr.unlift(adapter.cast(Expr.lift(ExprNode.Select(node, field.name))))
        }
        Expr.lift(ExprNode.makeProductUnsafe(elements, codec))
      ,
      maybeCheck
    ))
  }

  private[tyda] def buildFailableReadAdapterChildren[T](
      codec: Codec[T]
  )(rule: [t] => Codec[t] => Option[FailableReadAdapter[t, ?]]): Option[FailableReadAdapter[T, ?]] =
    codec match {
      case _: Codec.Primitive[T] => None
      case Codec.Option(element) =>
        rule(element).map { case FailableReadAdapter(readCodec, cast, maybeCheck) =>
          FailableReadAdapter(Codec.Option(readCodec), _.map(cast), maybeCheck.map(check => _.forall(check)))
        }
      case Codec.Seq(element) => rule(element).map { case FailableReadAdapter(readCodec, cast, maybeCheck) =>
          FailableReadAdapter(Codec.Seq(readCodec), _.map(cast), maybeCheck.map(check => _.forall(check)))
        }
      case Codec.Map(key, value) =>
        def error(keyOrValue: String): Nothing =
          throw new RuntimeException(s"Adapting $keyOrValue is currently unsupported")
        if rule(key).isDefined then error("map key")
        else if rule(value).isDefined then error("map value")
        else None
      case codec @ Codec.Product(_, _, _) => buildFailableProductReadAdapter(codec)([t] => rule(_))
      case codec @ Codec.FromInjection(_, to) => buildFailableReadAdapterChildren(to)(rule).map(_.map(e =>
          Expr.lift(ExprNode.FromRepr(Expr.unlift(e), codec))
        ))
    }

  private[tyda] def buildFailableReadAdapter[T](
      codec: Codec[T]
  )(rule: [t] => Codec[t] => Option[FailableReadAdapter[t, ?]]): Option[FailableReadAdapter[T, ?]] = {
    def impl[T](codec: Codec[T]): Option[FailableReadAdapter[T, ?]] =
      rule(codec)
        .map(adapter => impl(adapter.codec).fold(adapter)(_.map(adapter.cast)))
        .orElse(buildFailableReadAdapterChildren(codec)([t] => impl(_)))
    impl(codec)
  }
}
