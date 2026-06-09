package com.choreograph.tyda.iterator

import java.util.regex.Pattern

import com.github.plokhotnyuk.jsoniter_scala.core.JsonReaderException
import com.github.plokhotnyuk.jsoniter_scala.core.ReaderConfig
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString

import com.choreograph.tyda.Binary
import com.choreograph.tyda.CanCast
import com.choreograph.tyda.CanTryCast
import com.choreograph.tyda.Codec
import com.choreograph.tyda.CompiledExpr
import com.choreograph.tyda.CompiledExpr2
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Errors
import com.choreograph.tyda.Expr
import com.choreograph.tyda.ExprNode
import com.choreograph.tyda.NonEmpty
import com.choreograph.tyda.NumericLimits
import com.choreograph.tyda.PrimitiveAggregateEvaluation
import com.choreograph.tyda.PrimitiveAggregateEvaluation.comparableToOrd
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.json.CodecToJsoniter
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.tupleInstances
import com.choreograph.tyda.unreachable

object ExprEvaluation {

  /** Create a lambda that has the same behavior as given Expr function.
    *
    * The resulting function is not optimized for performance and intended to
    * only be used for testing.
    */
  def lambda[From: Codec, To](f: Expr[From] => Expr[To]): From => To = lambda(CompiledExpr(f))

  /** Create a lambda that has the same behavior as given Expr function with 2
    * arguments.
    *
    * For more information see [[lambda]].
    */
  def lambda2[T1: Codec, T2: Codec, R](f: (Expr[T1], Expr[T2]) => Expr[R]): (T1, T2) => R =
    lambda2(CompiledExpr2(f))

  /** Create a lambda that can be used to evaluate a Expr[To] on actual values
    * of From.
    */
  private[tyda] def lambda[From, To](compiled: CompiledExpr[From, To]): From => To =
    lambdaN(NonEmpty[Seq](compiled.arg), compiled.expr)

  private[tyda] def lambda2[T1, T2, R](compiled: CompiledExpr2[T1, T2, R]): (T1, T2) => R =
    (t1, t2) => lambdaN[(T1, T2), R](NonEmpty[Seq](compiled.arg1, compiled.arg2), compiled.expr)((t1, t2))

  private def trimSpaces(str: String): String = {
    val first = str.indexWhere(_ != ' ')
    if first < 0 then ""
    else {
      val last = str.lastIndexWhere(_ != ' ')
      str.substring(first, last + 1)
    }
  }

  private def lambdaN[From, To](
      args: NonEmpty[Seq[ExprNode.Reference[?]]],
      expr: ExprNode[To]
  ): From => To = {
    def binaryOp[To, R](lhs: ExprNode[To], rhs: ExprNode[To], op: (To, To) => R): From => R = {
      val lhsEval = impl(lhs)
      val rhsEval = impl(rhs)
      from => op(lhsEval(from), rhsEval(from))
    }

    /** Same as binaryOp but with by-name parameters that lazily evaluates the
      * LHS and RHS
      */
    def binaryOpLazy[To, R](lhs: ExprNode[To], rhs: ExprNode[To], op: (=> To, => To) => R): From => R = {
      val lhsEval = impl(lhs)
      val rhsEval = impl(rhs)
      from => op(lhsEval(from), rhsEval(from))
    }

    def impl[To](expr: ExprNode[To]): From => To =
      expr match {
        case ref @ ExprNode.Reference(_, _) if args.size == 1 =>
          if args.head != ref then Errors.failUnexpectedReference(ref, args)
          // TYPE SAFETY: For ExprNode.Reference From and To are the same type
          identity.asInstanceOf[From => To]
        case ref @ ExprNode.Reference(_, _) =>
          val index = args.indexOf(ref)
          if index < 0 then Errors.failUnexpectedReference(ref, args)
          /* TYPE SAFETY: When there are multiple arguments, we pass them as a tuple and each element of the
           * tuple is required to be of the expected ExprNode type. */
          ((from: Product) => from.productElement(index)).asInstanceOf[From => To]
        case ExprNode.Select(parent, name) =>
          val parentEval = impl(parent)
          val index = parent.codec match {
            case Codec.Product(_, fields, _) =>
              val fieldNames = fields.mapConst[String]([t] => _.name)
              fieldNames
                .zipWithIndex
                .collectFirst { case (`name`, index) => index }
                .getOrElse(unreachable(s"Field ${name} not found in Product codec fields: ${fieldNames}"))
            case _ => unreachable(s"Select only supported on Product, got ${parent.codec}")
          }
          parentEval.andThen { p =>
            // TYPE SAFETY: Select should only be created for product types.
            val field = p.asInstanceOf[Product].productElement(index)
            // TYPE SAFETY: The field being of type To, should be upheld by the construction of the Select.
            field.asInstanceOf[To]
          }
        case ExprNode.MakeProduct(values, codec) =>
          val valuesEval: IArray[From => Any] = IArray.from(tupleInstances(values).mapConst([t] => impl(_)))
          from => codec.fromProduct(Tuple.fromIArray(valuesEval.map(_(from))))
        case ExprNode.Range(start, end) =>
          val startEval = impl(start)
          val endEval = impl(end)
          from => startEval(from) until endEval(from)
        case ExprNode.MakeSeq(values, _) =>
          val valueLambdas = values.map(impl(_))
          from => valueLambdas.map(_(from))
        case ExprNode.ConcatSeq(lhs, rhs) =>
          val lhsEval = impl(lhs)
          val rhsEval = impl(rhs)
          from => lhsEval(from) ++ rhsEval(from)
        case ExprNode.MapSeq(seq, f) =>
          val seqEval = impl(seq)
          val fEval = lambdaN(args :+ f.arg, f.expr)
          // TYPE SAFETY: We know that when there are multiple arguments they will be wrapped in a Tuple.
          if args.size == 1 then from => seqEval(from).map(v => fEval((from, v)))
          else from => seqEval(from).map(v => fEval(from.asInstanceOf[Tuple] :* v))
        case ExprNode.FilterSeq(seq, predicate) =>
          val seqEval = impl(seq)
          val predEval = lambdaN(args :+ predicate.arg, predicate.expr)
          // TYPE SAFETY: We know that when there are multiple arguments they will be wrapped in a Tuple.
          if args.size == 1 then from => seqEval(from).filter(v => predEval((from, v)))
          else from => seqEval(from).filter(v => predEval(from.asInstanceOf[Tuple] :* v))
        case ExprNode.AggregateSeq(seq, default, primitive) =>
          val seqEval = impl(seq)
          val defaultEval = impl(default)
          val aggregator = PrimitiveAggregateEvaluation.aggregator(primitive)
          from => {
            val inputSeq = seqEval(from)
            if inputSeq.isEmpty then defaultEval(from)
            else aggregator.finish(inputSeq.foldLeft(aggregator.zero)(aggregator.reduce))
          }
        case ExprNode.Literal(value, _) => _ => value
        case ExprNode.Not(cond) => impl(cond).andThen(!_)
        case ExprNode.Or(lhs, rhs) => binaryOpLazy(lhs, rhs, _ || _)
        case ExprNode.And(lhs, rhs) => binaryOpLazy(lhs, rhs, _ && _)
        case ExprNode.Equals(lhs, rhs) => binaryOp(lhs, rhs, _ == _)
        case ExprNode.LessThan(comparable, lhs, rhs) => binaryOp(lhs, rhs, comparableToOrd(comparable).lt)
        case ExprNode.LessThanOrEqual(comparable, lhs, rhs) =>
          binaryOp(lhs, rhs, comparableToOrd(comparable).lteq)
        case ExprNode.UpcastToIterable(expr) => impl(expr)
        case ExprNode.OptionToIterable(expr) => impl(expr)
            .andThen(Option.option2Iterable)
            .andThen(_.toSeq) // cast to seq to provide comparable type for equality checks in tests
        case ExprNode.Udf(e, f, _) => impl(e).andThen(f)
        case ExprNode.MakeSome(expr) =>
          val exprEval = impl(expr)
          from => Some(exprEval(from))
        case ExprNode.Coalesce(operands) =>
          val operandLambdas = operands.map(impl(_))
          from => operandLambdas.iterator.map(_(from)).flatten.nextOption()
        case ExprNode.KnownNotNull(expr) => impl(expr).andThen(_.getOrElse(unreachable(
            s"$expr resulted in None but was marked as KnownNotNull"
          )))
        case ExprNode.RaiseError(message, _) =>
          val messageEval = impl(message)
          from => throw new RuntimeException(messageEval(from))
        case ExprNode.ScalarSubquery(ds) =>
          val value = DatasetOnIterator(ds)
            .nextOption()
            .getOrElse(unreachable(s"Scalar subquery ${ds} returned no rows"))
          _ => value
        case ExprNode.ExistsSubquery(ds) =>
          val subquery = DatasetOnIterator(ds)
          _ => subquery.hasNext
        case ExprNode.StartsWith(string, prefix) =>
          val stringEval = impl(string)
          val prefixEval = impl(prefix)
          from => stringEval(from).startsWith(prefixEval(from))
        case ExprNode.Trim(string) => impl(string).andThen(trimSpaces)
        case ExprNode.EndsWith(string, suffix) =>
          val stringEval = impl(string)
          val suffixEval = impl(suffix)
          from => stringEval(from).endsWith(suffixEval(from))
        case ExprNode.ConcatString(strings) =>
          val evals = strings.map(impl(_))
          from => evals.map(_(from)).mkString
        case ExprNode.Split(string, delimiter) =>
          binaryOp(string, delimiter, (str, del) => str.split(Pattern.quote(del), -1).toSeq)
        case ExprNode.ToJson(inner) =>
          val innerEval = impl(inner)
          val jsonCodec = CodecToJsoniter.create(using inner.codec)
          from => writeToString(innerEval(from))(using jsonCodec)
        case ExprNode.FromJson(inner, codec) =>
          val innerEval = impl(inner)
          val jsonCodec = CodecToJsoniter.create(using codec)
          val config = ReaderConfig.withCheckForEndOfInput(false)
          from =>
            val encoded = innerEval(from)
            try Some(readFromString(encoded, config)(using jsonCodec))
            catch { case _: JsonReaderException => None }
        case ExprNode.SizeSeq(operand) => impl(operand).andThen(_.size)
        case ExprNode.ElementSeq(array, index) =>
          val arrayEval = impl(array)
          val indexEval = impl(index)
          from => arrayEval(from)(indexEval(from))
        case ExprNode.Aggregate(_, _) =>
          unreachable("ExprNode.Aggregate should not not be contructable using public APIs on Epxr")
        case ExprNode.Cases(whenThenExpr, whenThenExprs, elseExpr) =>
          val branches = (whenThenExpr +: whenThenExprs).map(branch =>
            (impl(branch.whenExpr), impl(branch.thenExpr))
          )
          val elseEval = impl(elseExpr)
          from =>
            branches
              .collectFirst { case (whenEval, thenEval) if whenEval(from) => thenEval(from) }
              .getOrElse(elseEval(from))
        case ExprNode.Add(additive, lhs, rhs) => binaryOp(lhs, rhs, additive.plus)
        case ExprNode.Quotient(integral, lhs, rhs) => binaryOp(lhs, rhs, integral.quot)
        case ExprNode.ToRepr(inner, Codec.FromInjection(injection, _)) =>
          val innerEval = impl(inner)
          from => injection(innerEval(from))
        case ExprNode.FromRepr(inner, Codec.FromInjection(injection, _)) =>
          val innerEval = impl(inner)
          innerEval.andThen(injection.invert)
        case ExprNode.Cast(arg, canCast) => impl(arg).andThen(cast(canCast))
        case ExprNode.TryCast(arg, canTryCast) => impl(arg).andThen(tryCast(canTryCast))
        case ExprNode.TimestampToMicros(inner) => impl(inner).andThen(_.toMicros)
        case ExprNode.MicrosToTimestamp(inner) => impl(inner).andThen(Timestamp.fromMicros)
        case ExprNode.DurationToMicros(inner) => impl(inner).andThen(_.toMicros)
        case ExprNode.MicrosToDuration(inner) => impl(inner).andThen(Duration.fromMicros)
        case ExprNode.DateToDays(inner) => impl(inner).andThen(_.daysSinceEpoch)
        case ExprNode.DaysToDate(inner) => impl(inner).andThen(Date.fromDays)
        case ExprNode.BytesLength(inner) => impl(inner).andThen(_.length)
        case ExprNode.MakeMap(pairs) => from => {
            val seq = impl(pairs)(from)
            val duplicates = seq
              .map(_._1)
              .groupMapReduce(identity)(_ => 1)(_ + _)
              .collect { case (k, v) if v > 1 => k }
              .toSeq
            if duplicates.nonEmpty then
              throw new RuntimeException(s"MakeMap encountered duplicate keys: ${duplicates.mkString(", ")}")
            seq.toMap
          }
        case ExprNode.MapEntries(map) => impl(map).andThen(_.toSeq)
        case ExprNode.MapGet(map, key) =>
          val mapEval = impl(map)
          val keyEval = impl(key)
          from => mapEval(from).get(keyEval(from))
        case ExprNode.DistinctSeq(operand) => (impl(operand).andThen(_.distinct))
        case ExprNode.None(_) => _ => None
        case ExprNode.Rand() => _ => scala.util.Random.nextDouble()
        case ExprNode.IsNaN(operand) => impl(operand).andThen {
            case f: Float => f.isNaN
            case d: Double => d.isNaN
          }

      }

    impl(expr)
  }

  private def cast[From, To](canCast: CanCast[From, To]): From => To =
    canCast match {
      case CanCast.ByteToShort => _.toShort
      case CanCast.ByteToInt => _.toInt
      case CanCast.ByteToLong => _.toLong
      case CanCast.ByteToFloat => _.toFloat
      case CanCast.ByteToDouble => _.toDouble
      case CanCast.ShortToInt => _.toInt
      case CanCast.ShortToLong => _.toLong
      case CanCast.ShortToFloat => _.toFloat
      case CanCast.ShortToDouble => _.toDouble
      case CanCast.IntToLong => _.toLong
      case CanCast.IntToFloat => _.toFloat
      case CanCast.IntToDouble => _.toDouble
      case CanCast.LongToFloat => _.toFloat
      case CanCast.LongToDouble => _.toDouble
      case CanCast.FloatToDouble => _.toDouble
      case CanCast.DoubleToFloat => _.toFloat
      case CanCast.ByteToString => _.toString
      case CanCast.ShortToString => _.toString
      case CanCast.IntToString => _.toString
      case CanCast.LongToString => _.toString
      case CanCast.StringToBytes => Binary.fromString
      case CanCast.DecimalToFloat() => _.toFloat
      case CanCast.DecimalToDouble() => _.toDouble
      case cast @ CanCast.DecimalToDecimal() =>
        import cast.given
        _.widen
      case cast @ CanCast.ByteToDecimal() =>
        import cast.given
        Decimal(_)
      case cast @ CanCast.IntToDecimal() =>
        import cast.given
        Decimal(_)
      case cast @ CanCast.ShortToDecimal() =>
        import cast.given
        Decimal(_)
      case cast @ CanCast.LongToDecimal() =>
        import cast.given
        Decimal(_)
      case CanCast.SeqToSeq(innerCast) => seq => seq.map(cast(innerCast))
    }

  private def tryCast[From, To](canTryCast: CanTryCast[From, To]): From => Option[To] = {
    def checkedIntegral[
        From <: Long | Int | Short | Byte: Numeric as numericFrom,
        To <: Int | Short | Byte: {NumericLimits as limits, Numeric as numericTo}
    ]: From => Option[To] = {
      import numericFrom.mkNumericOps
      import numericTo.mkNumericOps
      value =>
        val longValue = value.toLong
        Option.when(longValue >= limits.min.toLong && longValue <= limits.max.toLong)(numericTo.fromInt(
          value.toInt
        ))
    }

    /* The default parser allows unicode chars as digits, but expected query engine behavior is only to accept
     * ascii digits and signs. */
    def checkedFromString[To: Numeric as numeric]: String => Option[To] =
      str =>
        str.trim match {
          case trimmed if trimmed.forall(c => (c >= '0' && c <= '9') || c == '-' || c == '+') =>
            numeric.parseString(trimmed)
          case _ => None
        }

    canTryCast match {
      case CanTryCast.FromCanCast(canCast) => cast(canCast).andThen(Some(_))
      case CanTryCast.LongToByte => checkedIntegral[Long, Byte]
      case CanTryCast.LongToShort => checkedIntegral[Long, Short]
      case CanTryCast.LongToInt => checkedIntegral[Long, Int]
      case CanTryCast.IntToByte => checkedIntegral[Int, Byte]
      case CanTryCast.IntToShort => checkedIntegral[Int, Short]
      case CanTryCast.ShortToByte => checkedIntegral[Short, Byte]
      case cast @ CanTryCast.FloatToDecimal() =>
        import cast.given
        (v: Float) => Decimal(v)
      case cast @ CanTryCast.DoubleToDecimal() =>
        import cast.given
        (v: Double) => Decimal(v)
      case cast @ CanTryCast.DecimalToDecimal() =>
        import cast.given
        v => Decimal(v.toBigDecimal)
      case CanTryCast.StringToByte => checkedFromString[Byte]
      case CanTryCast.StringToShort => checkedFromString[Short]
      case CanTryCast.StringToInt => checkedFromString[Int]
      case CanTryCast.StringToLong => checkedFromString[Long]
    }
  }
}
