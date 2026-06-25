package com.choreograph.tyda

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple
import scala.annotation.targetName
import scala.annotation.unused
import scala.collection.IterableFactory
import scala.collection.SeqOps
import scala.compiletime.erasedValue
import scala.deriving.Mirror
import scala.reflect.ClassTag

import shapeless3.deriving.K0

import com.choreograph.tyda.TupleOperations.`-`
import com.choreograph.tyda.shapeless3extras.mapConst
import com.choreograph.tyda.shapeless3extras.toTuple
import com.choreograph.tyda.shapeless3extras.tupleInstances

/** Contains the public api for a Expr[T] the api is provided as extension
  * methods.
  *
  * It is implemented as a trait taking a type parameter `Expr[_]` so that it
  * can be reused for both `Expr[_]` and `AggregateExpr[_]` without duplicating
  * the code.
  */
trait ExprApi[Expr[T]] {
  private[tyda] type SeqCC[X, CC[_]] = Seq[X] & SeqOps[X, CC, CC[X]]

  private[tyda] def lift[T](e: ExprNode[T]): Expr[T]
  private[tyda] def unlift[T](e: Expr[T]): ExprNode[T]

  extension (lhs: Expr[Boolean]) {
    def unary_! : Expr[Boolean] = lift(ExprNode.Not(unlift(lhs)))
    infix def ||(rhs: Expr[Boolean]): Expr[Boolean] = lift(ExprNode.Or(unlift(lhs), unlift(rhs)))
    infix def &&(rhs: Expr[Boolean]): Expr[Boolean] = lift(ExprNode.And(unlift(lhs), unlift(rhs)))
  }

  extension [T](opt: Expr[Option[T]]) {
    private def elementCodec: Codec[T] = unlift(opt).codec.element

    private[tyda] def get: Expr[T] = lift(ExprNode.KnownNotNull(unlift(opt)))

    /** True if the option is None, false otherwise
      */
    def isEmpty: Expr[Boolean] = {
      val optNode = unlift(opt)
      lift(ExprNode.Equals(optNode, ExprNode.None(optNode.codec.element)))
    }

    /** If this option is Some then it returned otherwise alternative is
      * returned.
      *
      * At runtime, the default expressions is evaluated lazily, so that this
      * function can be used for control flow involving raiseError.
      */
    def orElse[I: AsExpr.Of[Option[T]]](alternative: I): Expr[Option[T]] =
      lift(ExprNode.Coalesce(Seq(unlift(opt), unlift(AsExpr(alternative)))))

    /** If this option is Some then the value is returned otherwise default is
      * returned.
      *
      * At runtime, the default expressions is evaluated lazily, so that this
      * function can be used for control flow involving raiseError.
      */
    def getOrElse[I: AsExpr.Of[T]](default: I): Expr[T] = {
      val coalesced = ExprNode.Coalesce(Seq(unlift(opt), ExprNode.MakeSome(unlift(AsExpr(default)))))
      lift(ExprNode.KnownNotNull(coalesced))
    }

    /** If this option is Some then the value is returned otherwise raises an
      * error with the given message.
      *
      * This method will cause a runtime error when the option is None. Please
      * consider handling the None case in a safer way, for example using
      * `.map`, `.getOrElse` or `.orElse`
      *
      * Only use `.expect` when you are absolutely certain the option is Some
      * and None would represent a programming error.
      */
    def expect(message: String): Expr[T] = opt.getOrElse(raiseError[T](message)(using elementCodec))

    /** Apply the Expr function to the contained value if the option is Some
      * otherwise returns None.
      */
    @targetName("optionMap")
    def map[U, I: AsExpr.Of[U]](f: Expr[T] => I): Expr[Option[U]] = when(!isEmpty, f(get))

    /** Returns this Option if the predicate returns true or the option is
      * empty, otherwise returns None.
      */
    @targetName("optionFilter")
    def filter(p: Expr[T] => Expr[Boolean]): Expr[Option[T]] = when(isEmpty || p(get), get)

    /** Apply the Expr function returning an Option to the contained value if
      * the option is Some otherwise returns None.
      */
    def flatMap[U, I: AsExpr.Of[Option[U]]](f: Expr[T] => I): Expr[Option[U]] = {
      val mapped = AsExpr(f(get))
      given Codec[U] = mapped.elementCodec
      ternary(isEmpty, lit[Option[U]](None), mapped)
    }

    /** Combine two options into one option of a tuple. If either option is None
      * then the result is None.
      */
    def zip[U](other: Expr[Option[U]]): Expr[Option[(T, U)]] =
      when(!isEmpty && !other.isEmpty, tuple(get, other.get))

    /** Returns true if the option is Some and the predicate returns true when
      * applied to the value.
      */
    @targetName("optionExists")
    def exists[I: AsExpr.Of[Boolean]](p: Expr[T] => I): Expr[Boolean] =
      ternary(isEmpty, ifTrue = false, p(get))

    /** Returns true if the option is None or the predicate returns true when
      * applied to the value.
      */
    @targetName("optionForall")
    def forall[I: AsExpr.Of[Boolean]](p: Expr[T] => I): Expr[Boolean] =
      ternary(isEmpty, ifTrue = true, p(get))

    /** Returns true if the option is Some and contains the value.
      */
    @targetName("optionContains")
    def contains[I: AsExpr.Of[T]](value: I): Expr[Boolean] =
      lift(ExprNode.Equals(unlift(opt), ExprNode.MakeSome(unlift(AsExpr(value)))))
  }

  private def copyOrMerge[T](
      fromExpr: ExprNode[?],
      updateExpr: ExprNode[?],
      updateFieldNames: Set[String],
      resultCodec: Codec.Product[T]
  ): Expr[T] = {
    val combinedFields = resultCodec
      .fields
      .mapConst[ExprNode[?]]([t] =>
        f =>
          if updateFieldNames.contains(f.name) then ExprNode.Select(updateExpr, f.name)
          else ExprNode.Select(fromExpr, f.name)
      )
    lift(ExprNode.makeProductUnsafe(combinedFields, resultCodec))
  }

  extension [T](expr: Expr[T]) {

    /** Transform the value of this expression using the given Scala function.
      */
    def udf[U: Codec](f: T => U): Expr[U] = lift(ExprNode.Udf(unlift(expr), f))

    /** Perform a infallible cast from one type to another.
      *
      * The requirement of `CanCast` means at compile time it checked that the
      * cast is infallible.
      *
      * ```scala
      * import com.choreograph.tyda.functions.lit
      * val e: Expr[Int] = lit(1)
      * e.cast[Long]
      * ```
      *
      * If one wants to perform a fallible cast and error on failure use
      * [[tryCast]] and [[raiseError]].
      *
      * Note we are not using are curried version with a context bound becaue of
      * this https://github.com/scala/scala3/issues/24157 bug.
      */
    def cast[U](using CanCast[T, U]): Expr[U] = lift(ExprNode.Cast(unlift(expr), summon))

    /** Perform a fallible cast producing None on failure.
      *
      * ```scala
      * import com.choreograph.tyda.functions.lit
      * val e: Expr[Long] = Expr.lit(Long.MaxValue)
      * e.tryCast[Int]: Expr[Option[Int]]
      * ```
      *
      * The exact result of casts from Float/Double to Decimal is
      * implementation-defined. However, casting to the decimal type and back to
      * the original floating point type will preserve the original value. In
      * other words, the cast will be done with sufficient precision to avoid
      * precision loss, but the exact decimal representation is not specified.
      *
      * If the value is out of range for the target type the result will be
      * None. If the cast requires rounding it will be done using round half up.
      */
    def tryCast[U](using CanTryCast[T, U]): Expr[Option[U]] = lift(ExprNode.TryCast(unlift(expr), summon))

    /** Project a Expr of a product to a subset of its fields.
      *
      * That the result type is a valid subset is checked at compile time using
      * the [[Projector]] class.
      *
      * Note: T does not have Product as a upper bound, so this method can be
      * used with NamedTuples.
      */
    def project[To: Codec](using
        @unused
        proj: Projector[T, To]
    ): Expr[To] =
      Codec[To] match {
        case prod: Codec.Product[To] =>
          val fromExpr = unlift(expr)
          val fields = prod.fields.mapConst[ExprNode[?]]([t] => f => ExprNode.Select(fromExpr, f.name))
          // The Projector serves as witness that the fields are a subset
          lift(ExprNode.makeProductUnsafe(fields, prod))
        case unsupported => unreachable(s"Projection only supported for product types, found ${unsupported}")
      }

    /** Project a Expr to a product with the same fields.
      *
      * This is useful for converting from a NamedTuple to a case class with the
      * same fields. For projecting to a subset use [[project]] instead.
      */
    def as[To: Codec](using
        projector: Projector[T, To],
        @unused // This is here to witness that the transformation from From to To is bijective
        witness: Projector[To, T]
    ): Expr[To] = project[To]

    /** Copy all fields of the product and replace specified fields with new
      * values.
      *
      * Example: This example does not compile with snippet checking fails with
      * `Macro code depends on object Projector ...` we should probably report a
      * bug if we can minimize it.
      * ```scala sc:nocompile
      * case class Person(name: String, age: Int, city: String)
      * val p1 = Expr[Person](name = "Alice", age = 30, city = "New York")
      * val p2 = p1.copy((age = 31)) // Expr(Person("Alice", 31, "New York"))
      * val p3 = p1.copy((city = "San Francisco")) // Expr(Person("Alice", 30, "San Francisco"))
      * val p4 = p1.copy(name = "Bob", age = 25) // Expr(Person("Bob", 25, "New York"))
      * ```
      */
    def copy[P, I: AsExpr.Of[P]](update: I)(using
        @unused // Evidence that P is a projection of T
        proj: Projector[T, P]
    ): Expr[T] = {
      val fromExpr = unlift(expr)
      val updateExpr = unlift(AsExpr(update))
      (fromExpr.codec, updateExpr.codec) match {
        case (fromCodec: Codec.Product[T], updateCodec: Codec.Product[P]) =>
          val updateFieldNames = updateCodec.fields.mapConst[String]([t] => f => f.name).toSet
          copyOrMerge(fromExpr, updateExpr, updateFieldNames, fromCodec)
        case unsupported => unreachable(s"Copy only supported for product types, found ${unsupported}")
      }
    }

    /** Update a product by merging it with another product, allowing field
      * replacement and addition of new fields.
      *
      * Fields in the update has precedence over the original fields, so they
      * will be used in the resulting type and value when there are duplicate
      * field names.
      *
      * ```scala sc:nocompile
      * case class Person(name: String, age: Int)
      * val p = Expr[Person](name = "Alice", age = 30)
      * val p2: Expr[(name: String, age: String, city: String)] = p.update((age = "thirty", city = "NYC"))
      * ```
      */
    def update[P, I: AsExpr.Of[P]](update: I)(using merger: Merger[T, P]): Expr[merger.Out] = {
      val fromExpr = unlift(expr)
      val updateExpr = unlift(AsExpr(update))
      (fromExpr.codec, updateExpr.codec) match {
        case (fromCodec: Codec.Product[T], updateCodec: Codec.Product[P]) =>
          val updateFields = updateCodec.fields.mapConst[Field[?]]([t] => identity(_))
          val updateFieldNames = updateFields.map(_.name).toSet
          val keptFromFields = fromCodec
            .fields
            .mapConst[Field[?]]([t] => identity(_))
            .filter(f => !updateFieldNames(f.name))
          // TYPE SAFETY: Correctness of Merger.derived ensures the merged fields match merger.Out
          val mergedCodec = Codec
            .unsafeNamedTuple(keptFromFields ++ updateFields)
            .asInstanceOf[Codec.Product[merger.Out]]
          copyOrMerge(fromExpr, updateExpr, updateFieldNames, mergedCodec)
        case unsupported => unreachable(s"Update only supported for product types, found ${unsupported}")
      }
    }

    /** Select the unique field of type E in the elements of type T.
      */
    def select[E: Selector.From[T] as selector]: Expr[E] = {
      val fieldIndex = selector match { case Selector.UnsafeImpl(i) => i }
      unlift(expr).codec match {
        case Codec.Product(_, fields, _) =>
          val fieldsUntyped = fields.mapConst[Field[?]]([t] => identity(_))
          // TYPE SAFETY: The Selector should only derived when field has correct type
          val selected = fieldsUntyped(fieldIndex).asInstanceOf[Field[E]]
          lift(ExprNode.Select(unlift(expr), selected.name))
        case _ => unreachable("Selector should only exists for Product like types")
      }
    }

    /** Remove the unique field of type E in the elements of product type T.
      *
      * Example:
      * ```scala
      * val ds: Dataset[(a: Int, b: Double, c: Float)] = ???
      * val _: Dataset[(a: Int, c: Float)] = ds.select(_.remove[Double])
      * ```
      *
      * Note that tuple input will be interpreted as a NamedTuple:
      * ```scala
      * val ds: Dataset[(Int, Double, Float)] = ???
      * val _: Dataset[NamedTuple.NamedTuple[("_1", "_3"), (Int, Float)]] = ds.select(_.remove[Double])
      * ```
      */
    def remove[E: Remover.From[T] as remover]: Expr[remover.Out] = {
      val fieldIndex = remover match { case Remover.Impl(i) => i }
      val node = unlift(expr)
      node.codec match {
        case Codec.Product(_, fields, _) =>
          val fieldsUntyped = fields
            .mapConst[Field[?]]([t] => identity(_))
            .zipWithIndex
            .filter(_._2 != fieldIndex)
            .map(_._1)
          val selected = fieldsUntyped.map(f => ExprNode.Select(node, f.name))
          // TYPE SAFETY: The Remover should only derived when fields have correct types
          val codec = Codec.unsafeNamedTuple(fieldsUntyped).asInstanceOf[Codec.Product[remover.Out]]
          lift(ExprNode.makeProductUnsafe(selected, codec))
        case _ => unreachable("Remover should only exists for Product like types")
      }
    }

    /** Upcast the expression to a base type B of T, for a sum type B for which
      * T is a variant.
      */
    def asBase[B >: T: Mirror.SumOf: Codec as codec](using mirror: Mirror.ProductOf[T]): Expr[B] =
      codec match {
        case sum @ Codec.Sum(_, variants) =>
          val exprNode = unlift(expr)
          val caseCodec = exprNode.codec
          val discriminant = variants
            .mapConst([t] => identity(_))
            .collectFirst {
              case Variant.Product(name, `caseCodec`) => name
              case Variant.Singleton(name, _, `caseCodec`) => name
            }
            .getOrElse(unreachable(s"Type ${caseCodec} is not a variant of sum type ${sum}"))
          val exprs = sum
            .reprFields
            .map(f =>
              if f.name == Codec.Sum.discriminant then ExprNode.Literal(discriminant)
              else if f.name == discriminant then ExprNode.MakeSome(exprNode)
              else {
                // TYPE SAFETY: All fields other than the discriminant are <: Option[?]
                ExprNode.None(f.codec.asInstanceOf[Codec.Option[?]].element)
              }
            )
          // exprs have the right length and element types as defined by sum
          lift(ExprNode.makeSumUnsafe(exprs, sum))

        case sumAsString @ Codec.SumAsString(_, _) =>
          lift(ExprNode.Literal.create(mirror.fromProduct(EmptyTuple), sumAsString))

        case _ => unreachable("asBase only supported for sum types")
      }

    /** Downcast to a specific case `C` of the sum type `T`.
      *
      * @tparam T
      *   Base sum type
      * @tparam C
      *   Case type to downcast to
      * @return
      *   Some(C) if the expression is case C, otherwise None
      */
    def asCase[C: Codec as caseCodec](using ev: C <:< T, m: Mirror.SumOf[T]): Expr[Option[C]] =
      val exprNode = unlift(expr)
      exprNode.codec match {
        case sum @ Codec.Sum(_, variants) =>
          val reprNode = ExprNode.ToRepr(exprNode, sum)
          variants
            .mapConst([t] => identity(_))
            .collectFirst {
              case Variant.Product(name, `caseCodec`) => lift(ExprNode.Select(reprNode, name))
              case Variant.Singleton(name, single, `caseCodec`) => when(
                  lift(
                    ExprNode.Equals(ExprNode.Select(reprNode, Codec.Sum.discriminant), ExprNode.Literal(name))
                  ),
                  lit(single)
                )
            }
            .getOrElse(unreachable(s"Type ${caseCodec} is not a variant of sum type ${sum}"))

        case sum @ Codec.SumAsString(_, _) => caseCodec match {
            case Codec.Product(_, _, Some(singleton)) =>
              when(lift(ExprNode.Equals(exprNode, ExprNode.Literal.create(singleton, sum))), lit(singleton))
            case _ => unreachable("SumAsString requires that all cases are singletons")
          }

        case _ => unreachable("asCase only supported for sum types")
      }

    /** Start a cases-when-otherwise expression. Use `.when` to handle specific
      * cases and `.otherwise` for all other cases.
      *
      * Example:
      *
      * ```scala
      * //{
      * type Expr = com.choreograph.tyda.Expr
      * //}
      * sealed trait Shape
      * case class Circle(radius: Double) extends Shape derives Codec
      * case class Rectangle(width: Double, height: Double) extends Shape derives Codec
      * case class Triangle(base: Double, height: Double) extends Shape derives Codec
      *
      * val shapeExpr: Expr[Shape] = ???
      *
      * val shapeHeight: Expr[Double] = shapeExpr
      *   .cases[Double]
      *   .when[Circle](circle => circle.radius + circle.radius)
      *   .when[Rectangle](rectangle => rectangle.height)
      *   .when[Triangle](triangle => triangle.height)
      *
      * val isPolygon: Expr[Boolean] = shapeExpr
      *   .cases[Boolean]
      *   .when[Circle](_ => Expr.lit(false))
      *   .otherwise(Expr.lit(true))
      * ```
      *
      * See also:
      *   - [[asCase]] for downcasting to a specific case of a sum type.
      *   - [[asBase]] for upcasting from a specific case to a base sum type.
      *
      * @tparam R
      *   Return type of the all cases branches
      */
    def cases[R](using m: Mirror.SumOf[T]): UnhandledCases[m.MirroredElemTypes, T, R] = UnhandledCases(expr)
  }

  private[tyda] type MatchBuilder[Missing <: Tuple, T, R] = Missing match
    case EmptyTuple => Expr[R]
    case _ => UnhandledCases[Missing, T, R]

  final class UnhandledCases[Missing <: Tuple, T: Mirror.SumOf, R](
      private val expr: Expr[T],
      private val cases: Seq[ExprNode.WhenThen[R]] = Seq.empty[ExprNode.WhenThen[R]]
  ) {

    /** Handle when expression is of type `C`.
      *
      * @param thenExpr
      *   Branch expression to execute when expr is of type `C`
      * @tparam C
      *   Subtype of `T` to handle in this case
      */
    inline def when[C <: T: Codec](
        thenExpr: Expr[C] => Expr[R]
    )(using Tuple.Contains[Missing, C] =:= true): MatchBuilder[Missing - C, T, R] = {
      val caseExpr = expr.asCase[C]
      // SAFETY: We can safely call .get since the condition ensures caseExpr is not None
      val caseBranch = ExprNode.WhenThen(unlift(!caseExpr.isEmpty), unlift(thenExpr(caseExpr.get)))
      inline erasedValue[Missing - C] match {
        case _: EmptyTuple => lift(ExprNode.Cases(
            caseBranch,
            cases,
            unlift[R](raiseError("Unknown case")(using caseBranch.thenExpr.codec))
          ))
        case _ => UnhandledCases(expr, caseBranch +: cases)
      }
    }

    /** Handle all remaining cases not handled by previous `.when` calls.
      *
      * @param default
      *   Branch to execute for all remaining cases
      */
    def otherwise(default: Expr[R]): Expr[R] =
      cases match {
        case Nil => default
        case head +: tail => lift(ExprNode.Cases(head, tail, unlift(default)))
      }
  }

  // Compasion operators for Expr[T] where T has a given Ord[T] instance.
  extension [T: Comparable](lhs: Expr[T]) {

    /** Returns true if lhs is equal to rhs using the given Ord[T].
      */
    infix def >[I: AsExpr.Of[T]](rhs: I): Expr[Boolean] = !(lhs <= rhs)

    /** Returns true if lhs is greater than or equal to rhs using the given
      * Ord[T].
      */
    infix def >=[I: AsExpr.Of[T]](rhs: I): Expr[Boolean] = !(lhs < rhs)

    /** Returns true if lhs is less than rhs using the given Ord[T].
      */
    infix def <[I: AsExpr.Of[T]](rhs: I): Expr[Boolean] =
      lift(ExprNode.LessThan(Comparable[T], unlift(lhs), unlift(AsExpr(rhs))))

    /** Returns true if lhs is less than or equal to rhs using the given Ord[T].
      */
    infix def <=[I: AsExpr.Of[T]](rhs: I): Expr[Boolean] =
      lift(ExprNode.LessThanOrEqual(Comparable[T], unlift(lhs), unlift(AsExpr(rhs))))
  }

  extension [T: AdditiveExpr](lhs: Expr[T]) {

    /** Returns sum of two expressions.
      *
      * Throws an exception if the operation leads to overflowing of integral
      * types.
      */
    infix def +[I: AsExpr.Of[T]](rhs: I): Expr[T] =
      lift(ExprNode.Add(AdditiveExpr[T], unlift(lhs), unlift(AsExpr(rhs))))
  }

  extension [T: Integral](lhs: Expr[T]) {

    /** Returns the result of truncating division of the left operand by the
      * right operand.
      *
      * Throws an exception if the divisor is zero.
      */
    infix def /[I: AsExpr.Of[T]](rhs: I): Expr[T] =
      lift(ExprNode.Quotient(Integral[T], unlift(lhs), unlift(AsExpr(rhs))))

  }

  extension [T <: Float | Double](fp: Expr[T]) {

    /** Returns true if the value is NaN (Not a Number).
      *
      * Note: for Double and Float, NaN != NaN per IEEE 754. Use this method to
      * explicitly check for NaN values.
      */
    def isNaN: Expr[Boolean] = lift(ExprNode.IsNaN(unlift(fp)))
  }

  extension (string: Expr[String]) {

    /** True if this string starts with the given prefix. */
    def startsWith(prefix: Expr[String]): Expr[Boolean] =
      lift(ExprNode.StartsWith(unlift(string), unlift(prefix)))

    /** True if this string starts with the given literal prefix. */
    def startsWith(prefix: String): Expr[Boolean] = startsWith(lit(prefix))

    /** Removes leading and trailing whitespace from the string.
      */
    def trim(): Expr[String] = lift(ExprNode.Trim(unlift(string)))

    /** Returns true if the string ends with the given suffix.
      */
    def endsWith(suffix: String): Expr[Boolean] = endsWith(lit(suffix))

    /** Returns true if the string ends with the given suffix expression.
      */
    def endsWith(suffix: Expr[String]): Expr[Boolean] =
      lift(ExprNode.EndsWith(unlift(string), unlift(suffix)))

    /** Splits string to sequence of strings by literal delimiter.
      *
      * For more details see [[split(delimiter:Expr)]].
      */
    def split(delimiter: String): Expr[Seq[String]] = split(lit(delimiter))

    /** Splits string to sequence of strings by expr delimiter.
      *
      * Regexp as delimiter is not supported. Splitting by empty string gives a
      * sequence of individual characters.
      */
    def split(delimiter: Expr[String]): Expr[Seq[String]] =
      lift(ExprNode.Split(unlift(string), unlift(delimiter)))
  }

  /** Concatenates one or more strings together. */
  def concat(s0: Expr[String], rest: Expr[String]*): Expr[String] =
    lift(ExprNode.ConcatString((s0 +: rest).map(unlift).toSeq))

  extension (timestamp: Expr[Timestamp]) {

    /** Returns the number of microseconds since the unix epoch. */
    @targetName("timestampToMicros")
    def toMicros: Expr[Long] = lift(ExprNode.TimestampToMicros(unlift(timestamp)))
  }

  extension (duration: Expr[Duration]) {

    /** Returns the duration in microseconds. */
    @targetName("durationToMicros")
    def toMicros: Expr[Long] = lift(ExprNode.DurationToMicros(unlift(duration)))
  }

  extension (date: Expr[Date]) {

    /** Returns the number of days since the unix epoch. */
    def toDays: Expr[Int] = lift(ExprNode.DateToDays(unlift(date)))
  }

  extension (bytes: Expr[Binary]) {

    /** Returns the number of bytes. */
    def length: Expr[Int] = lift(ExprNode.BytesLength(unlift(bytes)))
  }

  extension [T, C <: Seq[T]](seq: Expr[C]) {

    /** Returns the size of the sequence.
      */
    def size: Expr[Int] = lift(ExprNode.SizeSeq(unlift(seq)))

    /** Returns the element at the given index.
      *
      * The index is zero-based. If the index is out of bounds a runtime error
      * will occur.
      *
      * Note: This method is named `get` instead of `apply` to avoid conflicts
      * with the auto-tupling behavior of `Expr.apply` used for struct
      * construction.
      */
    def get(index: Expr[Int]): Expr[T] = lift(ExprNode.ElementSeq(seq.toRepr, unlift(index)))

    /** Returns the element at the given index.
      *
      * For more details see [[get(index:Expr)]].
      */
    def get(index: Int): Expr[T] = get(lit(index))

    /** Performs an upcast from the specific collection type C to Seq[T].
      */
    def toSeq: Expr[Seq[T]] = lift(seq.toRepr)

    private def toRepr: ExprNode[Seq[T]] = {
      val node = unlift(seq)
      node.codec match {
        case Codec.Seq(_) => node
        /* TYPE SAFETY: Because of Seq being covariant the compiler can not prove this is correct. So maybe
         * there are edge cases where this do not hold? */
        case it: Codec.Iterable[T @unchecked, C] => ExprNode.ToRepr(node, it)
        case unsupported => unreachable(s"Expected Iterable codec but got $unsupported")
      }
    }
  }

  extension [T, CC[X] <: SeqCC[X, CC], C <: SeqCC[T, CC]](seq: Expr[C]) {
    private def valueCodec: Codec[T] = unlift(seq).codec.element

    private def factory: IterableFactory[CC] =
      unlift(seq).codec match {
        case Codec.Seq(_) => IndexedSeq
        case it: Codec.Iterable[?, C] => it.factory.fromSpecific(Seq.empty).iterableFactory
        case unsupported => unreachable(s"Expected Iterable codec but got $unsupported")
      }

    private def fromRepr[U: Codec](node: ExprNode[Seq[U]])(using tag: ClassTag[CC[U]]): Expr[CC[U]] = {
      val outputCodec = Codec.Iterable[U, CC[U]](tag, Codec[U])(using factory)
      lift(ExprNode.FromRepr(node, outputCodec))
    }

    /** Returns the distinct elements of the sequence.
      */
    def distinct(using ClassTag[CC[T]]): Expr[CC[T]] =
      fromRepr(ExprNode.DistinctSeq(seq.toRepr))(using valueCodec)

    /** Maps each element of the sequence using the given function.
      */
    def map[U, I: AsExpr.Of[U]](f: Expr[T] => I)(using ClassTag[CC[U]]): Expr[CC[U]] = {
      val arg = ExprNode.Reference[T]()(using seq.valueCodec)
      val fExpr = f.andThen(AsExpr(_))
      val compiled = CompiledExpr(arg, unlift(fExpr(lift(arg))))
      fromRepr(ExprNode.MapSeq(unlift(seq.toSeq), compiled))(using compiled.codec)
    }

    /** FlatMaps each element of the sequence using the given function, then
      * concatenates the resulting sequences into one.
      */
    def flatMap[U, I: AsExpr.Of[Iterable[U]]](f: Expr[T] => I)(using ClassTag[CC[U]]): Expr[CC[U]] =
      seq.map(f).flatten

    /** Filters the sequence to only include elements satisfying the predicate.
      */
    def filter(p: Expr[T] => Expr[Boolean]): Expr[CC[T]] = {
      val arg = ExprNode.Reference[T]()(using valueCodec)
      val compiled = CompiledExpr(arg, unlift(p(lift(arg))))
      fromRepr(ExprNode.FilterSeq(unlift(seq.toSeq), compiled))(using valueCodec)
    }

    /** Zips two sequences into a sequence of pairs.
      *
      * The resulting sequence has length equal to the smaller of the two input
      * sequences.
      */
    def zip[U, I: AsExpr.Of[Seq[U]]](other: I): Expr[Seq[(T, U)]] = {
      val otherSeq = AsExpr(other)
      range(0, min(seq.size, otherSeq.size)).map(index => (seq.get(index), otherSeq.get(index)))

    }

    /** Concatenate two sequences into a new sequence.
      */
    infix def ++[C2 <: Seq[T]](other: Expr[C2])(using ClassTag[CC[T]]): Expr[CC[T]] =
      fromRepr(ExprNode.ConcatSeq(seq.toRepr, other.toRepr))(using valueCodec)

    private def fold[R](
        agg: PrimitiveAggregate[T, R] & ExprNode.AggregateSeq.SupportedAggregates,
        onEmpty: R
    ): Expr[R] = lift(ExprNode.AggregateSeq(seq.toRepr, ExprNode.Literal.create(onEmpty, agg.codec), agg))

    /** Check if all elements in the sequence satisfy the given predicate.
      */
    def forall[I: AsExpr.Of[Boolean]](p: Expr[T] => I): Expr[Boolean] =
      map(p).fold(PrimitiveAggregate.BoolAnd(), onEmpty = true)

    /** Check if any element in the sequence satisfy the given predicate.
      */
    def exists[I: AsExpr.Of[Boolean]](p: Expr[T] => I): Expr[Boolean] =
      map(p).fold(PrimitiveAggregate.BoolOr(), onEmpty = false)

    /** Check if the sequence contains the given element.
      */
    def contains[I: AsExpr.Of[T]](value: I): Expr[Boolean] =
      exists(e => lift(ExprNode.Equals(unlift(e), unlift(AsExpr(value)))))
  }

  extension [U, CC[X] <: SeqCC[X, CC], C <: SeqCC[Iterable[U], CC]](seq: Expr[C]) {

    /** Flattens a sequence of iterables into a single sequence.
      */
    def flatten(using tag: ClassTag[CC[U]]): Expr[CC[U]] = {
      val node = unlift(seq.toSeq)
      given Codec[U] = node.codec.element.element
      lift(ExprNode.FromRepr(
        ExprNode.FlattenSeq(node),
        Codec.Iterable[U, CC[U]](tag, Codec[U])(using seq.factory)
      ))
    }
  }

  extension [K, V](entries: Expr[Seq[(K, V)]]) {

    /** Convert the sequence of key-value pairs into a Map.
      *
      * Throws an error at runtime if there are duplicate keys.
      */
    def toMap: Expr[Map[K, V]] = lift(ExprNode.MakeMap(unlift(entries)))
  }

  extension [K, V](map: Expr[Map[K, V]]) {

    /** Returns the key-value entries of the map as a sequence.
      */
    def entries: Expr[Seq[(key: K, value: V)]] = lift(ExprNode.MapEntries(unlift(map)))

    /** Returns the value associated with the given key.
      *
      * Returns None if the key is not present in the map.
      *
      * Note: Spark currently has a performance problem with Map lookup
      * (https://github.com/apache/spark/issues/54646) so this method is
      * intended for use with small maps where the performance issue is not
      * significant. TODO: remove this when the support a version of Spark with
      * the performance issue fixed.
      */
    @targetName("mapGet")
    def get(key: Expr[K]): Expr[Option[V]] = lift(ExprNode.MapGet(unlift(map), unlift(key)))
  }

  extension [N1 <: Tuple, V1 <: Tuple](e: Expr[NamedTuple[N1, V1]]) {

    /** Concatenate two NamedTuples into a new NamedTuple.
      *
      * This is the same as [[NamedTuple.++]] but for Exprs of NamedTuples.
      */
    infix def ++[N2 <: Tuple, V2 <: Tuple](other: Expr[NamedTuple[N2, V2]])(using
        Tuple.Disjoint[N1, N2] =:= true
    ): Expr[NamedTuple.Concat[NamedTuple[N1, V1], NamedTuple[N2, V2]]] = concatUnchecked(other)

    private[tyda] def concatUnchecked[N2 <: Tuple, V2 <: Tuple](other: Expr[NamedTuple[N2, V2]])(using
        Tuple.Disjoint[N1, N2] =:= true
    ): Expr[NamedTuple.Concat[NamedTuple[N1, V1], NamedTuple[N2, V2]]] = {
      val leftNode = unlift(e)
      val rightNode = unlift(other)
      (leftNode.codec, rightNode.codec) match {
        case (leftProd: Codec.Product[NamedTuple[N1, V1]], rightProd: Codec.Product[NamedTuple[N2, V2]]) =>
          def exprs[T](node: ExprNode[T], prod: Codec.Product[T]): Seq[ExprNode[?]] =
            prod.fields.mapConst[ExprNode[?]]([t] => f => ExprNode.Select(node, f.name))
          val combinedExprs = exprs(leftNode, leftProd) ++ exprs(rightNode, rightProd)
          /* TYPE SAFETY: When both left and right are NamedTuples the result from `concat` is the same as
           * `Concat` */
          val codec = Codec
            .concat(leftProd, rightProd)
            .asInstanceOf[Codec.Product[NamedTuple.Concat[NamedTuple[N1, V1], NamedTuple[N2, V2]]]]
          // combinedExprs is contructed to match the Tuple.Concat[A, B] structure
          lift(ExprNode.makeProductUnsafe(combinedExprs, codec))
        case unsupported => unreachable(s"NamedTuples must use Product codec, but found ${unsupported}")
      }
    }

    /** Concatenate a NamedTuple with a literal NamedTuple.
      *
      * For more details see [[++(other:Expr)]]. Note this method is overloaded
      * as that leads to better compiler warnings then if we only had a single
      * method and leveraged the AsExpr conversion.
      */
    infix def ++[V2 <: Tuple, N2 <: Tuple, I <: Tuple](other: NamedTuple[N2, I])(using
        AsExpr[NamedTuple[N2, I], NamedTuple[N2, V2]],
        Tuple.Disjoint[N1, N2] =:= true
    ): Expr[NamedTuple.Concat[NamedTuple[N1, V1], NamedTuple[N2, V2]]] = e ++ AsExpr(other)
  }

  extension [N1 <: Tuple, V1 <: Tuple](e: Expr[Option[NamedTuple[N1, V1]]]) {

    /** TODO: Decide if we should make part of the public api. */
    private[tyda] def spreadOption: Expr[NamedTuple.Map[NamedTuple[N1, V1], Option]] =
      unlift(e).codec match {
        case Codec.Option(Codec.Product(_, fields, _)) =>
          val guard = unlift(e.isEmpty)
          val value = unlift(e.get)
          val noneExprs = fields.mapConst[ExprNode[?]]([t] => f => ExprNode.None(f.codec))
          val someExprs =
            fields.mapConst[ExprNode[?]]([t] => f => ExprNode.MakeSome(ExprNode.Select(value, f.name)))
          val nullableFields = fields.mapConst([t] => f => f.copy(codec = Codec.Option(f.codec)))
          // TYPE SAFETY: The map above only adds Option to each field.
          val codec = Codec
            .unsafeNamedTuple(nullableFields)
            .asInstanceOf[Codec.Product[NamedTuple.Map[NamedTuple[N1, V1], Option]]]
          val someProduct = ExprNode.makeProductUnsafe(someExprs, codec)
          val noneProduct = ExprNode.makeProductUnsafe(noneExprs, codec)
          lift(ExprNode.Cases.ternary(guard, noneProduct, someProduct))
        case unsupported => unreachable(s"Expected Option of Product codec but got ${unsupported}")
      }
  }

  extension [T: Mirror.ProductOf as m](e: Expr[T]) {

    private def codec: Codec.Product[T] =
      // TYPE SAFETY: The Mirror.ProductOf context bound ensures that Codec[T] is a Product codec
      unlift(e).codec.asInstanceOf[Codec.Product[T]]

    /** Create an expression of a named tuple from a product.
      */
    def toNamedTuple: Expr[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes]] =
      val exprs = tupleInstances(unapply(e)).mapConst([t] => unlift(_))
      val codec = Codec.namedTuple(e.codec)
      lift(ExprNode.makeProductUnsafe(exprs, codec))

  }

  /** Extractor that allows Expr of Products being unwrapped to Expr of each
    * field.
    *
    * For example should allow code like:
    * {{{
    *  val ds: Dataset[(Int, Int)] = ???
    *  ds.select { case Expr(first, _) => first
    * }}}
    */
  def unapply[T: Mirror.ProductOf as m](
      e: Expr[T]
  ): NamedTuple.Map[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes], Expr] = {
    val fieldNames = e.codec.fields.mapConst([t] => _.name)
    val values = fieldNames.map(ExprNode.Select(unlift(e), _)).map(lift).toArray
    // TYPE SAFETY: The Mirror must match the fields in the product codec.
    Tuple
      .fromArray(values)
      .asInstanceOf[NamedTuple.Map[NamedTuple[m.MirroredElemLabels, m.MirroredElemTypes], Expr]]
  }

  /* Conversion from T => Expr[R].
   *
   * This is used in methods like Dataset.select it possible to return a normal tuple of exprs and have it
   * work without additionals syntax or implicit conversion. */
  trait AsExpr[T, R] extends Conversion[T, Expr[R]]

  trait LowPriorityAsExpr {
    given optionToIterable[T]: AsExpr[Expr[Option[T]], Iterable[T]] =
      e => lift(ExprNode.OptionToIterable(unlift(e)))

    given iterable[T, C <: Iterable[T]]: AsExpr[Expr[C], Iterable[T]] =
      e => lift(ExprNode.UpcastToIterable(unlift(e)))

    given literal[T: Codec]: AsExpr[T, T] = v => lift(ExprNode.Literal(v))
  }

  object AsExpr extends LowPriorityAsExpr {
    // Curried version for use in context bounds
    type Of[R] = [T] =>> AsExpr[T, R]

    def apply[T, R](using i: AsExpr[T, R]): AsExpr[T, R] = i

    trait TupleAsExpr[T <: Tuple, TRes <: Tuple] {
      def apply(t: T): Tuple.Map[TRes, Expr]
    }

    object TupleAsExpr {
      def apply[T <: Tuple, R <: Tuple](using i: TupleAsExpr[T, R]): TupleAsExpr[T, R] = i

      type Of[TRes <: Tuple] = [T <: Tuple] =>> TupleAsExpr[T, TRes]

      given empty: TupleAsExpr[EmptyTuple, EmptyTuple] with {
        def apply(t: EmptyTuple): EmptyTuple = EmptyTuple
      }

      given head[R, H: AsExpr.Of[R], TRes <: Tuple, T <: Tuple: TupleAsExpr.Of[TRes]]
          : TupleAsExpr[H *: T, R *: TRes] with {
        def apply(t: H *: T): Tuple.Map[R *: TRes, Expr] =
          AsExpr[H, R](t.head) *: TupleAsExpr[T, TRes](t.tail)
      }
    }

    given expr[T]: AsExpr[Expr[T], T] = identity(_)

    given tuple[TRes <: Tuple, T <: Tuple: TupleAsExpr.Of[TRes]]: AsExpr[T, TRes] =
      inputTuple => {
        val tupledExpression = tupleInstances(TupleAsExpr(inputTuple)).mapK([t] => unlift(_))
        lift(ExprNode.makeTuple(tupledExpression.toTuple))
      }

    given namedTuple[Fields <: Tuple: StringLiterals, TRes <: Tuple, T <: Tuple: TupleAsExpr.Of[TRes]]
        : AsExpr[NamedTuple[Fields, T], NamedTuple[Fields, TRes]] =
      inputTuple => {
        val nodes = tupleInstances(TupleAsExpr(inputTuple)).mapK([t] => unlift(_)).toTuple
        lift(ExprNode.makeNamedTuple(nodes))
      }
  }

  /** Construct an Expr[To] where To is a product with named fields or a named
    * tuple. Example:
    *
    * ```scala sc:nocompile
    * final case class Person(name: String, age: Int)
    * val personExpr = Expr[Person](name = "Alice", age = 30)
    * ```
    *
    * This method ensures that the field names and types in the input match
    * exactly with the output.
    */
  def apply[To: Codec](using
      DummyImplicit
  )[From <: AnyNamedTuple, I: AsExpr.Of[From]](args: I)(using
      projector: Projector[From, To],
      @unused // This is here to witness that the transformation from From to To is bijective
      witness: Projector[To, From]
  ): Expr[To] = AsExpr(args).project[To]

  /** Returns the first expr that is not None, or None if all are None.
    *
    * At runtime, the expressions are evaluated lazily, so that this function
    * can be used for control flow involving raiseError.
    */
  def coalesce[T](expr1: Expr[Option[T]], exprs: Expr[Option[T]]*): Expr[Option[T]] =
    lift(ExprNode.Coalesce((expr1 +: exprs).map(unlift)))

  /** Lift a literal value into an Expr. */
  def lit[T: Codec](value: T): Expr[T] = lift(ExprNode.Literal(value))

  /** Raise an error with the provided static message.
    *
    * For details see [[raiseError(message:Expr]]
    */
  def raiseError[T: Codec](message: String): Expr[T] = raiseError(lit(message))

  /** Raise an error with the provided Expr message.
    *
    * Note: Since Expr is invariant the return type is a based on a type
    * parameter T instead of Expr[Nothing].
    */
  def raiseError[T: Codec](message: Expr[String]): Expr[T] = lift(ExprNode.RaiseError(unlift(message)))

  /** Create a Seq from a sequence of expressions.
    */
  def seq[T: Codec](exprs: Seq[Expr[T]]): Expr[Seq[T]] = lift(ExprNode.MakeSeq(exprs.map(unlift)))

  /** Create an empty Seq
    */
  def seq[T: Codec](): Expr[Seq[T]] = seq(Seq.empty)

  /** Create a Seq from a sequence of expressions.
    */
  def seq[T](expr: Expr[T], exprs: Expr[T]*): Expr[Seq[T]] =
    given Codec[T] = unlift(expr).codec
    seq(expr +: exprs)

  /** Create an empty Map.
    */
  def makeMap[K: Codec, V: Codec](): Expr[Map[K, V]] = seq[(K, V)]().toMap

  /** Create a Map from a sequence of key-value pair expressions.
    *
    * Throws an error at runtime if there are duplicate keys.
    */
  def makeMap[K, V](entry: (Expr[K], Expr[V]), entries: (Expr[K], Expr[V])*): Expr[Map[K, V]] =
    seq(tuple(entry), entries.map(tuple)*).toMap

  /** Returns the minium of two expressions using the given Ord[T].
    */
  def min[T: Comparable](a: Expr[T], b: Expr[T]): Expr[T] = ternary(a < b, a, b)

  /** Returns the maximum of two expressions using the given Ord[T].
    */
  def max[T: Comparable](a: Expr[T], b: Expr[T]): Expr[T] = ternary(a < b, b, a)

  /** Create a Expr[Option[T]] from a Expr[T].
    */
  def some[R, I: AsExpr.Of[R]](expr: I): Expr[Option[R]] = lift(ExprNode.MakeSome(unlift(AsExpr(expr))))

  /** Create a Expr[Option[T]] corresponding to a None.
    */
  def none[T: Codec]: Expr[Option[T]] = lift(ExprNode.None(Codec[T]))

  /** Creates a Timestamp from microseconds since the unix epoch. */
  def microsToTimestamp(micros: Expr[Long]): Expr[Timestamp] =
    lift(ExprNode.MicrosToTimestamp(unlift(micros)))

  /** Creates a Duration from a number of microseconds. */
  def microsToDuration(micros: Expr[Long]): Expr[Duration] = lift(ExprNode.MicrosToDuration(unlift(micros)))

  /** Creates a Date from the number of days since the unix epoch. */
  def daysToDate(days: Expr[Int]): Expr[Date] = lift(ExprNode.DaysToDate(unlift(days)))

  /** Create an expression of a tuple from a tuple of expressions.
    */
  def tuple[U <: Tuple, T <: Tuple: AsExpr.Of[U] as asExpr](t: T): Expr[U] = asExpr(t)

  /** Create an expression of a named tuple from a named tuple of expressions.
    */
  def namedTuple[N <: Tuple, U <: Tuple, T <: Tuple](t: NamedTuple[N, T])(using
      AsExpr[NamedTuple[N, T], NamedTuple[N, U]]
  ): Expr[NamedTuple[N, U]] = AsExpr(t)

  /** Create a range expression from start (inclusive) to end (exclusive).
    *
    * e.g. range(lit(0), lit(4)) yields Expr of Seq(0, 1, 2, 3)
    *
    * If end <= start the result is an empty sequence.
    */
  def range[Start: AsExpr.Of[Int], End: AsExpr.Of[Int]](start: Start, end: End): Expr[Seq[Int]] =
    lift(ExprNode.Range(unlift(AsExpr[Start, Int](start)), unlift(AsExpr[End, Int](end))))

  /** Returns a random Double uniformly distributed in [0.0, 1.0).
    *
    * The value is independently sampled for every row.
    */
  def rand(): Expr[Double] = lift(ExprNode.Rand())

  /** Create a ternary expression, yielding `ifTrue` if `cond` is true,
    * otherwise `ifFalse`.
    *
    * At runtime, the ifTrue and ifFalse expressions are evaluated lazily, so
    * that this function can be used for control flow involving raiseError.
    */
  def ternary[R, IfTrue: AsExpr.Of[R] as asExprIfTrue, IfFalse: AsExpr.Of[R] as asExprIfFalse](
      cond: Expr[Boolean],
      ifTrue: IfTrue,
      ifFalse: IfFalse
  ): Expr[R] =
    lift(ExprNode.Cases.ternary(unlift(cond), unlift(asExprIfTrue(ifTrue)), unlift(asExprIfFalse(ifFalse))))

  /** Create an Option containing `ifTrue` if `cond` is true, otherwise None.
    *
    * At runtime, the ifTrue expression is evaluated lazily, so that this
    * function can be used for control flow involving raiseError.
    */
  def when[R, I: AsExpr.Of[R] as asExpr](cond: Expr[Boolean], ifTrue: I): Expr[Option[R]] =
    val ifTrueExpr = asExpr(ifTrue)
    ternary(cond, some(ifTrueExpr), none(using unlift(ifTrueExpr).codec))

  /** Serialize a value expression to a JSON string. */
  def toJson[T: JsonArrayOrObject](expr: Expr[T]): Expr[String] = lift(ExprNode.ToJson(unlift(expr)))

  /** Parse a JSON string expression into a value of type T.
    *
    * Returns `None` if the input string is not a valid JSON representation of
    * T.
    */
  def fromJson[T: Codec: JsonArrayOrObject](expr: Expr[String]): Expr[Option[T]] =
    lift(ExprNode.FromJson(unlift(expr), Codec[T]))
}
