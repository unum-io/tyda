package com.choreograph.tyda.table

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InvalidClassException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.OptionalDataException
import java.io.StreamCorruptedException
import java.util.Base64

import scala.annotation.tailrec
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.deriving.Mirror
import scala.util.chaining.scalaUtilChainingOps

import shapeless3.deriving.K0
import shapeless3.deriving.Labelling

import com.choreograph.tyda.AllSingletons
import com.choreograph.tyda.Date
import com.choreograph.tyda.Defaults
import com.choreograph.tyda.SimpleTypeName
import com.choreograph.tyda.shapeless3extras.Combine
import com.choreograph.tyda.shapeless3extras.EitherApInstances.given
import com.choreograph.tyda.shapeless3extras.StateAp
import com.choreograph.tyda.shapeless3extras.labelled
import com.choreograph.tyda.shapeless3extras.productInstances
import com.choreograph.tyda.table.ArgsParser.Error
import com.choreograph.tyda.table.ArgsParser.Result
import com.choreograph.tyda.toMirroredElemTypes

/** ArgsParser is a typeclass that can parse and serialize command line
  * arguments based on a case class T.
  *
  * Basic usage:
  * ```
  * case class Args(foo: Int, bar: String, fooBaz: Boolean = false)
  * ArgsParser[Args].parse(Seq("--foo", "42", "--bar=hello")) // Right(Args(42, "hello", false))
  * ArgsParser[Args].serialize(Args(42, "hello")) // List("--foo", "42", "--bar", "hello")
  * ```
  */
trait ArgsParser[T] {
  def parse(path: Option[String], args: ParsedArgs, default: Option[T]): (ParsedArgs, Result[T])
  def serialize(path: Option[String], t: T, default: Option[T], templateTag: Option[String]): List[String]
  def help(path: Option[String], default: Option[T]): ArgsParser.HelpInfo
}

final case class ParsedArgs(keyValues: Map[String, List[String]], parsingErrors: List[Error]) {
  def consume[T](
      key: String,
      arg: ArgsParser.Arg[T],
      default: Option[T]
  ): (ParsedArgs, ArgsParser.Result[T]) =
    keyValues.get(key) match {
      case Some(Nil) => assert(assertion = false, "This should never happen")
      case Some(value :: Nil) => (copy(keyValues = keyValues - key), arg.parseResult(value))
      case Some(value :: tail) => (copy(keyValues = keyValues.updated(key, tail)), arg.parseResult(value))
      case None => (this, default.toRight(ArgsParser.Error.MissingRequired(key, arg.hint)))
    }

  def consumeAll[T](
      key: String,
      arg: ArgsParser.Arg[T],
      default: Option[Seq[T]]
  ): (ParsedArgs, ArgsParser.Result[Seq[T]]) =
    keyValues.get(key) match {
      case Some(Nil) => assert(assertion = false, "This should never happen")
      case Some(values) =>
        val parsed = values
          .map(arg.parseResult)
          .foldRight(Right(Vector[T]()): Result[Seq[T]])((value, acc) =>
            ArgsParser.combine(value, acc, _ +: _)
          )
        (copy(keyValues = keyValues - key), parsed)
      case None => (this, default.toRight(ArgsParser.Error.MissingRequired(key, arg.hint)))
    }

  def finished: Option[Error] = {
    val unusedError = keyValues
      .toSeq
      .flatMap { case (key, values) => values.map((key, _)) }
      .map(Error.UnusedFlag.apply)
    (parsingErrors ++ unusedError).reduceOption(_ ++ _)
  }
}

object ArgsParser {
  def apply[T](using parser: ArgsParser[T]): ArgsParser[T] = parser

  type Result[T] = Either[Error, T]

  /** Error represents a failure to parse arguments. */
  enum Error {

    /** A required key was not found in the arguments. */
    case MissingRequired(key: String, hint: String)

    /** No value was provided for a key. */
    case MissingValue(key: String)

    /** An argument did not start with -- and was not parsed into an argument
      * flag
      */
    case UnparsedFlag(unparsed: String)

    /** A value could not be parsed into the expected type. */
    case UnparsableValue(value: String, hint: String)

    /** A flag was provided that did not match any known flags. */
    case UnusedFlag(flag: String, value: String)

    /** Multiple errors occurred. */
    case Multiple(errors: Seq[Error])

    /** Failed to parse enum value, each error is from a different parser. */
    case EnumAllFailed(error: Seq[Error])

    /** Enum multiple parsers succeeded, so it is ambiguous which one to use. */
    case EnumAmbiguous(values: Seq[String])

    def ++(other: Error): Error =
      (this, other) match {
        case (Multiple(errors), Multiple(others)) => Multiple(errors ++ others)
        case (Multiple(errors), err2) => Multiple(errors :+ err2)
        case (err1, Multiple(errors)) => Multiple(err1 +: errors)
        case (err1, err2) => Multiple(Seq(err1, err2))
      }

    private def formatted(level: Int): String = {
      val indent = "  " * level
      this match {
        case Error.MissingRequired(key, hint) =>
          s"${indent}Missing required argument: --$key with value hint: $hint"
        case Error.MissingValue(key) => s"${indent}Missing value for argument: --$key"
        case Error.UnparsedFlag(unparsed) => s"${indent}Unparsed flag: $unparsed"
        case Error.UnparsableValue(value, hint) => s"${indent}Unparsable value: $value with value hint $hint"
        case Error.UnusedFlag(flag, value) => s"${indent}Unused flag: $flag with value $value"
        case Error.EnumAllFailed(errors) => errors
            .map(_.formatted(level + 1))
            .mkString(s"${indent}Failed to parse enum. The errors were:\n", s"\n${indent}Or\n", "")
        case Error.EnumAmbiguous(values) =>
          s"${indent}Ambiguous enum, could be parsed as any of: ${values.mkString(", ")}"
        case Error.Multiple(errors) => errors.map(_.formatted(level)).mkString("\n")
      }
    }

    def formatted: String = formatted(0)
  }

  object Error {
    given Combine[Error] = _ ++ _
  }

  enum DefaultInfo {
    case Value(value: String)
    // Default value is None/Seq.empty
    case EmptyDefault
    // There is no default and the argument is required
    case NoDefault
  }

  /** HelpInfo represents information about available command line arguments. */
  enum HelpInfo {

    /** An argument that takes one or more values. */
    case Arg(flag: String, hint: String, default: DefaultInfo, multiple: Boolean)

    /** A collection of arguments. */
    case Arguments(fields: Seq[HelpInfo])

    /** A sum type without tag, showing alternatives. */
    case SumUntagged(prefix: Option[String], alternatives: Seq[(String, HelpInfo)])

    /** A sum type with tag, showing discriminant and cases. */
    case SumTagged(discriminantFlag: String, cases: Seq[(String, HelpInfo)], default: Option[String])

    private def formatted(level: Int): String = {
      val indent = "  " * level
      this match {
        case HelpInfo.Arg(flag, hint, default, multiple) =>
          val reqAndDefault = (default, multiple) match {
            case (DefaultInfo.NoDefault, true) => " [required, multiple]"
            case (DefaultInfo.NoDefault, false) => " [required]"
            case (DefaultInfo.Value(d), true) => s" [multiple] (default: $d)"
            case (DefaultInfo.Value(d), false) => s" (default: $d)"
            case (_, true) => " [multiple]"
            case (_, false) => ""
          }
          s"${indent}--$flag <$hint>$reqAndDefault"
        case HelpInfo.Arguments(fields) => fields.map(_.formatted(level)).mkString("\n")
        case HelpInfo.SumUntagged(prefix, alternatives) =>
          val header = prefix match {
            case Some(p) => s"${indent}Alternative --$p one of:"
            case None => s"${indent}one of:"
          }
          val cases = alternatives
            .map { case (label, helpInfo) => s"${indent}$label:\n${helpInfo.formatted(level + 1)}" }
            .mkString("\n")
          s"$header\n$cases"
        case HelpInfo.SumTagged(discriminantFlag, cases, default) =>
          val discriminantHints = cases.map(_._1).mkString("|")
          val defaultOrRequired = default.fold("[required]")(d => s"(default: $d)")
          val header = s"${indent}--$discriminantFlag <$discriminantHints> $defaultOrRequired"
          val nonEmptyCases = cases.filter { case (_, helpInfo) =>
            helpInfo match {
              case HelpInfo.Arguments(fields) => fields.nonEmpty
              case _ => true
            }
          }
          val caseDetails = nonEmptyCases
            .map { case (tag, helpInfo) =>
              s"${indent}When --$discriminantFlag=$tag:\n${helpInfo.formatted(level + 1)}"
            }
            .mkString("\n")
          if caseDetails.isEmpty then header else s"$header\n$caseDetails"
      }
    }

    def formatted: String = formatted(0)
  }

  def parse[T: ArgsParser](args: Seq[String]): Result[T] =
    finish[T].tupled(ArgsParser[T].parse(None, untypedParse(args), None))

  def parse[T1: ArgsParser, T2: ArgsParser](args: Seq[String]): Result[(T1, T2)] =
    val (parsedArgs, res1) = ArgsParser[T1].parse(None, untypedParse(args), None)
    val (finalArgs, res2) = ArgsParser[T2].parse(None, parsedArgs, None)
    finish(finalArgs, combine(res1, res2, (_, _)))

  private def finish[T](parsedArgs: ParsedArgs, result: Result[T]): Result[T] =
    (result, parsedArgs.finished) match {
      case (result, None) => result
      case (Right(_), Some(error)) => Left(error)
      case (Left(error), Some(otherError)) => Left(error ++ otherError)
    }

  def serialize[T: ArgsParser](t: T, templateTag: Option[String] = None): Seq[String] =
    ArgsParser[T].serialize(None, t, None, templateTag)

  def help[T: ArgsParser]: String = ArgsParser[T].help(None, None).formatted

  trait Arg[T] {
    def parse(t: String): Option[T]
    final def parseResult(t: String): Result[T] = parse(t).toRight(Error.UnparsableValue(t, hint))
    def serialize(t: T, templateTag: Option[String]): String
    // Return information about what as valid value
    def hint: String

    def withHint(newHint: String): Arg[T] =
      new Arg[T] {
        def parse(t: String): Option[T] = Arg.this.parse(t)
        def serialize(t: T, templateTag: Option[String]): String = Arg.this.serialize(t, templateTag)
        def hint: String = newHint
      }
  }

  object Arg {
    def apply[T: Arg]: Arg[T] = summon

    given Arg[String] with {
      def parse(t: String): Option[String] = Some(t)
      def serialize(t: String, templateTag: Option[String]): String = t
      def hint: String = "string"
    }

    given Arg[Boolean] with {
      def parse(t: String): Option[Boolean] = t.toBooleanOption
      def serialize(t: Boolean, templateTag: Option[String]): String = t.toString
      def hint: String = "true|false"
    }

    given numeric[T: Numeric: SimpleTypeName]: Arg[T] with {
      def parse(t: String): Option[T] = Numeric[T].parseString(t)
      def serialize(t: T, templateTag: Option[String]): String = t.toString
      def hint: String = SimpleTypeName.name[T].toLowerCase
    }

    given date: Arg[Date] with {
      def parse(t: String): Option[Date] = Date.fromIsoString(t)
      def serialize(t: Date, templateTag: Option[String]): String = t.toIsoString
      def hint: String = "YYYY-MM-DD"
    }

    given option[T: Arg]: Arg[Option[T]] with {
      def parse(t: String): Option[Option[T]] = Arg[T].parse(t).map(Some(_))
      def serialize(t: Option[T], templateTag: Option[String]): String =
        t.map(Arg[T].serialize(_, templateTag))
          .getOrElse(
            throw new RuntimeException(
              "Can't serialize None value. For full Option support use a default value of None"
            )
          )
      def hint: String = Arg[T].hint
    }

    given [T: AllSingletons]: Arg[T] with {
      val lookup = AllSingletons[T].map(f => pascalToKebab(f.toString) -> f).toMap
      def parse(t: String): Option[T] = lookup.get(t.toLowerCase)
      def serialize(t: T, templateTag: Option[String]): String = pascalToKebab(t.toString)
      def hint: String = lookup.keys.mkString("|")
    }

    def javaSerializedBase64[T]: Arg[T] =
      new Arg[T] {
        def parse(t: String): Option[T] =
          try Some(
              /* TYPE SAFETY: We catch ClassNotFoundException but will violate type safety if `T` has type
               * parameters. */
              ObjectInputStream(ByteArrayInputStream(Base64.getDecoder.decode(t)))
                .readObject()
                .asInstanceOf[T]
            )
          catch {
            case _: (ClassNotFoundException | InvalidClassException | StreamCorruptedException |
                  OptionalDataException | ClassCastException | IllegalArgumentException) => None
          }
        def serialize(t: T, templateTag: Option[String]): String =
          Base64
            .getEncoder
            .encodeToString(
              ByteArrayOutputStream().tap(os => ObjectOutputStream(os).writeObject(t)).toByteArray
            )
        def hint: String = "BASE64 encoded java serialization of object"
      }
  }

  given source[M, P <: Partitioner]: ArgsParser[Source[M, P]] = {
    given Arg[TestValues[M]] = Arg.javaSerializedBase64[TestValues[M]]
    sumUntagged
  }

  given sink[M, P <: Partitioner]: ArgsParser[Sink[M, P]] = {
    given Arg[TestVerifier[M]] = Arg.javaSerializedBase64[TestVerifier[M]]
    sumUntagged
  }

  private def isAllMissingRequired(error: Error): Boolean =
    error match {
      case Error.MissingRequired(_, _) => true
      case Error.Multiple(errors) => errors.forall(isAllMissingRequired)
      case Error.EnumAllFailed(errors) => errors.forall(isAllMissingRequired)
      case _ => false
    }

  given sourceOption[M, P <: Partitioner](using
      parser: ArgsParser[Source[M, P]]
  ): ArgsParser[Option[Source[M, P]]] =
    new ArgsParser[Option[Source[M, P]]] {
      type S = Option[Source[M, P]]

      private def checkIfEmpty(value: Option[S]) =
        assert(value.flatten == None, "Only None is supported as a default for Option[Source]")

      def parse(path: Option[String], args: ParsedArgs, default: Option[S]): (ParsedArgs, Result[S]) =
        checkIfEmpty(default)
        val (parsedArgs, res) = parser.parse(path, args, default.flatten)
        res match {
          case Left(error) if isAllMissingRequired(error) => (args, Right(None))
          case _ => (parsedArgs, res.map(Some(_)))
        }

      def serialize(
          path: Option[String],
          t: S,
          default: Option[S],
          templateTag: Option[String]
      ): List[String] =
        checkIfEmpty(default)
        t.map(parser.serialize(path, _, default.flatten, templateTag)).getOrElse(Nil)

      def help(path: Option[String], default: Option[S]): HelpInfo = {
        checkIfEmpty(default)
        parser.help(path, default.flatten)
      }
    }

  given optionParser[T: ArgsParser]: ArgsParser[Option[T]] = {
    // Remove $ from `None$` for a clearner api
    given Labelling[Option[T]] = {
      val default = Labelling[Option[T]]
      Labelling[Option[T]](default.label, default.elemLabels.map(_.stripSuffix("$")))
    }
    sumTagged[Option[T]]
  }

  private[table] def flag(path: Option[String], label: String): String = {
    val kebabLabel = camelToKebab(label)
    path.fold(kebabLabel)(p => s"$p-${kebabLabel}")
  }

  enum FieldParser[T] {
    case Simple(arg: Arg[T])
    case Multiple[T](arg: Arg[T]) extends FieldParser[Seq[T]]
    case Nested(parser: ArgsParser[T])
  }
  object FieldParser {
    inline given [T]: FieldParser[T] =
      summonFrom {
        // TYPE SAFETY: The cast is only needed because of https://github.com/scala/scala3/issues/24813
        // the existence of the given makes it safe.
        case _: (T =:= Seq[t]) => FieldParser.Multiple(summonInline[Arg[t]]).asInstanceOf[FieldParser[T]]
        case arg: Arg[T] => FieldParser.Simple(arg)
        case parser: ArgsParser[T] => FieldParser.Nested(parser)
      }
  }

  given product[T](using
      m: Mirror.ProductOf[T],
      labels: Labelling[T],
      defaults: Defaults.Aux[T, m.MirroredElemTypes],
      parserInstances: K0.ProductInstances[FieldParser, T]
  ): ArgsParser[T] =
    new ArgsParser[T] {

      private def effectiveDefaults(maybeOverride: Option[T]): Tuple.Map[m.MirroredElemTypes, Option] =
        maybeOverride match {
          case Some(value) => value.toMirroredElemTypes.map([t] => Option(_))
          case None => defaults.defaults
        }
      def instances(default: Option[T]) =
        parserInstances.zip(productInstances(effectiveDefaults(default))).labelled
      def parse(path: Option[String], args: ParsedArgs, default: Option[T]): (ParsedArgs, Result[T]) =
        instances(default)
          .constructA[StateAp.For[Result, ParsedArgs]] { [t] => labelParserDefault =>
            val (label, (parser, default)) = labelParserDefault
            parser match {
              case FieldParser.Simple(arg) =>
                (args: ParsedArgs) => args.consume(flag(path, label), arg, default)
              case FieldParser.Multiple(arg) =>
                (args: ParsedArgs) => args.consumeAll(flag(path, label), arg, default)
              case FieldParser.Nested(parser) =>
                (args: ParsedArgs) => parser.parse(Some(flag(path, label)), args, default)
            }
          }(summon, summon, summon)
          .run(args)
      def serialize(
          path: Option[String],
          t: T,
          default: Option[T],
          templateTag: Option[String]
      ): List[String] =
        instances(default).foldLeft(t)(List.empty[String]) { [t] => (acc, labelParserDefault, value) =>
          val (label, (parser, default)) = labelParserDefault
          if default.contains(value) then acc
          else {
            val inner = parser match {
              case FieldParser.Simple(arg) =>
                List(s"--${flag(path, label)}", arg.serialize(value, templateTag))
              case FieldParser.Multiple(arg) =>
                if value.isEmpty then
                  throw new RuntimeException(
                    "Can't serialize empty Seq value without a empty default, since it can not be read back correctly."
                  )
                value.flatMap(v => List(s"--${flag(path, label)}", arg.serialize(v, templateTag))).toList
              case FieldParser.Nested(p) => p.serialize(Some(flag(path, label)), value, default, templateTag)
            }
            acc ++ inner
          }
        }
      def help(path: Option[String], default: Option[T]): HelpInfo = {
        val fieldHelps =
          instances(default).foldLeft0[Vector[HelpInfo]](Vector.empty) { [t] => (acc, labelParserDefault) =>
            val (label, (fieldParser, defaultOpt)) = labelParserDefault
            val helpForField = fieldParser match {
              case FieldParser.Simple(arg) =>
                val defaultInfo = defaultOpt match {
                  case Some(None) => DefaultInfo.EmptyDefault
                  case Some(value) => DefaultInfo.Value(arg.serialize(value, None))
                  case None => DefaultInfo.NoDefault
                }
                HelpInfo.Arg(flag(path, label), arg.hint, defaultInfo, multiple = false)
              case FieldParser.Multiple(arg) =>
                val defaultInfo = defaultOpt match {
                  case Some(Seq()) => DefaultInfo.EmptyDefault
                  case Some(values) => DefaultInfo.Value(values.map(arg.serialize(_, None)).mkString(" "))
                  case None => DefaultInfo.NoDefault
                }
                HelpInfo.Arg(flag(path, label), arg.hint, defaultInfo, multiple = true)
              case FieldParser.Nested(parser) => parser.help(Some(flag(path, label)), defaultOpt)
            }
            acc :+ helpForField
          }
        HelpInfo.Arguments(fieldHelps)
      }
    }

  /** An ArgsParser for sum types that tries all parsers and succeeds if only
    * one case succeed.
    *
    * Since a default value would effectively make the parser always succeed for
    * that case, default values are not supported for this parser.
    *
    * Note: There is no automatic check that the arguments names do not overlap
    * and that this parser will work in all cases. So it up to a user to ensure
    * it valid for that particular class.
    */
  def sumUntagged[S: Mirror.SumOf as m](using
      instances: K0.CoproductInstances[ArgsParser, S],
      labels: Labelling[S]
  ): ArgsParser[S] =
    new ArgsParser[S] {
      def checkDefault(default: Option[S]): Unit =
        assert(default == None, "Default values are not supported for sum types")

      def parse(path: Option[String], args: ParsedArgs, default: Option[S]): (ParsedArgs, Result[S]) = {
        checkDefault(default)
        val parsed = (0 until instances.arity).map(i =>
          instances.inject(i)([t <: S] => _.parse(path, args, None))
        )
        val succeeded = parsed.filter(_._2.isRight)
        if succeeded.size == 1 then succeeded.head
        else {
          val error =
            if succeeded.size > 1 then Error.EnumAmbiguous(succeeded.flatMap(_._2.toOption.map(_.toString)))
            else Error.EnumAllFailed(parsed.flatMap(_._2.left.toOption))
          (args, Left(error))
        }
      }
      def serialize(
          path: Option[String],
          t: S,
          maybeDefault: Option[S],
          templateTag: Option[String]
      ): List[String] = {
        checkDefault(maybeDefault)
        instances.fold(t)([t <: S] => _.serialize(path, _, None, templateTag))
      }
      def help(path: Option[String], default: Option[S]): HelpInfo = {
        checkDefault(default)
        val alternatives = labels
          .elemLabels
          .zipWithIndex
          .map { case (label, index) =>
            val caseHelp = instances.inject(index)([t <: S] => _.help(path, None))
            (label, caseHelp)
          }
        HelpInfo.SumUntagged(path, alternatives)
      }
    }

  /** An ArgsParser for sum types that uses a tag argument to determine which
    * case to parse.
    *
    * If used as a top-level parser, the tag argument will be
    * `--discriminant <tag>`, where `<tag>` is the kebab-case version of the
    * case class name. If used within a product parser, the tag argument will be
    * `--<path> <tag>`, where `<path>` is the kebab-case version of the field
    * path.
    */
  def sumTagged[S: Mirror.SumOf as m](using
      instances: K0.CoproductInstances[ArgsParser, S],
      labels: Labelling[S]
  ): ArgsParser[S] =
    new ArgsParser[S] {
      val lookup: Map[String, Int] = labels.elemLabels.map(pascalToKebab).zipWithIndex.toMap
      val tagArg = new Arg[Int] {
        def parse(t: String): Option[Int] = lookup.get(t)
        def serialize(t: Int, templateTag: Option[String]): String = t.toString
        def hint: String = lookup.keys.mkString("|")
      }
      def tagKey(path: Option[String]): String = path.getOrElse("discriminant")
      def tagValue(t: S): String =
        instances.labelled.fold(t)([t <: S] => (labelAndParser, _) => pascalToKebab(labelAndParser._1))
      def tagIndex(t: S): Int = lookup(tagValue(t))
      def parse(path: Option[String], args: ParsedArgs, maybeDefault: Option[S]): (ParsedArgs, Result[S]) = {
        val (argsAfterTag, tagResult) = args.consume(tagKey(path), tagArg, maybeDefault.map(tagIndex))
        (tagResult, maybeDefault) match {
          case (Left(err), _) => (args, Left(err))
          case (Right(index), Some(default)) if tagIndex(default) == index =>
            instances.fold(default)([t <: S] =>
              (parser, default) => parser.parse(path, argsAfterTag, Some(default))
            )
          case (Right(index), _) => instances.inject(index)([t <: S] => _.parse(path, argsAfterTag, None))
        }
      }
      def serialize(
          path: Option[String],
          t: S,
          maybeDefault: Option[S],
          templateTag: Option[String]
      ): List[String] = {
        def serializedNoDefault =
          instances.fold(t)([t <: S] => (parser, value) => parser.serialize(path, value, None, templateTag))
        val inner = maybeDefault match {
          case Some(default) => instances.fold2(t, default)(serializedNoDefault)([t <: S] =>
              (parser, value, defValue) => parser.serialize(path, value, Some(defValue), templateTag)
            )
          case None => serializedNoDefault
        }
        if maybeDefault.map(tagValue).contains(tagValue(t)) then inner
        else s"--${tagKey(path)}" :: tagValue(t) :: inner
      }
      def help(path: Option[String], default: Option[S]): HelpInfo = {
        val cases = labels
          .elemLabels
          .zipWithIndex
          .map { case (label, index) =>
            val caseHelp = default match {
              case Some(d) if tagValue(d) == pascalToKebab(label) =>
                instances.fold(d)([t <: S] => (parser, value) => parser.help(path, Some(value)))
              case _ => instances.inject(index)([t <: S] => _.help(path, None))
            }
            (pascalToKebab(label), caseHelp)
          }
        val defaultTag = default.map(tagValue)
        HelpInfo.SumTagged(tagKey(path), cases, defaultTag)
      }
    }

  private def untypedParse(args: Seq[String]): ParsedArgs = {
    def stripDashes(s: String): String = s.stripPrefix("--")

    @tailrec
    def processArgs(
        remaining: List[String],
        keyValues: List[(String, String)],
        errors: List[Error]
    ): ParsedArgs =
      remaining match
        case Nil => ParsedArgs(keyValues.reverse.groupMap(_._1)(_._2), errors)

        // Handle --key=value format
        case arg :: rest if arg.startsWith("--") && arg.contains("=") =>
          val (flag, equalsAndValue) = arg.splitAt(arg.indexOf("="))
          val key = stripDashes(flag)
          val value = equalsAndValue.stripPrefix("=")
          processArgs(rest, (key -> value) :: keyValues, errors)

        // Handle --key value format
        case arg :: value :: rest if arg.startsWith("--") =>
          val key = stripDashes(arg)
          processArgs(rest, (key -> value) :: keyValues, errors)

        // Missing value for key
        case arg :: rest if arg.startsWith("--") =>
          processArgs(rest, keyValues, Error.MissingValue(stripDashes(arg)) :: errors)

        case invalid :: rest => processArgs(rest, keyValues, Error.UnparsedFlag(invalid) :: errors)

    processArgs(args.toList, List(), List())
  }

  private def pascalToKebab(s: String): String = s.head.toLower +: camelToKebab(s.tail)

  private def camelToKebab(s: String): String =
    s.foldLeft("")((acc, c) => if (c.isUpper) acc + "-" + c.toLower else acc + c)

  private[table] def combine[T1, T2, R](res1: Result[T1], res2: Result[T2], f: (T1, T2) => R): Result[R] =
    (res1, res2) match {
      case (Right(v1), Right(v2)) => Right(f(v1, v2))
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)
    }

  inline def derived[T](using m: Mirror.Of[T]): ArgsParser[T] =
    inline m match {
      case given Mirror.SumOf[T] => sumTagged
      case m: Mirror.ProductOf[T] => product[T](using m, summonInline, summonInline, summonInline)
    }
}
