package com.choreograph.tyda.table

import java.util.Base64

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag
import scala.util.Random

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Format
import com.choreograph.tyda.TypeName
import com.choreograph.tyda.table.ArgsParser.Error

object ArgsParserSpec {
  enum Case {
    case LowerCase
    case UpperCase
  }
  enum CaseOptions derives ArgsParser {
    case Flags1(a: Int, b: String)
    case Flags2(c: Boolean, d: String)
  }
  final case class Args1(arg1: Int, arg2: String)
  final case class ArgsInt(arg1: Int)
  final case class ArgsLong(arg1: Long)
  final case class ArgsShort(arg1: Short)
  final case class ArgsByte(arg1: Byte)
  final case class ArgsFloat(arg1: Float)
  final case class ArgsDouble(arg1: Double)
  final case class ArgsBoolean(arg1: Boolean)
  final case class ArgsEnum(arg1: Case)
  final case class ArgsEnumOptions(arg1: CaseOptions) derives ArgsParser
  object ArgsEnumOptions {
    given ArgsParser[CaseOptions] = ArgsParser.sumUntagged
  }
  final case class ArgsTaggedEnumOptions(arg1: CaseOptions) derives ArgsParser
  final case class ArgsTaggedEnumOptionsWithDefault(arg1: CaseOptions = CaseOptions.Flags1(1, "a"))
      derives ArgsParser
  final case class ArgsWithDate(date: String)
  final case class ArgsWithDefault(arg1: Int, arg2: String = "default")
  final case class ArgsWithOption(arg1: Int, arg2: Option[Int])
  final case class ArgsWithOptionAndNoneDefault(arg1: Int, arg2: Option[Int] = None)
  final case class ArgsWithOptionAndDefault(arg1: Int, arg2: Option[String] = Some("default"))
  final case class ArgsWithOptionProduct(arg1: Int, arg2: Option[Args1] = None)
  final case class ArgsWithLongNames(nameWithMultipleWords: String)
  final case class ArgsWithSeq(arg1: Seq[Int])
  final case class ArgsWithSeqDefault(arg1: Seq[Int] = Seq(1, 2, 3))
  final case class ArgsWithSeqEmptyDefault(int: Int, arg1: Seq[Int] = Seq.empty)
  final case class ArgsWithSeqAndInt(int: Int, arg1: Seq[Int])
  final case class NestedArgs(group1: Args1, group2: ArgsWithDefault)
  final case class NestedArgs2(prefix: NestedArgs)
  final case class NestedOverridingDefaults(group1: ArgsWithDefault = ArgsWithDefault(1, "new default"))
  final case class DuplicatedArgs(prefix: NestedArgs, `prefix-group1-arg1`: Int)
  final case class DuplicatedArgsReversed(`prefix-group1-arg1`: Int, prefix: NestedArgs)
  final case class AllDefaultArgs(int: Int = 1)
  final case class ArgsWithAllDefaultArgsAlone(args: AllDefaultArgs)

  final case class Model()
  final case class ArgsWithSource(source: Source[Model, Partitioner.None])
  final case class ArgsWithOptionSource(source: Option[Source[Model, Partitioner.None]])
  final case class ArgsWithSink(sink: Sink[Model, Partitioner.None])

  final case class ArgsWithCustomTemplate(arg: OpaqueArgWithCustomTemplate)
  final case class DeeplyNestedArgsWithCustomTemplate(
      args: ArgsWithCustomTemplate,
      int: Int,
      arg: OpaqueArgWithCustomTemplate
  )

  opaque type OpaqueArgWithCustomTemplate = String

  object OpaqueArgWithCustomTemplate {
    val customTemplate = "CustomTemplate"

    def apply(value: String): OpaqueArgWithCustomTemplate = value

    given Arbitrary[OpaqueArgWithCustomTemplate] = Arbitrary.string

    given ArgsParser.Arg[OpaqueArgWithCustomTemplate] with {
      def hint: String = "Custom template args"
      def parse(value: String): Option[OpaqueArgWithCustomTemplate] = Some(value)
      def serialize(value: OpaqueArgWithCustomTemplate, templateTag: Option[String]): String =
        if templateTag.contains("custom") then customTemplate else value
    }
  }

  /** In general function object are not comparble, so we use a singleton that
    * is the same after java serialization
    */
  object DummyVerifier extends Function1[Any, Any] {
    def apply(v: Any): Any = ()
  }
}

class ArgsParserSuite extends AnyFunSuite {
  import ArgsParserSpec.*

  private val boolHint = ArgsParser.Arg[Boolean].hint
  private val intHint = ArgsParser.Arg[Int].hint
  private val longHint = ArgsParser.Arg[Long].hint
  private val shortHint = ArgsParser.Arg[Short].hint
  private val byteHint = ArgsParser.Arg[Byte].hint
  private val floatHint = ArgsParser.Arg[Float].hint
  private val doubleHint = ArgsParser.Arg[Double].hint
  private val optionIntHint = ArgsParser.Arg[Option[Int]].hint
  private val stringHint = ArgsParser.Arg[String].hint

  private def checkParseAndSerialize[T: ArgsParser](args: Seq[String], expected: T) = {
    assert(ArgsParser.parse[T](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support basic args") {
    assert(ArgsParser.parse[Args1](Seq("--arg1", "1", "--arg2", "2")) == Right(Args1(1, "2")))
  }

  test("support using --flag=value") {
    assert(ArgsParser.parse[Args1](Seq("--arg1=1", "--arg2=2")) == Right(Args1(1, "2")))
  }

  test("support boolean args") {
    val args = Seq("--arg1", "true")
    val expected = ArgsBoolean(true)
    assert(ArgsParser.parse[ArgsBoolean](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("no implicit false for boolean flags") {
    assert(ArgsParser.parse[ArgsBoolean](Seq()) == Left(Error.MissingRequired("arg1", boolHint)))
  }

  test("no implicit special support for boolean flags") {
    assert(
      ArgsParser.parse[ArgsBoolean](Seq("--arg1")) ==
        Left(Error.MissingRequired("arg1", boolHint) ++ Error.MissingValue("arg1"))
    )
  }

  test("support enum args") { checkParseAndSerialize(Seq("--arg1", "upper-case"), ArgsEnum(Case.UpperCase)) }

  private def checkUnparsable[T: ArgsParser](value: String, hint: String) =
    assert(ArgsParser.parse[T](Seq("--arg1", value)) == Left(Error.UnparsableValue(value, hint)))

  test("return error for int overflow") {
    checkUnparsable[ArgsInt]((Int.MaxValue.toLong + 1).toString, intHint)
  }
  test("return error for unparsable long") { checkUnparsable[ArgsLong]("not-a-long", longHint) }
  test("return error for short overflow") { checkUnparsable[ArgsShort]("99999", shortHint) }
  test("return error for byte overflow") { checkUnparsable[ArgsByte]("999", byteHint) }
  test("return error for unparsable float") { checkUnparsable[ArgsFloat]("not-a-float", floatHint) }
  test("return error for unparsable double") { checkUnparsable[ArgsDouble]("not-a-double", doubleHint) }

  test("float NaN parses and serializes") {
    ArgsParser.parse[ArgsFloat](Seq("--arg1", "NaN")) match {
      case Right(ArgsFloat(v)) =>
        assert(v.isNaN)
        assert(ArgsParser.serialize(ArgsFloat(v)) == Seq("--arg1", "NaN"))
      case other => fail(s"Expected Right with NaN, got $other")
    }
  }

  test("double NaN parses and serializes") {
    ArgsParser.parse[ArgsDouble](Seq("--arg1", "NaN")) match {
      case Right(ArgsDouble(v)) =>
        assert(v.isNaN)
        assert(ArgsParser.serialize(ArgsDouble(v)) == Seq("--arg1", "NaN"))
      case other => fail(s"Expected Right with NaN, got $other")
    }
  }

  test("support default values") { checkParseAndSerialize(Seq("--arg1", "1"), ArgsWithDefault(1, "default")) }

  test("no implicit default of None for Option") {
    assert(
      ArgsParser.parse[ArgsWithOption](Seq("--arg1", "1")) ==
        Left(Error.MissingRequired("arg2", optionIntHint))
    )
  }

  test("serialize Options as underlying value") {
    checkParseAndSerialize(Seq("--arg1", "1", "--arg2", "2"), ArgsWithOption(1, Some(2)))
  }

  test("do not support serializing None when it's not default") {
    val e = intercept[RuntimeException] { ArgsParser.serialize(ArgsWithOption(1, None)) }
    assert(
      e.getMessage.contains("Can't serialize None value. For full Option support use a default value of None")
    )
  }

  test("not serialize Option with default") {
    assert(ArgsParser.serialize(ArgsWithOptionAndDefault(1)) == List("--arg1", "1"))
  }

  test("support Option of products as tagged sum type out of the box") {
    val someArgs = ArgsWithOptionProduct(1, Some(Args1(42, "hello")))
    val someFlags =
      Seq("--arg1", "1", "--arg2", "some", "--arg2-value-arg1", "42", "--arg2-value-arg2", "hello")
    checkParseAndSerialize(someFlags, someArgs)

    val noneArgs = ArgsWithOptionProduct(1, None)
    val noneFlags = Seq("--arg1", "1")
    checkParseAndSerialize(noneFlags, noneArgs)
  }

  test("not serialize default values") {
    val args = ArgsWithDefault(1, "default")
    assert(ArgsParser.serialize(args) == List("--arg1", "1"))
  }

  test("long names are split by capital letters") {
    checkParseAndSerialize(Seq("--name-with-multiple-words", "value"), ArgsWithLongNames("value"))
  }

  test("support args with Seq") {
    checkParseAndSerialize(
      Seq("--arg1", "1", "--arg1", "2", "--arg1", "3", "--arg1", "4"),
      ArgsWithSeq(Vector(1, 2, 3, 4))
    )
  }

  test("support non empty default values") { checkParseAndSerialize(Seq(), ArgsWithSeqDefault()) }

  test("do not serialize empty value when it can not be read back") {
    def mustThrow(f: => Any) = {
      val e = intercept[RuntimeException](f)
      assert(e.getMessage.contains("Can't serialize empty Seq"))
    }
    mustThrow(ArgsParser.serialize(ArgsWithSeqDefault(Seq())))
    mustThrow(ArgsParser.serialize(ArgsWithSeq(Seq())))
  }

  test("no implicit default for Seq.empty") {
    assert(ArgsParser.parse[ArgsWithSeq](Seq()) == Left(Error.MissingRequired("arg1", intHint)))
  }

  test("support args with empty Seq default and int") {
    checkParseAndSerialize(Seq("--int", "1"), ArgsWithSeqEmptyDefault(1, Vector()))
  }

  test("support args with Seq and int in mixed order") {
    val args = Seq("--arg1", "1", "--int", "1", "--arg1", "2", "--arg1", "3", "--arg1", "4")
    val expected = ArgsWithSeqAndInt(1, Vector(1, 2, 3, 4))
    assert(ArgsParser.parse[ArgsWithSeqAndInt](args) == Right(expected))
  }

  test("nesting args adds prefix to flags") {
    assert(
      ArgsParser.parse[NestedArgs](Seq("--group1-arg1", "1", "--group1-arg2", "2", "--group2-arg1", "3")) ==
        Right(NestedArgs(Args1(1, "2"), ArgsWithDefault(3, "default")))
    )
  }

  test("support multiple levels of nesting") {
    assert(
      ArgsParser.parse[NestedArgs2](
        Seq("--prefix-group1-arg1", "1", "--prefix-group1-arg2", "2", "--prefix-group2-arg1", "3")
      ) == Right(NestedArgs2(NestedArgs(Args1(1, "2"), ArgsWithDefault(3, "default"))))
    )
  }

  test("not require specifying class with defaults for all args") {
    assert(
      ArgsParser.parse[ArgsWithAllDefaultArgsAlone](Seq()) ==
        Right(ArgsWithAllDefaultArgsAlone(AllDefaultArgs()))
    )
  }

  test("support overriding defaults when using nested args") {
    checkParseAndSerialize(Seq(), NestedOverridingDefaults(ArgsWithDefault(1, "new default")))
    checkParseAndSerialize(
      Seq("--group1-arg1", "2"),
      NestedOverridingDefaults(ArgsWithDefault(2, "new default"))
    )
    checkParseAndSerialize(
      Seq("--group1-arg1", "3", "--group1-arg2", "value"),
      NestedOverridingDefaults(ArgsWithDefault(3, "value"))
    )
  }

  test("give error when it ambiguous which case to use") {
    val arg1 = ArgsEnumOptions(CaseOptions.Flags1(1, "2"))
    val arg2 = ArgsEnumOptions(CaseOptions.Flags2(true, "2"))
    val args = ArgsParser.serialize(arg1) ++ ArgsParser.serialize(arg2)
    ArgsParser.parse[ArgsEnumOptions](args) match {
      case Left(Error.Multiple(errors)) => assert(errors.exists {
          case Error.EnumAmbiguous(_) => true
          case _ => false
        })
      case other => fail(s"Expected Left(Error.Multiple with EnumAmbiguous), got $other")
    }
  }

  test("give error when no case matches") {
    val args = Seq()
    assert(
      ArgsParser.parse[ArgsEnumOptions](args) == Left(Error.EnumAllFailed(Seq(
        Error.Multiple(
          Seq(Error.MissingRequired("arg1-a", intHint), Error.MissingRequired("arg1-b", stringHint))
        ),
        Error.Multiple(
          Seq(Error.MissingRequired("arg1-c", boolHint), Error.MissingRequired("arg1-d", stringHint))
        )
      )))
    )
  }

  test("tagged sum give error for flag from wrong case") {
    val args = Seq("--arg1", "flags1", "--arg1-a", "1", "--arg1-b", "2", "--arg1-d", "value")
    assert(ArgsParser.parse[ArgsTaggedEnumOptions](args) == Left(Error.UnusedFlag("arg1-d", "value")))
  }

  test("tagged sum give error when tag is missing") {
    val args = Seq()
    assert(
      ArgsParser.parse[ArgsTaggedEnumOptions](args) == Left(Error.MissingRequired("arg1", "flags1|flags2"))
    )
  }

  test("tagged sum with default empty") {
    val args = Seq()
    val default = ArgsTaggedEnumOptionsWithDefault()
    assert(ArgsParser.parse[ArgsTaggedEnumOptionsWithDefault](args) == Right(default))
    assert(ArgsParser.serialize(default) == Seq())
  }

  test("tagged sum with default no tag") {
    val args = Seq("--arg1-a", "2", "--arg1-b", "value")
    val expected = ArgsTaggedEnumOptionsWithDefault(CaseOptions.Flags1(2, "value"))
    assert(ArgsParser.parse[ArgsTaggedEnumOptionsWithDefault](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support reading Source.Path") {
    val args = Seq("--source-base-path", "/tmp")
    val expected = ArgsWithSource(Source.Path("/tmp"))
    assert(ArgsParser.parse[ArgsWithSource](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support reading Source.Path with format") {
    val args = Seq("--source-base-path", "/tmp", "--source-format", "json")
    val expected = ArgsWithSource(Source.Path("/tmp", Format.Json))
    assert(ArgsParser.parse[ArgsWithSource](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support reading Option[Source]") {
    val args = Seq("--source-base-path", "/tmp")
    val expected = ArgsWithOptionSource(Some(Source.Path("/tmp")))
    assert(ArgsParser.parse[ArgsWithOptionSource](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support reading Option[Source] if empty") {
    val args = Seq()
    val expected = ArgsWithOptionSource(None)
    assert(ArgsParser.parse[ArgsWithOptionSource](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support duplicated args") {
    val args = Seq(
      "--prefix-group1-arg1",
      "1",
      "--prefix-group1-arg2",
      "2",
      "--prefix-group2-arg1",
      "3",
      "--prefix-group1-arg1",
      "4"
    )
    val expected = DuplicatedArgs(NestedArgs(Args1(1, "2"), ArgsWithDefault(3, "default")), 4)
    assert(ArgsParser.parse[DuplicatedArgs](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support reading Sink.Path") {
    val args = Seq("--sink-base-path", "/tmp")
    val expected = ArgsWithSink(Sink.Path("/tmp"))
    assert(ArgsParser.parse[ArgsWithSink](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  test("support reading Sink.Path with format") {
    val args = Seq("--sink-base-path", "/tmp", "--sink-format", "json")
    val expected = ArgsWithSink(Sink.Path("/tmp", Format.Json))
    assert(ArgsParser.parse[ArgsWithSink](args) == Right(expected))
    assert(ArgsParser.serialize(expected) == args)
  }

  private def roundTrip[T: ArgsParser: Arbitrary: TypeName]() = {
    test(s"Round trip ${TypeName.name[T]}") {
      val values = Seq.fill(10)(Arbitrary[T]())
      values.foreach { value => assert(ArgsParser.parse[T](ArgsParser.serialize(value)) == Right(value)) }
    }
  }

  roundTrip[Args1]()
  roundTrip[ArgsBoolean]()
  roundTrip[ArgsLong]()
  roundTrip[ArgsShort]()
  roundTrip[ArgsByte]()
  {
    given Arbitrary[Float] = Arbitrary[Float].filter(!_.isNaN)
    roundTrip[ArgsFloat]()
  }
  {
    given Arbitrary[Double] = Arbitrary[Double].filter(!_.isNaN)
    roundTrip[ArgsDouble]()
  }
  roundTrip[ArgsEnum]()
  roundTrip[ArgsEnumOptions]()
  roundTrip[ArgsTaggedEnumOptions]()
  roundTrip[ArgsTaggedEnumOptionsWithDefault]()
  roundTrip[CaseOptions]()
  roundTrip[ArgsWithDefault]()
  roundTrip[ArgsWithLongNames]()
  roundTrip[ArgsWithOptionProduct]()
  roundTrip[DuplicatedArgs]()
  roundTrip[DuplicatedArgsReversed]()
  roundTrip[ArgsWithSeqEmptyDefault]()
  {
    // Without a default value empty Seq does not roundtrip
    given Arbitrary[Seq[Int]] = Arbitrary.iterable[Int, Seq[Int]].filter(_.nonEmpty)
    roundTrip[ArgsWithSeq]()
    roundTrip[ArgsWithSeqDefault]()
    roundTrip[ArgsWithSeqAndInt]()
  }
  roundTrip[NestedArgs]()
  roundTrip[NestedArgs2]()
  roundTrip[NestedOverridingDefaults]()
  roundTrip[ArgsWithAllDefaultArgsAlone]()
  roundTrip[ArgsWithCustomTemplate]()
  roundTrip[DeeplyNestedArgsWithCustomTemplate]()
  {
    // Note we explicitly use ArraySeq for all Source.Test cases to avoid serialization
    // issues caused by https://github.com/scala/bug/issues/9237
    given seq[T: Arbitrary: ClassTag]: Arbitrary[Seq[T]] = Arbitrary[ArraySeq[T]].map(identity)
    // The default Map special cases for size up to 4 that does not run into the serialization issue
    given map[K: Arbitrary, V: Arbitrary]: Arbitrary[Map[K, V]] = Arbitrary.seqN[(K, V)](4).map(_.toMap)
    given verifier[T]: Arbitrary[Seq[T] => Any] = Arbitrary.oneOf(DummyVerifier)
    roundTrip[ArgsWithSource]()
    roundTrip[ArgsWithSink]()
  }

  test("return error for missing required") {
    assert(ArgsParser.parse[Args1](Seq("--arg2", "2")) == Left(Error.MissingRequired("arg1", intHint)))
  }

  test("return all missing required") {
    assert(
      ArgsParser.parse[Args1](Seq()) ==
        Left(Error.MissingRequired("arg1", intHint) ++ Error.MissingRequired("arg2", stringHint))
    )
  }

  test("return all missing required and invalid") {
    assert(
      ArgsParser.parse[Args1](Seq("--arg2", "2", "-df")) ==
        Left(Error.MissingRequired("arg1", intHint) ++ Error.UnparsedFlag("-df"))
    )
  }

  test("return error for unparsable value") {
    assert(
      ArgsParser.parse[Args1](Seq("--arg1", "a", "--arg2", "2")) == Left(Error.UnparsableValue("a", intHint))
    )
  }

  test("return error for invalid flag") {
    assert(
      ArgsParser.parse[ArgsWithDefault](Seq("--arg1", "1", "-arg2")) == Left(Error.UnparsedFlag("-arg2"))
    )
  }

  test("support parsing mutiple args classes from the same command line") {
    val args1 = Seq("--arg1", "1", "--arg2", "2")
    val argsDate = Seq("--date", "0")
    assert(
      ArgsParser.parse[Args1, ArgsWithDate](args1 ++ argsDate) == Right((Args1(1, "2"), ArgsWithDate("0")))
    )
    assert(ArgsParser.parse[Args1, ArgsWithDate](args1) == Left(Error.MissingRequired("date", stringHint)))
    assert(
      ArgsParser.parse[Args1, ArgsWithDate](argsDate) ==
        Left(Error.MissingRequired("arg1", intHint) ++ Error.MissingRequired("arg2", stringHint))
    )
  }

  test("parse args in order when using multiple args classes") {
    val args = Seq("--arg1", "1", "--arg2", "", "--arg1", "true")
    assert(ArgsParser.parse[Args1, ArgsBoolean](args) == Right((Args1(1, ""), ArgsBoolean(true))))
  }

  test("formatted should produce human readable error") {
    val err = Error.MissingRequired("key", stringHint) ++ Error.UnparsableValue("key2", intHint) ++
      Error.MissingValue("key3") ++ Error.UnparsedFlag("-key4") ++ Error.UnusedFlag("key5", "val5")
    assert(err.formatted == """Missing required argument: --key with value hint: string
                         |Unparsable value: key2 with value hint int
                         |Missing value for argument: --key3
                         |Unparsed flag: -key4
                         |Unused flag: key5 with value val5""".stripMargin)
  }

  test("should indent enum all failed errors") {
    val missingRequired1 = Error.MissingRequired("arg", intHint)
    val missingRequired2 = Error.MissingRequired("arg2", intHint)
    val unparsable = Error.UnparsableValue("arg3", stringHint)
    val err = Error.EnumAllFailed(
      Seq(Error.Multiple(Seq(missingRequired1, missingRequired2)), Error.Multiple(Seq(unparsable)))
    )
    assert(err.formatted == s"""Failed to parse enum. The errors were:
                         |  ${missingRequired1.formatted}
                         |  ${missingRequired2.formatted}
                         |Or
                         |  ${unparsable.formatted}""".stripMargin)
  }

  test("support custom template serialization") {
    assert(
      ArgsParser.serialize(ArgsWithCustomTemplate(OpaqueArgWithCustomTemplate("value")), Some("custom")) ==
        List("--arg", OpaqueArgWithCustomTemplate.customTemplate)
    )
  }

  test("support deeply nested custom template serialization") {
    assert(
      ArgsParser.serialize(
        DeeplyNestedArgsWithCustomTemplate(
          ArgsWithCustomTemplate(OpaqueArgWithCustomTemplate("value")),
          1,
          OpaqueArgWithCustomTemplate("value2")
        ),
        Some("custom")
      ) == List(
        "--args-arg",
        OpaqueArgWithCustomTemplate.customTemplate,
        "--int",
        "1",
        "--arg",
        OpaqueArgWithCustomTemplate.customTemplate
      )
    )
  }

  test("javaSerializedBase64 should never throw") {
    val argInt = ArgsParser.Arg.javaSerializedBase64[Int]
    val argTuple = ArgsParser.Arg.javaSerializedBase64[(Int, String)]
    val _ = argInt.parse(argTuple.serialize(Arbitrary[(Int, String)](), None))
    (0 to 100).foreach { _ =>
      val _ = argInt.parse(Random.nextString(10))
      val _ = argInt.parse(Base64.getEncoder.encodeToString(Random.nextBytes(100)))
    }
  }

  test("help for simple args") {
    val help = ArgsParser.help[Args1]
    val expected = """|--arg1 <int> [required]
                      |--arg2 <string> [required]""".stripMargin
    assert(help == expected)
  }

  test("help for args with defaults") {
    val help = ArgsParser.help[ArgsWithDefault]
    val expected = """|--arg1 <int> [required]
                      |--arg2 <string> (default: default)""".stripMargin
    assert(help == expected)
  }

  test("help for nested args") {
    val help = ArgsParser.help[NestedArgs]
    val expected = """|--group1-arg1 <int> [required]
                      |--group1-arg2 <string> [required]
                      |--group2-arg1 <int> [required]
                      |--group2-arg2 <string> (default: default)""".stripMargin
    assert(help == expected)
  }

  test("help for tagged enum options") {
    val help = ArgsParser.help[ArgsTaggedEnumOptions]
    val expected = """|--arg1 <flags1|flags2> [required]
                      |When --arg1=flags1:
                      |  --arg1-a <int> [required]
                      |  --arg1-b <string> [required]
                      |When --arg1=flags2:
                      |  --arg1-c <true|false> [required]
                      |  --arg1-d <string> [required]""".stripMargin
    assert(help == expected)
  }

  test("help for seq args") {
    val help = ArgsParser.help[ArgsWithSeq]
    val expected = """--arg1 <int> [required, multiple]"""
    assert(help == expected)
  }

  test("help for seq with default") {
    val help = ArgsParser.help[ArgsWithSeqDefault]
    val expected = """--arg1 <int> [multiple] (default: 1 2 3)"""
    assert(help == expected)
  }

  test("help for nested args with overridden defaults") {
    val help = ArgsParser.help[NestedOverridingDefaults]
    val expected = """|--group1-arg1 <int> (default: 1)
                      |--group1-arg2 <string> (default: new default)""".stripMargin
    assert(help == expected)
  }

  test("help for args with option") {
    val help = ArgsParser.help[ArgsWithOption]
    val expected = """|--arg1 <int> [required]
                      |--arg2 <int> [required]""".stripMargin
    assert(help == expected)
  }

  test("help for args with option product") {
    val help = ArgsParser.help[ArgsWithOptionProduct]
    val expected = """|--arg1 <int> [required]
                      |--arg2 <none|some> (default: none)
                      |When --arg2=some:
                      |  --arg2-value-arg1 <int> [required]
                      |  --arg2-value-arg2 <string> [required]""".stripMargin
    assert(help == expected)
  }

  test("help for args with option with None default") {
    val help = ArgsParser.help[ArgsWithOptionAndNoneDefault]
    val expected = """|--arg1 <int> [required]
                      |--arg2 <int>""".stripMargin
    assert(help == expected)
  }

  test("help for args with seq with empty default") {
    val help = ArgsParser.help[ArgsWithSeqEmptyDefault]
    val expected = """|--int <int> [required]
                      |--arg1 <int> [multiple]""".stripMargin
    assert(help == expected)
  }

  test("help for args with source") {
    val help = ArgsParser.help[ArgsWithSource]
    val expected = """|Alternative --source one of:
                      |Path:
                      |  --source-base-path <string> [required]
                      |  --source-format <parquet|json> (default: parquet)
                      |  --source-unpivot <true|false> (default: false)
                      |  --source-filename-glob-filter <string> (default: *.parquet)
                      |  --source-numerics-read-mode <exact|widen-big-query> (default: exact)
                      |Table:
                      |  --source-identifier <string> [required]
                      |  --source-location <native|big-query> (default: native)
                      |Test:
                      |  --source-data <BASE64 encoded java serialization of object> [required]
                      |  --source-metadata-file_path <string> [required]""".stripMargin
    assert(help == expected)
  }
}
