package com.choreograph.tyda.testsuites

import java.io.File

import scala.reflect.Typeable
import scala.util.Random
import scala.util.Using

import org.apache.commons.io.FileUtils
import org.scalactic.Equality
import org.scalatest.enablers.Aggregating
import shapeless3.deriving.Labelling

import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Binary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.EnumStableHashCode
import com.choreograph.tyda.Format
import com.choreograph.tyda.NumericsReadMode
import com.choreograph.tyda.Ord
import com.choreograph.tyda.PartitionEncoding.encode
import com.choreograph.tyda.Runner
import com.choreograph.tyda.Timestamp
import com.choreograph.tyda.TypeName

object DatasetReadWriteSuite {
  final case class TmpPath(path: String, cleanup: () => Unit)
  object TmpPath {
    def apply(tmpDir: String, cleanup: String => Unit): TmpPath = {
      val uniqueId = java.util.UUID.randomUUID().toString
      val path = s"${tmpDir.stripSuffix("/")}/dataset-read-write-$uniqueId"
      new TmpPath(path, () => cleanup(path))
    }
    given Using.Releasable[TmpPath] = tmp => tmp.cleanup()
  }
  private final case class MyProduct(a: Option[Long], b: String, c: (Long, Option[Long]))
      derives Arbitrary, Codec
  private enum MyEnum extends EnumStableHashCode derives Arbitrary, Codec {
    case A, B
    case C(a: Long, b: Option[Long])
    case D(a: Long, b: Option[Long])
    case E
    case F(prod: MyProduct)
  }

  private enum SimpleEnum extends EnumStableHashCode derives Arbitrary, Codec.EnumAsString {
    case X, Y, Z
  }

  private final case class Model(column: String, value: String) derives Arbitrary, Codec
  private final case class ModelExtended(column: String, value: String, extra: Option[Int]) derives Codec

  private enum Type extends EnumStableHashCode derives Arbitrary, Codec {
    case FixedString(n: Int)
    case Int
    case Decimal
  }

  private enum TypeExtended extends EnumStableHashCode derives Codec {
    case FixedByteArray(n: Int)
    case FixedString(n: Int)
    case Int
    case Float
    case Decimal(precision: Option[Int], scale: Option[Int])
  }

  def equalityFromOrd[T: Ord: Typeable]: Equality[T] =
    new Equality[T] {
      override def areEqual(a: T, b: Any): Boolean =
        b match {
          case b: T => Ord[T].equiv(a, b)
          case _ => false
        }
    }

  private given Equality[Float] = equalityFromOrd[Float]
  private given Equality[Double] = equalityFromOrd[Double]

  private def anyCauseContains(throwable: Throwable, expectedOneOf: Seq[String]): Boolean =
    Iterator
      .iterate(throwable)(_.getCause)
      .takeWhile(_ != null)
      .exists(t => expectedOneOf.exists(t.getMessage.contains))
}

trait DatasetReadWriteSuite extends DatasetSuite {
  import DatasetSuite.Result
  import DatasetReadWriteSuite.{*, given}

  /** tmpDir and cleanupPath can be overridden if the suite should not used the
    * local filesystem
    */
  def tmpDir: String = System.getProperty("java.io.tmpdir")
  def cleanupPath(path: String): Unit = {
    val file = new File(path)
    if (file.exists()) {
      if (file.isDirectory) { FileUtils.deleteDirectory(file) } else { file.delete(): Unit }
    }
  }
  def numericsReadModeForWriteTests: NumericsReadMode = NumericsReadMode.Exact
  def format: Format
  def includeReadTests: Boolean = true
  def includeOverwriteTests: Boolean = true

  private def checkSame[T](read: Seq[T], expected: Seq[T])(using aggregating: Aggregating[Seq[T]]): Result = {
    val containsSameElements = aggregating.containsTheSameElementsAs(read, expected)
    if containsSameElements then Result.Success else Result.Failure(s"""|Read values:
            |${read.mkString(", ")}
            |
            |Expected values:
            |${expected.mkString(", ")}""".stripMargin)
  }

  private def checkReadWrite[T: Arbitrary: Codec: Equality](
      write: Runner,
      read: Runner,
      readMode: NumericsReadMode
  ): Unit = checkReadWrite[T, T](write, read, identity, readMode)

  private def pattern(pattern: Format): String =
    pattern match {
      case Format.Parquet => "*.parquet"
      case Format.Json => "*.json"
    }

  private def checkReadWrite[Old: Arbitrary: Codec, New: Codec: Equality](
      write: Runner,
      read: Runner,
      update: Old => New,
      readMode: NumericsReadMode
  ): Unit = {
    def check(oldValues: Seq[Old]): Result =
      Using.resource(TmpPath(tmpDir, cleanupPath)) { tmpPath =>
        val path = tmpPath.path

        val writeDs = Dataset.from(oldValues).writeToPath(path, format)
        write.execute(writeDs)

        val readDs = readMode match {
          case NumericsReadMode.Exact =>
            Dataset.read[New](path, format, unpivot = false, filenameGlobFilter = pattern(format))
          case NumericsReadMode.WidenBigQuery =>
            val wideCodecAndCast = NumericsReadMode.widenBigQuery(Codec[New])
            Dataset
              .read(path, format, unpivot = false, filenameGlobFilter = pattern(format))(using
                wideCodecAndCast.codec
              )
              .select(wideCodecAndCast.cast)
        }
        checkSame(read.collect(readDs), oldValues.map(update))
      }
    val shrinkableValues = Vector.fill(100)(Arbitrary[Old].shrinkable(Random))
    val values = shrinkableValues.map(_.value)
    check(values).failureMessage match {
      case None => ()
      case Some(failureMessage) =>
        alert(failureMessage + "\n\nStarting to shrink failure...")
        val minimized = shrinkableValues.minimize(input => check(input).isFailure)
        check(minimized).check
    }
  }

  def testReadWrite[T: Arbitrary: Codec: TypeName: Equality]: Unit = {
    test(s"write ${TypeName.name[T]}")(checkReadWrite(
      write = implementation,
      read = reference,
      readMode = numericsReadModeForWriteTests
    ))

    if !includeReadTests then return
    test(s"read ${TypeName.name[T]}")(checkReadWrite(
      write = reference,
      read = implementation,
      readMode = NumericsReadMode.Exact
    ))
  }

  def testReadExtended[Old: Arbitrary: Codec: TypeName, New: Codec: TypeName: Equality](
      update: Old => New,
      customName: Option[String] = None
  ): Unit = {
    if !includeReadTests then return
    val name = customName.getOrElse(s"read ${TypeName.name[Old]} as ${TypeName.name[New]}")
    test(name)(checkReadWrite[Old, New](reference, implementation, update, NumericsReadMode.Exact))
  }

  def testReadPartitioned[P: Arbitrary: Codec: TypeName: Labelling as labels, M: Arbitrary: Codec: TypeName](
      // We can not use Partitioner.Hive here since that currently leads to cyclic module dependencies.
      formatPartition: P => String
  ): Unit = {
    if !includeReadTests then return

    def check(values: Seq[(P, Seq[M])]): Result =
      Using.resource(TmpPath(tmpDir, cleanupPath)) { tmpPath =>
        val basePath = tmpPath.path

        values.foreach { case (p, ms) =>
          val writeDs = Dataset.from(ms).writeToPath(s"$basePath/${formatPartition(p)}", format)
          reference.execute(writeDs)
        }

        val path = labels.elemLabels.map(p => s"$p=*").mkString(s"$basePath/", "/", "/")
        val readDs =
          Dataset.readWithHivePartitions[P, M](basePath, path, format, filenameGlobFilter = pattern(format))
        val read = implementation.collect(readDs)
        checkSame(read, values.flatMap { case (p, ms) => ms.map(m => (p, m)) })
      }

    test(s"read partitioned ${TypeName.name[M]} with partition type ${TypeName.name[P]}") {
      val shrinkableValues = Arbitrary[Seq[(P, Seq[M])]].filter(!_.isEmpty).shrinkable(Random)
      val values = shrinkableValues.value
      check(values).failureMessage match {
        case None => ()
        case Some(failureMessage) =>
          alert(failureMessage + "\n\nStarting to shrink failure...")
          val minimized = shrinkableValues.minimize(input => check(input).isFailure)
          check(minimized).check
      }
    }
  }

  test("write to existing path should fail") {
    if includeOverwriteTests then {

      Using.resource(TmpPath(tmpDir, cleanupPath)) { tmpPath =>
        val values = Vector.fill(10)(Arbitrary[Int]())
        val path = tmpPath.path

        val write = Dataset.from(values).writeToPath(path, format)
        implementation.execute(write)
        val exception = intercept[Exception](implementation.execute(write))
        val expectedOneOf = List("already exists", "destination is not empty")
        assert(
          anyCauseContains(exception, expectedOneOf),
          s"Exception '${exception}' did not contain any of expected snippets: ${expectedOneOf.mkString(
              ", "
            )}"
        )
      }: Unit
    }
  }

  test("read partitions paths") {
    if includeReadTests then {
      Using.resource(TmpPath(tmpDir, cleanupPath)) { tmpPath =>
        val basePath = tmpPath.path
        val formatPartition = (p: (p1: Int, p2: String)) => s"p1=${p.p1}/p2=${encode(p.p2)}"
        val partitions = Seq((p1 = 1, p2 = "a"), (p1 = 2, p2 = "b"))
        partitions.foreach { p =>
          val writeDs = Dataset.from(Seq(1L)).writeToPath(s"$basePath/${formatPartition(p)}", format)
          reference.execute(writeDs)
        }
        val globPath = s"$basePath/p1=*/p2=*"
        val readDs = Dataset.readPartitionsPaths[(p1: Int, p2: String)](globPath)
        val read = implementation.collect(readDs)
        checkSame(read, partitions).check
      }
    }
  }

  testReadWrite[Boolean]
  testReadWrite[Byte]
  testReadWrite[Short]
  testReadWrite[Int]
  testReadWrite[Long]
  testReadWrite[Float]
  testReadWrite[Double]
  testReadWrite[String]
  testReadWrite[Decimal[3, 0]]
  testReadWrite[Decimal[15, 5]]
  testReadWrite[Decimal[38, 9]]
  testReadWrite[Binary]
  testReadWrite[Date]
  testReadWrite[Timestamp]
  testReadWrite[Duration]
  testReadWrite[Option[Long]]
  testReadWrite[Option[Option[Long]]]
  testReadWrite[Seq[Long]]
  testReadWrite[Option[Seq[Long]]]
  testReadWrite[Seq[Seq[Long]]]
  testReadWrite[Seq[(Long, Option[Long])]]
  testReadWrite[Map[String, Long]]
  testReadWrite[Map[String, Duration]]
  testReadWrite[Option[Map[String, Long]]]
  testReadWrite[Map[(String, Long), (String, Option[Long])]]
  testReadWrite[Map[Seq[String], Seq[Long]]]
  testReadWrite[(Long, Option[Long])]
  testReadWrite[MyProduct]
  testReadWrite[Option[MyProduct]]
  testReadWrite[MyEnum]
  testReadWrite[Option[MyEnum]]
  testReadWrite[SimpleEnum]
  testReadWrite[Option[SimpleEnum]]

  {
    /* TODO: Spark will encode empty Strings are encoded as "__HIVE_DEFAULT_PARTITION__" and read back as
     * `null`. We should probably align our behavior to be compatible. */
    given Arbitrary[String] = Arbitrary[String].filter(s => !s.isEmpty)
    testReadPartitioned[(p1: Int, p2: String, p3: Date, p4: SimpleEnum), (m1: Long, m2: Option[Long])](p =>
      val encodedEnum = p.p4.toString.toLowerCase
      s"p1=${p.p1}/p2=${encode(p.p2)}/p3=${p.p3.toIsoString}/p4=${encodedEnum}"
    )
  }
  testReadExtended[Model, ModelExtended](v => ModelExtended(v.column, v.value, None))
  testReadExtended[Type, TypeExtended] {
    case Type.FixedString(n) => TypeExtended.FixedString(n)
    case Type.Int => TypeExtended.Int
    case Type.Decimal => TypeExtended.Decimal(None, None)
  }

  /** Verifies that string-encoded numbers in JSON (e.g., "123") are cleanly
    * coerced into their respective integral types on read. This ensures
    * consistent cross-backend behavior (tydaJson, tydaSpark, tydaBigQuery) when
    * parsing and decoding loose JSON formats without throwing strict schema
    * errors.
    */
  private def testJsonCoercion[Num: Codec: Equality: TypeName: com.choreograph.tyda.SimpleTypeName](
      fromString: String => Num
  )(using Arbitrary[Num]): Unit =
    if (format == Format.Json) {
      given Arbitrary[String] = Arbitrary[Num].map(_.toString)
      testReadExtended[String, Num](
        fromString,
        customName = Some(
          s"JSON coercion: read string-encoded numbers as ${com.choreograph.tyda.SimpleTypeName.name[Num]}"
        )
      )
    }

  testJsonCoercion[Byte](_.toByte)
  testJsonCoercion[Short](_.toShort)
  testJsonCoercion[Int](_.toInt)
  testJsonCoercion[Long](_.toLong)
}
