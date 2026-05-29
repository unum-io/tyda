package com.choreograph.tyda.testsuites

import java.io.File
import java.io.PrintWriter

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

import org.scalatest.BeforeAndAfterAllConfigMap
import org.scalatest.ConfigMap
import org.scalatest.funsuite.AnyFunSuite

trait GoldenTestSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {
  import GoldenTestSuite.*

  private val testOutputs = mutable.ArrayBuffer.empty[(name: String, output: String)]
  private var goldenData: Map[String, String] = Map.empty
  private var isRegenerateMode: Boolean = false

  def goldenFileName = s"${getClass.getSimpleName}.golden"

  def goldenTest(name: String)(output: => String): Unit =
    test(name) {
      val actualOutput = output

      if (isRegenerateMode) { testOutputs.append((name, actualOutput)) }
      else {
        goldenData.get(name) match {
          case None => fail(s"Test '$name' not found in golden file. Run with ${GenerateGoldenFiles
                .env}=1 to regenerate.")
          case Some(expected) => if (actualOutput != expected) {
              val diff = generateDiff(expected, actualOutput)
              fail(s"Output mismatch for test '$name':\n$diff")
            }
        }
      }
    }

  override def beforeAll(configMap: ConfigMap): Unit = {
    val goldenDirectory = GoldenDirectory.get(configMap) match {
      case Some(goldenDirectory) => goldenDirectory
      case None =>
        val prop = GoldenDirectory.property
        fail(
          s"${prop} found in configMap. Please configure Test / testOptions += Tests.Argument(\"-D$prop=...\")"
        )
    }
    isRegenerateMode = GenerateGoldenFiles.get(configMap).isDefined
    if (!isRegenerateMode) { goldenData = loadGoldenFile(goldenDirectory) }
  }

  override def afterAll(configMap: ConfigMap): Unit =
    GoldenDirectory.get(configMap) match {
      case Some(goldenDirectory) if isRegenerateMode =>
        writeGoldenFile(testOutputs, getGoldenFilePath(goldenDirectory))
        val filePath = getGoldenFilePath(goldenDirectory).getPath
        info(s"Regenerated golden file: $filePath (${testOutputs.size} test cases)")
      case _ =>
    }

  private def getGoldenFilePath(goldenDirectory: String): java.io.File =
    new File(goldenDirectory, goldenFileName)

  private def loadGoldenFile(goldenDirectory: String): Map[String, String] = {
    val file = getGoldenFilePath(goldenDirectory)
    if (!file.exists()) {
      fail(s"Golden file not found: ${file.getPath}\nRun with ${GenerateGoldenFiles.env}=1 to generate it.")
    }

    Using.resource(Source.fromFile(file))(parseGoldenFile)
  }
}

object GoldenTestSuite {
  final case class ConfigurationOption(name: String*) {
    def env: String = name.map(_.toUpperCase).mkString("_")
    def property: String = name.map(_.toLowerCase).mkString(".")

    def get(configMap: ConfigMap): Option[String] =
      configMap.get(property).collect { case s: String => s }.orElse(sys.env.get(env))
  }

  private val GoldenDirectory = ConfigurationOption("tyda", "golden", "directory")
  private val GenerateGoldenFiles = ConfigurationOption("tyda", "golden", "generate", "files")
  private val TestMarker = "------ Test: "
  private val EndMarker = "\n------ end"

  private def writeGoldenFile(
      data: collection.Seq[(name: String, output: String)],
      goldenFile: File
  ): Unit = {
    goldenFile.getParentFile.mkdirs()

    Using.resource(new PrintWriter(goldenFile)) { writer =>
      val entries = data.sortBy(_.name)
      entries
        .zipWithIndex
        .foreach { case ((testName, output), idx) =>
          writer.println(s"$TestMarker$testName")
          writer.print(output)
          writer.println(EndMarker)
          if (idx < entries.length - 1) writer.println()
        }
    }
  }

  private def generateDiff(expected: String, actual: String): String = {
    val maxLen = 1000
    val expectedPreview = if (expected.length > maxLen) expected.take(maxLen) + "..." else expected
    val actualPreview = if (actual.length > maxLen) actual.take(maxLen) + "..." else actual

    s"""Expected:
       |$expectedPreview
       |
       |Actual:
       |$actualPreview
       |""".stripMargin
  }

  private def parseGoldenFile(source: Source): Map[String, String] = {
    val content = source.mkString

    def parseTests(startPos: Int): List[(String, String)] = {
      if (startPos >= content.length) return Nil

      val testStart = content.indexOf(TestMarker, startPos)
      if (testStart == -1) return Nil

      val testNameStart = testStart + TestMarker.length
      val testNameEnd = content.indexOf('\n', testNameStart)
      if testNameEnd == -1 then
        throw new IllegalStateException(s"Missing newline after test marker at position $testStart")

      val testName = content.substring(testNameStart, testNameEnd)
      val outputStart = testNameEnd + 1
      val outputEnd = content.indexOf(EndMarker, outputStart)

      if outputEnd == -1 then throw new IllegalStateException(s"Missing end marker for test '$testName'")

      val output = content.substring(outputStart, outputEnd)
      val nextPos = outputEnd + EndMarker.length

      (testName, output) :: parseTests(nextPos)
    }

    parseTests(0).toMap
  }
}
