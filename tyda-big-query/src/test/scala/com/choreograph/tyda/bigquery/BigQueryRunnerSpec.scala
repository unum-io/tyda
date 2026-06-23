package com.choreograph.tyda.bigquery

import org.scalatest.funsuite.AnyFunSuite

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Format
import com.choreograph.tyda.RunnerArgs
import com.choreograph.tyda.RunnerArgs.ValidateWriteCompatibility

class BigQueryRunnerSpec extends AnyFunSuite {
  test("createRunner returns BigQueryRunner") {
    val runner = RunnerArgs.createRunner(RunnerArgs.BigQuery("test-project-id"), "test-app")
    assert(runner.isInstanceOf[BigQueryRunner])
  }

  private def check[T: Codec](mode: ValidateWriteCompatibility) =
    BigQueryRunner.checkWriteCompatibility(Dataset.from[T](Seq.empty).writeToPath("p", Format.Parquet), mode)

  private def expectIncompatible[T: Codec](
      name: String,
      mode: ValidateWriteCompatibility = ValidateWriteCompatibility.Strict
  ) =
    test(name) {
      val ex = intercept[RuntimeException](check[T](mode))
      assert(ex.getMessage.contains("incompatible"))
    }

  private def expectLossy[T: Codec](name: String) =
    test(name) {
      val ex = intercept[RuntimeException](check[T](ValidateWriteCompatibility.Strict))
      assert(ex.getMessage.contains("lossy"))
    }

  private def expectCompatible[T: Codec](
      name: String,
      mode: ValidateWriteCompatibility = ValidateWriteCompatibility.Strict
  ) = test(name) { check[T](mode) }

  expectCompatible[Seq[Int]]("compatible: simple array")
  expectCompatible[Option[Int]]("compatible: optional scalar")
  expectCompatible[(a: Int, b: Seq[String])]("compatible: struct with array field")
  expectCompatible[Option[Seq[Int]]](
    "compatible: optional array in Lossy mode",
    ValidateWriteCompatibility.Lossy
  )
  expectCompatible[Seq[Seq[Int]]]("compatible: nested arrays in Warn mode", ValidateWriteCompatibility.Warn)
  expectCompatible[Seq[Seq[Int]]]("compatible: nested arrays in Off mode", ValidateWriteCompatibility.Off)

  expectIncompatible[Seq[Seq[Int]]]("Strict: nested arrays are incompatible")
  expectIncompatible[Seq[Option[Int]]]("Strict: array of optionals is incompatible")
  expectIncompatible[Map[String, Int]]("Strict: map is incompatible")
  expectIncompatible[(a: Int, b: Seq[Seq[Int]])]("Strict: nested arrays inside struct are incompatible")
  expectIncompatible[(a: Int, b: Seq[Option[Int]])](
    "Strict: array of optionals inside struct is incompatible"
  )
  expectIncompatible[Seq[(a: Int, b: Seq[Seq[Int]])]](
    "Strict: nested arrays inside array of structs is incompatible"
  )
  expectIncompatible[Seq[(a: Int, b: Seq[Option[Int]])]](
    "Strict: array of optionals inside array of structs is incompatible"
  )
  expectLossy[Option[Seq[Int]]]("Strict: optional array is lossy")
  expectLossy[(a: Int, b: Option[Seq[Int]])]("Strict: optional array inside struct is lossy")
  expectLossy[Seq[(a: Int, b: Option[Seq[Int]])]]("Strict: optional array inside array of structs is lossy")

  expectIncompatible[Seq[Seq[Int]]]("Lossy: nested arrays are incompatible", ValidateWriteCompatibility.Lossy)
  expectIncompatible[Seq[Option[Int]]](
    "Lossy: array of optionals is incompatible",
    ValidateWriteCompatibility.Lossy
  )
}
