package com.choreograph.tyda.testsuites

import org.scalatest.Assertions.assume

import com.choreograph.tyda.unreachable

object BigQueryIntegrationTestEnvVariables {
  private val ProjectId = "TYDA_BIGQUERY_TEST_PROJECT_ID"
  private val TmpDir = "TYDA_BIGQUERY_TEST_TMP_LOCATION"

  def skipIfProjectNotSet(): Unit = {
    // We use a local variable here to avoid the whole env becoming part of the error message
    val shouldRun = sys.env.contains(ProjectId)
    assume(
      condition = shouldRun,
      s"Provide a GCP project id using enviroment variable $ProjectId enable BigQuery integration tests"
    ): Unit
  }

  def skipIfProjectIsSet(): Unit = {
    // We use a local variable here to avoid the whole env becoming part of the error message
    val shouldRun = !sys.env.contains(ProjectId)
    assume(
      condition = shouldRun,
      s"Skipping bigquery-emulator tests: real BigQuery integration is active ($ProjectId is set)"
    ): Unit
  }

  def getProjectId: Option[String] = sys.env.get(ProjectId)

  def getProjectIdOrSkip: String =
    getProjectId match {
      case Some(projectId) => projectId
      case None =>
        skipIfProjectNotSet()
        unreachable("Test skipped by assume")
    }

  private def skipIfTmpDirNotSet(): Unit =
    // We use a local variable here to avoid the whole env becoming part of the error message
    val shouldRun = sys.env.contains(TmpDir)
    assume(
      condition = shouldRun,
      s"Provide gcs location using the eviroment variable $TmpDir to enable BigQuery integration read/write tests"
    ): Unit

  def getTmpDirOrSkip: String =
    sys.env.get(TmpDir) match {
      case Some(tmpDir) => tmpDir
      case None =>
        skipIfTmpDirNotSet()
        unreachable("Test skipped by assume")
    }
}
