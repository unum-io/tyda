package com.choreograph.tyda.bigquery

import java.net.HttpURLConnection
import java.net.URL
import java.util.UUID

import scala.util.Try

import org.scalatest.Assertions.assume
import org.slf4j.LoggerFactory

import com.choreograph.tyda.unreachable

/** Manages the bigquery-emulator for integration tests.
  *
  * The emulator exposes a BigQuery-compatible HTTP API that accepts the same
  * Google Cloud BigQuery client, allowing locally generated SQL to be validated
  * against an actual BigQuery SQL engine.
  *
  * Startup behaviour (in priority order):
  *   1. If `TYDA_BQ_EMULATOR_ENDPOINT` is set, use that pre-started emulator.
  *   2. If Docker is available, start a container automatically.
  *   3. Otherwise skip the test with an informative message.
  *
  * @see
  *   https://github.com/goccy/bigquery-emulator
  */
object BigQueryEmulatorContainer {
  val Image = "ghcr.io/goccy/bigquery-emulator:latest"
  val DefaultProjectId = "tyda-test"
  val DefaultDatasetId = "tyda_dataset"
  val ContainerHttpPort = 9050

  private val EndpointEnvVar = "TYDA_BQ_EMULATOR_ENDPOINT"
  private val ProjectIdEnvVar = "TYDA_BQ_EMULATOR_PROJECT_ID"

  private val HealthCheckTimeoutMs = 60_000
  private val HealthCheckIntervalMs = 500
  private val logger = LoggerFactory.getLogger(getClass)

  /** Provides an emulator connection, starting a Docker container if needed.
    *
    * Returns the endpoint URL and project ID for the running emulator, or
    * calls `assume(false, ...)` to skip the calling test if the emulator is
    * unavailable.
    */
  def connectOrSkip(): EmulatorConnection = {
    sys.env.get(EndpointEnvVar) match {
      case Some(endpoint) =>
        val projectId = sys.env.getOrElse(ProjectIdEnvVar, DefaultProjectId)
        logger.info(s"Using pre-started bigquery-emulator at $endpoint (project=$projectId)")
        ExternalConnection(endpoint, projectId)
      case None =>
        startInDocker()
    }
  }

  private def startInDocker(): StartedContainer = {
    if (!isDockerAvailable) {
      assume(
        condition = false,
        s"bigquery-emulator is not available. Either start it manually and set " +
          s"$EndpointEnvVar=<http-url> (and optionally $ProjectIdEnvVar=<project-id>), " +
          s"or ensure Docker is accessible to run the emulator automatically."
      )
      unreachable("Test skipped by assume")
    }
    start()
  }

  private def isDockerAvailable: Boolean =
    Try {
      val process = new ProcessBuilder("docker", "info")
        .redirectErrorStream(true)
        .start()
      // Drain output to avoid blocking
      process.getInputStream.readAllBytes()
      process.waitFor() == 0
    }.getOrElse(false)

  private[bigquery] def start(): StartedContainer = {
    val containerName = s"tyda-bq-emulator-${UUID.randomUUID().toString.take(8)}"
    val port = findFreePort()
    val cmd = Seq(
      "docker",
      "run",
      "--rm",
      "-d",
      "--name",
      containerName,
      "-p",
      s"$port:$ContainerHttpPort",
      Image,
      s"--project=$DefaultProjectId",
      s"--dataset=$DefaultDatasetId"
    )

    logger.info(s"Starting bigquery-emulator container: ${cmd.mkString(" ")}")
    val process = new ProcessBuilder(cmd*)
      .redirectErrorStream(true)
      .start()
    // Read output before waitFor to avoid blocking if the buffer fills up
    val output = new String(process.getInputStream.readAllBytes())
    val exitCode = process.waitFor()
    if (exitCode != 0) {
      throw new RuntimeException(
        s"Failed to start bigquery-emulator container (exit $exitCode): $output"
      )
    }

    val containerId = output.strip()
    val endpoint = s"http://localhost:$port"
    logger.info(s"Started container $containerName (id=$containerId) at $endpoint")

    waitForHealthy(endpoint, DefaultProjectId)
    StartedContainer(containerName, endpoint, DefaultProjectId, port)
  }

  private def findFreePort(): Int = {
    val socket = new java.net.ServerSocket(0)
    try socket.getLocalPort
    finally socket.close()
  }

  private[bigquery] def waitForHealthy(endpoint: String, projectId: String): Unit = {
    val deadline = System.currentTimeMillis() + HealthCheckTimeoutMs
    var lastError: Throwable = new RuntimeException("Timeout waiting for emulator")
    while (System.currentTimeMillis() < deadline) {
      val result = Try {
        val conn = URL(s"$endpoint/bigquery/v2/projects/$projectId/datasets").openConnection()
          .asInstanceOf[HttpURLConnection]
        conn.setConnectTimeout(1000)
        conn.setReadTimeout(1000)
        conn.getResponseCode
      }
      result match {
        case scala.util.Success(_) =>
          logger.info(s"bigquery-emulator is ready at $endpoint")
          return
        case scala.util.Failure(e) =>
          lastError = e
          Thread.sleep(HealthCheckIntervalMs)
      }
    }
    throw new RuntimeException(
      s"bigquery-emulator did not become ready within ${HealthCheckTimeoutMs}ms",
      lastError
    )
  }

  sealed trait EmulatorConnection {
    def endpoint: String
    def projectId: String
  }

  final class ExternalConnection(val endpoint: String, val projectId: String)
      extends EmulatorConnection

  final class StartedContainer(
      val name: String,
      val endpoint: String,
      val projectId: String,
      val port: Int
  ) extends EmulatorConnection
      with AutoCloseable {
    def close(): Unit = {
      logger.info(s"Stopping bigquery-emulator container $name")
      val process = new ProcessBuilder("docker", "stop", name)
        .redirectErrorStream(true)
        .start()
      process.waitFor(): Unit
    }
  }
}
