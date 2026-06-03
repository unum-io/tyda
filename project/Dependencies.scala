import _root_.scalafix.sbt.BuildInfo as ScalafixBuildInfo
import sbt.*

import Keys.*

object Dependencies {
  val scala3Version = "3.7.4"
  val sparkVersion = "3.5.1"
  val jsoniterVersion = "2.38.6"

  object TestDeps {
    val scalatest = CompileDeps.scalatest % Test
    val commonsIo = CompileDeps.commonsIo % Test
    val sparkSql = (CompileDeps.sparkSql % Test).exclude("org.scala-lang.modules", "scala-xml_2.13")
    val gcsConnector = CompileDeps.gcsConnector % Test
    val jsonSchemaValidator = "com.networknt" % "json-schema-validator" % "3.0.1" % Test
    val bigQuerySparkConnector = "com.google.cloud.spark" % "spark-3.5-bigquery" % "0.44.1" % Test
  }

  object CompileDeps {
    val shapeless3 = "org.typelevel" %% "shapeless3-deriving" % "3.6.0"
    val jsoniterCore = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % jsoniterVersion
    val scalatest = "org.scalatest" %% "scalatest" % "3.2.19"
    val commonsIo = "commons-io" % "commons-io" % "2.21.0"
    val slf4j = "org.slf4j" % "slf4j-api" % "2.0.17"
    val bigQuery = "com.google.cloud" % "google-cloud-bigquery" % "2.61.0"
    val parquet = "org.apache.parquet" % "parquet-hadoop" % "1.13.1"
    val hadoop = "org.apache.hadoop" % "hadoop-client-runtime" % "3.3.4"
    val sparkSql = ("org.apache.spark" %% "spark-sql" % sparkVersion).cross(CrossVersion.for3Use2_13)
    val scalameta = "org.scalameta" %% "scalameta" % "4.15.2"
    val gcsConnector = ("com.google.cloud.bigdataoss" % "gcs-connector" % "hadoop3-2.2.31")
  }

  object ProvidedDeps {
    val sparkSql = ("org.apache.spark" %% "spark-sql" % sparkVersion % Provided).cross(
      CrossVersion.for3Use2_13
    )
    val sparkHadoop = ("org.apache.spark" %% "spark-hadoop-cloud" % sparkVersion % Provided).cross(
      CrossVersion.for3Use2_13
    )
    val jsoniterMacros = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" %
      jsoniterVersion % Provided
  }

  val scalafix = libraryDependencies ++= Seq(
    ("ch.epfl.scala" %% "scalafix-core" % ScalafixBuildInfo.scalafixVersion).cross(CrossVersion.for3Use2_13),
    CompileDeps.shapeless3,
    TestDeps.scalatest
  )

  val tyda = libraryDependencies ++= Seq(TestDeps.scalatest, CompileDeps.shapeless3)

  val tydaRewrite = libraryDependencies ++= Seq(TestDeps.scalatest)

  val tydaCollection = libraryDependencies ++= Seq(TestDeps.scalatest)

  val tydaDocs = libraryDependencies ++= Seq(CompileDeps.sparkSql)

  val tydaTestSuites = libraryDependencies ++= Seq(CompileDeps.scalatest, CompileDeps.commonsIo)

  val tydaParquet = libraryDependencies ++=
    Seq(CompileDeps.parquet, CompileDeps.hadoop, TestDeps.commonsIo, TestDeps.scalatest)

  val tydaMeta = libraryDependencies ++= Seq(TestDeps.scalatest)

  val tydaJson = libraryDependencies ++= Seq(
    CompileDeps.jsoniterCore,
    ProvidedDeps.jsoniterMacros,
    TestDeps.scalatest,
    TestDeps.jsonSchemaValidator
  )

  val tydaIterator = libraryDependencies ++= Seq(TestDeps.commonsIo, TestDeps.scalatest)

  val tydaMetadata = libraryDependencies ++=
    Seq(CompileDeps.jsoniterCore, CompileDeps.scalameta, ProvidedDeps.jsoniterMacros, TestDeps.scalatest)

  val tydaSql = libraryDependencies ++= Seq()

  val tydaSparkSql = libraryDependencies ++= Seq(TestDeps.sparkSql)

  val tydaBigQuery = libraryDependencies ++=
    Seq(CompileDeps.bigQuery, TestDeps.scalatest, TestDeps.gcsConnector)

  val tydaSpark = libraryDependencies ++= Seq(
    TestDeps.scalatest,
    TestDeps.bigQuerySparkConnector,
    ProvidedDeps
      .sparkSql
      /* Both scalatest and Spark depend on scala-xml we need to exclude one of them since one can not mix
       * 2.13 and 3 of the same artifact. But since scalatest is only used in test any compatability issues
       * should show up in tests. */
      .exclude("org.scala-lang.modules", "scala-xml_2.13")
  )

  val tydaTable = libraryDependencies ++= Seq(TestDeps.scalatest)

  val tydaJob = libraryDependencies ++=
    Seq(CompileDeps.slf4j, TestDeps.scalatest.exclude("org.scala-lang.modules", "scala-xml_3"))

  val tydaJobTest = libraryDependencies ++=
    Seq(CompileDeps.scalatest.exclude("org.scala-lang.modules", "scala-xml_3"), CompileDeps.sparkSql)
}
