import java.nio.file.StandardCopyOption

import scala.sys.process.*
import scala.util.Try
import scala.util.Success
import scala.util.Failure

import com.github.sbt.git.SbtGit.GitKeys.useConsoleForROGit

ThisBuild / tlBaseVersion := "0.4"
ThisBuild / organization := "com.wppresolve.tyda"
ThisBuild / organizationName := "WPP"
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  tlGitHubDev("eejbyfeldt", "Emil Ejbyfeldt"),
  tlGitHubDev("dahlbaek", "Jonas Dahlbæk"),
  tlGitHubDev("ch1nq", "Aske Ching"),
  tlGitHubDev("shambala-ifmo", "Andrei Mavrin")
)

ThisBuild / scalaVersion := Dependencies.scala3Version

ThisBuild / scalacOptions ++= Seq(
  "-Werror",
  // Seems to be quite easy to hit the default limit of 32 when using shapeless-3
  // And sometimes hitting the limit produces unexplainable errors
  // https://github.com/scala/scala3/issues/13927
  "-Xmax-inlines:256",
  "-Wall",
  "-Wunused:all", // Only needed to run scalafix rule RemoveUnused, -Wall already enables this
  // Until https://github.com/scalameta/mdoc/issues/1064 is fixed we have to avoid using spaces in -Wconf
  "-Wconf:" + Seq(
    "id=E192:s", // We don't currently care about binary compatibility
    "id=E176&msg=org\\.scalatest.*Assertion:s", // Unused Assertions is fine
    "id=E198&msg=import:i", // Unused imports will be handled by scalafix
    "id=E209&msg=toString:s" // TODO: Address new warning in Scala 3.7.2
  ).mkString(","),
  "-deprecation",
  "-feature"
)

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision
ThisBuild / scalafixDependencies += "com.github.xuwei-k" %% "scalafix-rules" % "0.6.24"

// This is needed for using git worktrees, without having jgit crash.
// When https://github.com/sbt/sbt-git/issues/264 is solved we can remove this.
ThisBuild / useConsoleForROGit := true

ThisBuild / tlCiDependencyGraphJob := false // TODO: Decide we we want this and if so implement it
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / githubWorkflowEnv := Map.empty // Do not set GITHUB_TOKEN everywhere
ThisBuild / githubWorkflowTargetBranches := Seq("**")

ThisBuild / githubWorkflowAddedJobs ++= {
  val javaVersions = (ThisBuild / githubWorkflowJavaVersions).value.toList
  val setupSteps = List(WorkflowStep.Checkout, WorkflowStep.SetupSbt) ++ WorkflowStep.SetupJava(javaVersions)
  Seq(
    WorkflowJob(
      id = "scalafmt",
      name = "Scalafmt",
      scalas = List("3"),
      javas = javaVersions,
      steps = setupSteps :+
        WorkflowStep.Sbt(List("scalafmtCheckAll", "scalafmtSbtCheck"), name = Some("Check scalafmt"))
    ),
    WorkflowJob(
      id = "scalafix",
      name = "Scalafix",
      scalas = List("3"),
      javas = javaVersions,
      steps = setupSteps :+ WorkflowStep.Sbt(List("scalafixAll --check"), name = Some("Check scalafix"))
    )
  )
}

ThisBuild / githubWorkflowPublishNeeds ++= Seq("scalafmt", "scalafix")

lazy val commonSettings =
  // mdoc snippets are checked by mdoc instead of the snippet compiler
  Seq(
    Compile / doc / scalacOptions ++= Seq("-snippet-compiler:compile,tyda-docs/target/mdoc/_docs=nocompile")
  )

// Scaladoc static site settings for sbt-unidoc
lazy val docSettings = Seq(
  ScalaUnidoc / unidoc / scalacOptions ++=
    Seq("-project-version", version.value, "-siteroot", (tydaDocs / mdocOut).value.getParent())
)

lazy val sparkRunSettings = Seq(
  /* This is need for running Spark. Based on
   * https://github.com/apache/spark/blob/v3.5.4/project/SparkBuild.scala#L1620 */
  javaOptions ++= Seq(
    "--add-opens=java.base/java.lang=ALL-UNNAMED",
    "--add-opens=java.base/java.lang.invoke=ALL-UNNAMED",
    "--add-opens=java.base/java.lang.reflect=ALL-UNNAMED",
    "--add-opens=java.base/java.io=ALL-UNNAMED",
    "--add-opens=java.base/java.net=ALL-UNNAMED",
    "--add-opens=java.base/java.nio=ALL-UNNAMED",
    "--add-opens=java.base/java.util=ALL-UNNAMED",
    "--add-opens=java.base/java.util.concurrent=ALL-UNNAMED",
    "--add-opens=java.base/java.util.concurrent.atomic=ALL-UNNAMED",
    "--add-opens=java.base/jdk.internal.ref=ALL-UNNAMED",
    "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED",
    "--add-opens=java.base/sun.nio.cs=ALL-UNNAMED",
    "--add-opens=java.base/sun.security.action=ALL-UNNAMED",
    "--add-opens=java.base/sun.util.calendar=ALL-UNNAMED",
    "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
  ),
  // Add provided dependencies to run to classpath based on
  /* https://stackoverflow.com/questions/18838944/how-to-add-provided-dependencies-back-to-run-test-tasks-classpath/21803413#21803413 */
  // https://github.com/sbt/sbt/issues/3733
  // Adding it to the Runtime configuration will not work as that will also include it in the assembly
  Compile / run :=
    Defaults.runTask(Compile / fullClasspath, Compile / run / mainClass, Compile / run / runner).evaluated,
  Compile / runMain := Defaults.runMainTask(Compile / fullClasspath, Compile / run / runner).evaluated,
  Compile / run / fork := true,
  Compile / run / javaOptions ++= Seq("-Dspark.master=local[*]"),
  // Override Spark's transitive dependencies to avoid version conflicts (e.g. Jackson)
  dependencyOverrides ++= SparkDependencies.dependencies
)

lazy val sparkTestSettings = Seq(Test / fork := true, Test / parallelExecution := false)

lazy val sparkSettings = sparkRunSettings ++ sparkTestSettings

lazy val tydaJobSettings = sparkRunSettings ++
  (if (sys.env.get("TYDA_JOB_TEST_RUNNER").map(_.toLowerCase).contains("spark")) sparkTestSettings else Seq())

lazy val root = (project in file("."))
  .aggregate(
    scalafixInput,
    scalafixOutput,
    scalafixRules,
    scalafixTests,
    tyda,
    tydaBigQuery,
    tydaCollection,
    tydaDocs,
    tydaIterator,
    tydaJob,
    tydaJobTest,
    tydaJson,
    tydaMeta,
    tydaMetadata,
    tydaParquet,
    tydaRepl,
    tydaRewrite,
    tydaSpark3,
    tydaSpark4,
    tydaSql,
    tydaSparkSql,
    tydaTable,
    tydaTestSuites
  )
  .enablePlugins(ScalaUnidocPlugin, NoPublishPlugin)
  .settings(commonSettings)
  .settings(docSettings)
  // Run mdoc as part of unidoc generation
  .settings(Compile / unidoc := (Compile / unidoc).dependsOn((tydaDocs / mdoc).toTask("")).value)
  .settings(
    // tydaTestSuites depends on scalatest which generates a lots of documentation warnings.
    // Since it a internal test project, we just exclude it from docs like other Test projects.
    ScalaUnidoc / unidoc / unidocProjectFilter :=
      inAnyProject -- inProjects(tydaTestSuites) -- inProjects(tydaSpark4)
  )
  .disablePlugins(ScalafixPlugin)
  .settings(name := "tyda")

lazy val tydaDocs = (project in file("tyda-docs"))
  .settings(name := "tyda-docs")
  .settings(
    // examples commonly have unused values for showing api usage and there also some mdoc internal warnings
    scalacOptions ++= Seq("-Wconf:id=E176&msg=value:s"),
    mdocIn := baseDirectory.value / "docs",
    mdocOut := mdocOut.value / "_docs"
  )
  .settings(Dependencies.tydaDocs)
  .settings(sparkRunSettings)
  .dependsOn(tyda, tydaTable, tydaJob, tydaJobTest, tydaSpark3)
  .enablePlugins(MdocPlugin)
  .disablePlugins(ScalafixPlugin)
  .enablePlugins(NoPublishPlugin)

lazy val scalafixRules = (project in file("scalafix/rules"))
  .settings(name := "tyda-scalafix-rules")
  .settings(commonSettings)
  .disablePlugins(ScalafixPlugin)
  .settings(Dependencies.scalafix)
lazy val scalafixInput = (project in file("scalafix/input"))
  .settings(name := "tyda-scalafix-input")
  .disablePlugins(ScalafixPlugin)
  .enablePlugins(NoPublishPlugin)
lazy val scalafixOutput = (project in file("scalafix/output"))
  .settings(name := "tyda-scalafix-output")
  .disablePlugins(ScalafixPlugin)
  .enablePlugins(NoPublishPlugin)
lazy val scalafixTests = (project in file("scalafix/tests"))
  .settings(name := "tyda-scalafix-tests")
  .settings(commonSettings)
  .settings(
    scalaVersion := "3.7.4", // Needs to match scalafix testkit version
    scalafixTestkitOutputSourceDirectories := (scalafixOutput / Compile / sourceDirectories).value,
    scalafixTestkitInputSourceDirectories := (scalafixInput / Compile / sourceDirectories).value,
    scalafixTestkitInputClasspath := (scalafixInput / Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions := (scalafixInput / Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion := (scalafixInput / Compile / scalaVersion).value
  )
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(scalafixInput, scalafixRules)
  .enablePlugins(ScalafixTestkitPlugin, NoPublishPlugin)

lazy val tyda = (project in file("tyda"))
  .settings(name := "tyda")
  .settings(commonSettings)
  .settings(Dependencies.tyda)
  .dependsOn(scalafixRules % ScalafixConfig)
  .settings(
    Compile / sourceGenerators +=
      Def
        .task {
          val log = streams.value.log
          val file = (Compile / sourceManaged).value / "com" / "choreograph" / "tyda" / "BuildInfo.scala"
          val gitDescribe = Try("git describe --tags --dirty --always".!!.trim) match {
            case Success(describe) =>
              if (describe.endsWith("-dirty")) { log.warn("Git repository is dirty.") }
              describe
            case Failure(e) =>
              log.warn(
                s"Failed to get git describe. This might be caused by not having git installed or not being in a git repository.\n $e"
              )
              "unknown"
          }
          val content = s"""|package com.choreograph.tyda
            |
            |/** Generated by sbt */
            |object BuildInfo {
            |  val gitDescribe: String = "$gitDescribe"
            |}
            |""".stripMargin
          IO.write(file, content)
          Seq(file)
        }
        .taskValue
  )

lazy val tydaJson = (project in file("tyda-json"))
  .settings(commonSettings)
  .settings(name := "tyda-json")
  .settings(Dependencies.tydaJson)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaTestSuites % "test->compile")

lazy val tydaRewrite = (project in file("tyda-rewrite"))
  .settings(name := "tyda-rewrite")
  .settings(commonSettings)
  .settings(Dependencies.tydaRewrite)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda)
  .dependsOn(tydaIterator % "test->compile")

lazy val tydaCollection = (project in file("tyda-collection"))
  .settings(name := "tyda-collection")
  .settings(commonSettings)
  .settings(Dependencies.tydaCollection)
  .dependsOn(scalafixRules % ScalafixConfig)

lazy val tydaIterator = (project in file("tyda-iterator"))
  .settings(name := "tyda-iterator")
  .settings(commonSettings)
  .settings(Dependencies.tydaIterator)
  .settings(tydaGoldenTestsSettings)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaJson, tydaParquet, tydaTestSuites % "test->compile")

lazy val tydaMeta = (project in file("tyda-meta"))
  .settings(name := "tyda-meta")
  .settings(commonSettings)
  .settings(Dependencies.tydaMeta)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaIterator % "test->compile")

lazy val tydaGoldenTestsSettings = Seq(
  Test / testOptions +=
    Tests.Argument("-Dtyda.golden.directory=" + (Test / resourceDirectory).value / "golden")
)

lazy val tydaTestSuites = (project in file("tyda-test-suites"))
  .settings(name := "tyda-test-suites")
  .settings(commonSettings)
  .settings(tydaGoldenTestsSettings)
  .settings(Dependencies.tydaTestSuites)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda)
  .enablePlugins(NoPublishPlugin)

lazy val tydaSpark3 = (project in file("tyda-spark"))
  .settings(name := "tyda-spark")
  .settings(commonSettings)
  .settings(sparkSettings)
  .settings(tydaGoldenTestsSettings)
  .settings(Dependencies.tydaSpark3)
  .settings(Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "main" / "spark-3")
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaRewrite)
  .dependsOn(tydaIterator % "test->compile")
  .dependsOn(tydaTestSuites % "test->compile")

lazy val tydaSpark4 = (project in file("tyda-spark4"))
  .settings(name := "tyda-spark4")
  .settings(commonSettings)
  .settings(sparkSettings)
  .settings(tydaGoldenTestsSettings)
  .settings(Dependencies.tydaSpark4)
  .settings(sourceDirectory := (tydaSpark3 / sourceDirectory).value)
  .settings(
    Compile / unmanagedSourceDirectories += (tydaSpark3 / baseDirectory).value / "src" / "main" / "spark-4",
    // We use the overrides to force spark 3 dependencies. Therefore we must remove them here.
    dependencyOverrides := Seq.empty,
    // This silences the following warning:
    //
    // An existential type that came from a Scala-2 classfile for class AgnosticEncoders
    // cannot be mapped accurately to a Scala-3 equivalent.
    //
    // Seems like the nowarn annotation can not be used for this warning, so we can not make the silencing
    // more local than this.
    scalacOptions += "-Wconf:id=E098:s"
  )
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaRewrite)
  .dependsOn(tydaIterator % "test->compile")
  .dependsOn(tydaTestSuites % "test->compile")

lazy val tydaMetadata = (project in file("tyda-metadata"))
  .settings(name := "tyda-metadata")
  .settings(commonSettings)
  .settings(Dependencies.tydaMetadata)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda)

lazy val tydaSql = (project in file("tyda-sql"))
  .settings(name := "tyda-sql")
  .settings(commonSettings)
  .settings(tydaGoldenTestsSettings)
  .settings(Dependencies.tydaSql)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaRewrite)
  .dependsOn(tydaIterator % "test->compile")
  .dependsOn(tydaTestSuites % "test->compile")

lazy val tydaSparkSql = (project in file("tyda-sparksql"))
  .settings(name := "tyda-spark-sql")
  .settings(commonSettings)
  .settings(sparkSettings)
  .settings(Dependencies.tydaSparkSql)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tydaSql, tydaSpark3)
  .dependsOn(tydaIterator % "test->compile")
  .dependsOn(tydaTestSuites % "test->compile")
  .enablePlugins(NoPublishPlugin)

lazy val tydaBigQuery = (project in file("tyda-big-query"))
  .settings(name := "tyda-big-query")
  .settings(commonSettings)
  .settings(Dependencies.tydaBigQuery)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tydaSql)
  .dependsOn(tydaIterator % "test->compile")
  .dependsOn(tydaTestSuites % "test->compile")

lazy val tydaRepl = (project in file("tyda-repl"))
  .settings(name := "tyda-repl")
  .settings(commonSettings)
  .settings(libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaTable)

lazy val tydaTable = (project in file("tyda-table"))
  .settings(name := "tyda-table")
  .settings(commonSettings)
  .settings(Dependencies.tydaTable)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda)
  .dependsOn(tydaIterator % "test->compile")

lazy val tydaJob = (project in file("tyda-job"))
  .settings(name := "tyda-job")
  .settings(commonSettings)
  .settings(Dependencies.tydaJob)
  .settings(tydaJobSettings)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tyda, tydaTable)

lazy val tydaJobTest = (project in file("tyda-job-test"))
  .settings(name := "tyda-job-test")
  .settings(commonSettings)
  .settings(tydaJobSettings)
  .settings(Dependencies.tydaJobTest)
  .dependsOn(scalafixRules % ScalafixConfig)
  .dependsOn(tydaJob, tydaSpark3, tydaIterator)

lazy val tydaParquet = (project in file("tyda-parquet"))
  .settings(name := "tyda-parquet")
  .settings(commonSettings)
  .settings(Dependencies.tydaParquet)
  .dependsOn(tyda)
  .dependsOn(scalafixRules % ScalafixConfig)
