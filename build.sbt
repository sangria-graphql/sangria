import sbt.Developer
import sbt.Keys.{
  crossScalaVersions,
  developers,
  organizationHomepage,
  scalacOptions,
  scmInfo,
  startYear
}

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq("2.12.12", "2.13.4")
ThisBuild / scalaVersion := crossScalaVersions.value.last
ThisBuild / githubWorkflowPublishTargetBranches := List()
ThisBuild / githubWorkflowBuildPreamble += WorkflowStep.Sbt(
  List("scalafmtCheckAll"),
  name = Some("Check formatting"))

lazy val root = project
  .in(file("."))
  .withId("sangria-root")
  .aggregate(core, benchmarks)
  .settings(inThisBuild(projectInfo))
  .settings(
    scalacSettings ++ shellSettings ++ publishSettings ++ noPublishSettings
  )

lazy val core = project
  .in(file("modules/core"))
  .withId("sangria-core")
  .settings(scalacSettings ++ shellSettings ++ publishSettings)
  .settings(
    name := "sangria",
    description := "Scala GraphQL implementation",
    mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria" % "1.4.2"),
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
    libraryDependencies ++= Seq(
      // AST Parser
      "org.parboiled" %% "parboiled" % "2.2.1",
      // AST Visitor
      "org.sangria-graphql" %% "macro-visit" % "0.1.3",
      // Marshalling
      "org.sangria-graphql" %% "sangria-marshalling-api" % "1.0.5",
      // Streaming
      "org.sangria-graphql" %% "sangria-streaming-api" % "1.0.2",
      // Macros
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      // Testing
      "co.fs2" %% "fs2-core" % "2.5.0" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.3" % Test,
      "org.sangria-graphql" %% "sangria-spray-json" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-argonaut" % "1.0.1" % Test,
      "org.sangria-graphql" %% "sangria-ion" % "2.0.0" % Test,
      "org.sangria-graphql" %% "sangria-monix" % "2.0.0" % Test,
      "eu.timepit" %% "refined" % "0.9.19" % Test,
      // CATs
      "net.jcazevedo" %% "moultingyaml" % "0.4.2" % Test,
      "io.github.classgraph" % "classgraph" % "4.8.98" % Test
    )
  )

lazy val benchmarks = project
  .in(file("modules/benchmarks"))
  .withId("sangria-benchmarks")
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(scalacSettings ++ shellSettings ++ noPublishSettings)
  .settings(
    name := "sangria-benchmarks",
    description := "Benchmarks of Sangria functionality"
  )

/* Commonly used functionality across the projects
 */

lazy val projectInfo = Seq(
  organization := "org.sangria-graphql",
  homepage := Some(url("http://sangria-graphql.org")),
  licenses := Seq(
    "Apache License, ASL Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  startYear := Some(2015),
  organizationHomepage := Some(url("https://github.com/sangria-graphql")),
  developers := Developer(
    "OlegIlyenko",
    "Oleg Ilyenko",
    "",
    url("https://github.com/OlegIlyenko")) :: Nil,
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("https://github.com/sangria-graphql/sangria.git"),
      connection = "scm:git:git@github.com:sangria-graphql/sangria.git"
    ))
)

lazy val scalacSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint:-missing-interpolator,_"),
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.12")) Seq("-language:higherKinds") else List.empty[String]
  },
  scalacOptions += "-target:jvm-1.8",
  javacOptions ++= Seq("-source", "8", "-target", "8")
)

lazy val shellSettings = Seq(
  // nice *magenta* prompt!
  ThisBuild / shellPrompt := { state =>
    scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val publishSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseVcsSign := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := (_ => false),
  publishTo := Some(
    if (isSnapshot.value)
      "snapshots".at("https://oss.sonatype.org/content/repositories/snapshots")
    else
      "releases".at("https://oss.sonatype.org/service/local/staging/deploy/maven2"))
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)
