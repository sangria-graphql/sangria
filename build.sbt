import sbt.Developer
import sbt.Keys.{
  crossScalaVersions,
  developers,
  organizationHomepage,
  scalacOptions,
  scmInfo,
  startYear
}
import com.typesafe.tools.mima.core.{
  DirectMissingMethodProblem,
  IncompatibleResultTypeProblem,
  Problem,
  ProblemFilters
}

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq("2.12.15", "2.13.6")
ThisBuild / scalaVersion := crossScalaVersions.value.last
ThisBuild / githubWorkflowBuildPreamble ++= List(
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Check binary compatibility")),
  WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check formatting"))
)

// Release
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(RefPredicate.StartsWith(Ref.Tag("v")))

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

// Binary Incompatible Changes, we'll document.
ThisBuild / mimaBinaryIssueFilters ++= Seq(
  ProblemFilters.exclude[Problem]("sangria.schema.ProjectedName*"),
  ProblemFilters.exclude[Problem]("sangria.schema.Args*"),
  ProblemFilters.exclude[Problem]("sangria.execution.deferred.FetcherConfig*"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.ast.FragmentDefinition.typeConditionOpt"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("sangria.ast.ObjectValue.fieldsByName"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem](
    "sangria.marshalling.QueryAstInputUnmarshaller.getMapKeys")
)

lazy val root = project
  .in(file("."))
  .withId("sangria-root")
  .aggregate(core, benchmarks)
  .settings(inThisBuild(projectInfo))
  .settings(
    scalacSettings ++ shellSettings ++ noPublishSettings
  )
  .disablePlugins(MimaPlugin)

lazy val core = project
  .in(file("modules/core"))
  .withId("sangria-core")
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria",
    description := "Scala GraphQL implementation",
    mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria" % "2.1.3"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
    libraryDependencies ++= Seq(
      // AST Parser
      "org.parboiled" %% "parboiled" % "2.3.0",
      // AST Visitor
      "org.sangria-graphql" %% "macro-visit" % "0.1.3",
      // Marshalling
      "org.sangria-graphql" %% "sangria-marshalling-api" % "1.0.6",
      // Streaming
      "org.sangria-graphql" %% "sangria-streaming-api" % "1.0.3",
      // Macros
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      // Testing
      "co.fs2" %% "fs2-core" % "3.1.5" % Test,
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.4" % Test,
      "org.sangria-graphql" %% "sangria-spray-json" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-argonaut" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-ion" % "2.0.1" % Test,
      "org.sangria-graphql" %% "sangria-monix" % "2.0.1" % Test,
      "eu.timepit" %% "refined" % "0.9.27" % Test,
      // CATs
      "net.jcazevedo" %% "moultingyaml" % "0.4.2" % Test,
      "io.github.classgraph" % "classgraph" % "4.8.126" % Test
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
  .disablePlugins(MimaPlugin)

/* Commonly used functionality across the projects
 */

lazy val projectInfo = Seq(
  organization := "org.sangria-graphql",
  homepage := Some(url("https://sangria-graphql.github.io/")),
  licenses := Seq(
    "Apache License, ASL Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  startYear := Some(2015),
  organizationHomepage := Some(url("https://github.com/sangria-graphql")),
  developers :=
    Developer("OlegIlyenko", "Oleg Ilyenko", "", url("https://github.com/OlegIlyenko")) ::
      Developer("yanns", "Yann Simon", "", url("https://github.com/yanns")) ::
      Developer("nickhudkins", "Nick Hudkins", "", url("https://github.com/nickhudkins")) ::
      Developer("sh0hei", "Shohei Shimomura", "", url("https://github.com/sh0hei")) ::
      Nil,
  scmInfo := Some(
    ScmInfo(
      browseUrl = url("https://github.com/sangria-graphql/sangria"),
      connection = "scm:git:git@github.com:sangria-graphql/sangria.git"
    ))
)

lazy val scalacSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint:-missing-interpolator,_"),
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.12")) Seq("-language:higherKinds") else List.empty[String]
  },
  scalacOptions += "-target:jvm-1.8",
  Compile / doc / scalacOptions ++= Seq( // scaladoc options
    "-groups"),
  javacOptions ++= Seq("-source", "8", "-target", "8")
)

lazy val shellSettings = Seq(
  // nice *magenta* prompt!
  ThisBuild / shellPrompt := { state =>
    scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val noPublishSettings = Seq(
  publish / skip := true
)
