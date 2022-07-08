import sbt.Developer
import sbt.Keys._

// sbt-github-actions needs configuration in `ThisBuild`
ThisBuild / crossScalaVersions := Seq("2.12.15", "2.13.8")
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

lazy val root = project
  .in(file("."))
  .withId("sangria-root")
  .aggregate(ast, parser, core, benchmarks, derivation, sangria)
  .settings(inThisBuild(projectInfo))
  .settings(
    scalacSettings ++ shellSettings ++ noPublishSettings
  )
  .disablePlugins(MimaPlugin)

lazy val ast = project
  .in(file("modules/ast"))
  .withId("sangria-ast")
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria-ast",
    description := "Scala GraphQL AST representation",
    mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria-ast" % "3.0.0"),
    apiURL := {
      val ver = CrossVersion.binaryScalaVersion(scalaVersion.value)
      Some(url(s"https://www.javadoc.io/doc/org.sangria-graphql/sangria-ast_$ver/latest/"))
    }
  )

lazy val parser = project
  .in(file("modules/parser"))
  .withId("sangria-parser")
  .dependsOn(ast)
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria-parser",
    description := "Scala GraphQL parser",
    mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria-parser" % "3.0.0"),
    libraryDependencies ++= Seq(
      // AST Parser
      "org.parboiled" %% "parboiled" % "2.4.0",
      "org.scalatest" %% "scalatest" % "3.2.12" % Test
    ),
    apiURL := {
      val ver = CrossVersion.binaryScalaVersion(scalaVersion.value)
      Some(url(s"https://www.javadoc.io/doc/org.sangria-graphql/sangria-parser_$ver/latest/"))
    }
  )

lazy val core = project
  .in(file("modules/core"))
  .withId("sangria-core")
  .dependsOn(parser)
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria-core",
    description := "Scala GraphQL implementation",
    mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria-core" % "3.0.0"),
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
    libraryDependencies ++= Seq(
      // AST Visitor
      "org.sangria-graphql" %% "macro-visit" % "0.1.3",
      // Marshalling
      "org.sangria-graphql" %% "sangria-marshalling-api" % "1.0.7",
      // Streaming
      "org.sangria-graphql" %% "sangria-streaming-api" % "1.0.3",
      // Macros
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      // Testing
      "co.fs2" %% "fs2-core" % "2.5.11" % Test,
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
      "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.4" % Test,
      "org.sangria-graphql" %% "sangria-spray-json" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-argonaut" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-ion" % "2.0.1" % Test,
      "org.sangria-graphql" %% "sangria-monix" % "2.0.1" % Test,
      "eu.timepit" %% "refined" % "0.9.29" % Test,
      // CATs
      "net.jcazevedo" %% "moultingyaml" % "0.4.2" % Test,
      "io.github.classgraph" % "classgraph" % "4.8.149" % Test
    ),
    apiURL := {
      val ver = CrossVersion.binaryScalaVersion(scalaVersion.value)
      Some(url(s"https://www.javadoc.io/doc/org.sangria-graphql/sangria-core_$ver/latest/"))
    }
  )

lazy val derivation = project
  .in(file("modules/derivation"))
  .withId("sangria-derivation")
  .dependsOn(core % "compile->compile;test->test")
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria-derivation",
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oF"),
    mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria-derivation" % "3.0.0"),
    libraryDependencies ++= Seq(
      // Macros
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    apiURL := {
      val ver = CrossVersion.binaryScalaVersion(scalaVersion.value)
      Some(url(s"https://www.javadoc.io/doc/org.sangria-graphql/sangria-derivation_$ver/latest/"))
    }
  )

lazy val sangria = project
  .in(file("modules/sangria"))
  .withId("sangria")
  .dependsOn(core, derivation)
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria",
    description := "Scala GraphQL implementation",
    apiURL := {
      val ver = CrossVersion.binaryScalaVersion(scalaVersion.value)
      Some(url(s"https://www.javadoc.io/doc/org.sangria-graphql/sangria_$ver/latest/"))
    }
  )
  .disablePlugins(MimaPlugin)

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
  autoAPIMappings := true,
  Compile / doc / scalacOptions ++= // scaladoc options
    Opts.doc.title("Sangria") ++ Seq(
      "-groups",
      "-diagrams"
    ),
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
