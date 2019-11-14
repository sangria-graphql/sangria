import sbt.Developer
import sbt.Keys.{crossScalaVersions, developers, organizationHomepage, scalacOptions, scmInfo, startYear}

lazy val root = project
  .in(file("."))
  .withId("sangria-root")
  .aggregate(core)
  .settings(
    commonSettings,
    noPublishSettings
  )

lazy val core = project
  .in(file("modules/core"))
  .withId("sangria-core")
  .settings(
    commonSettings
  )

lazy val commonSettings = projectInfo ++ scalaSettings ++ shellSettings

lazy val projectInfo = Seq(
  organization := "org.sangria-graphql",
  homepage := Some(url("http://sangria-graphql.org")),
  licenses := Seq("Apache License, ASL Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  startYear := Some(2015),
  organizationHomepage := Some(url("https://github.com/sangria-graphql")),
  developers := Developer("OlegIlyenko", "Oleg Ilyenko", "", url("https://github.com/OlegIlyenko")) :: Nil,
  scmInfo := Some(ScmInfo(
    browseUrl = url("https://github.com/sangria-graphql-org/sangria.git"),
    connection = "scm:git:git@github.com:sangria-graphql-org/sangria.git"
  ))
)

lazy val scalaSettings = Seq(
  scalaVersion := "2.13.0",
  crossScalaVersions := Seq("2.11.12", "2.12.10", scalaVersion.value),
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Xlint:-missing-interpolator,_"),
  scalacOptions ++= {
    if (scalaVersion.value startsWith "2.11")
      Seq("-target:jvm-1.7")
    else
      Seq.empty
  }
)

lazy val shellSettings = Seq(
  // nice *magenta* prompt!
  ThisBuild / shellPrompt := { state =>
    scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
  }
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)
