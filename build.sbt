name := "sangria"
organization := "org.sangria-graphql"
version := "1.0.0-RC6-SNAPSHOT"

description := "Scala GraphQL implementation"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq("Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.12.0"
crossScalaVersions := Seq("2.11.8", "2.12.0")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Xlint",
  "-Xlint:-missing-interpolator")

scalacOptions ++= {
  if (scalaVersion.value startsWith "2.12")
    Seq.empty
  else
    Seq("-target:jvm-1.7")
}

libraryDependencies ++= Seq(
  // macros
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  // parsing
  "org.parboiled" %% "parboiled" % "2.1.3",

  // marshalling
  "org.sangria-graphql" %% "sangria-marshalling-api" % "0.2.2",

  // streaming
  "org.sangria-graphql" %% "sangria-streaming-api" % "0.1.1",

  // testing
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.sangria-graphql" %% "sangria-marshalling-testkit" % "0.2.3" % "test",
  "org.sangria-graphql" %% "sangria-spray-json" % "0.3.2" % "test",
  "org.sangria-graphql" %% "sangria-ion" % "0.1.1" % "test",
  "org.sangria-graphql" %% "sangria-monix" % "0.1.1" % "test",
  "org.sangria-graphql" %% "sangria-rxscala" % "0.1.1" % "test",

  // CATs
  "net.jcazevedo" %% "moultingyaml" % "0.3.1" % "test",
  "io.github.lukehutch" % "fast-classpath-scanner" % "1.9.18" % "test"
)

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

// Publishing

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := (_ ⇒ false)
publishTo := Some(
  if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

startYear := Some(2015)
organizationHomepage := Some(url("https://github.com/sangria-graphql"))
developers := Developer("OlegIlyenko", "Oleg Ilyenko", "", url("https://github.com/OlegIlyenko")) :: Nil
scmInfo := Some(ScmInfo(
  browseUrl = url("https://github.com/sangria-graphql/sangria.git"),
  connection = "scm:git:git@github.com:sangria-graphql/sangria.git"
))

// nice *magenta* prompt!

shellPrompt in ThisBuild := { state ⇒
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}