name := "sangria"
organization := "org.sangria-graphql"
version := "1.3.1-SNAPSHOT"

description := "Scala GraphQL implementation"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq("Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.12.3"
crossScalaVersions := Seq("2.11.11", "2.12.3")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Xlint:-missing-interpolator,_")

scalacOptions ++= {
  if (scalaVersion.value startsWith "2.12")
    Seq.empty
  else
    Seq("-target:jvm-1.7")
}

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.4",
  "org.sangria-graphql" %% "macro-visit" % "0.1.1",

  // macros
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  // marshalling
  "org.sangria-graphql" %% "sangria-marshalling-api" % "1.0.0",

  // streaming
  "org.sangria-graphql" %% "sangria-streaming-api" % "1.0.0",

  // testing
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.0" % Test,
  "org.sangria-graphql" %% "sangria-spray-json" % "1.0.0" % Test,
  "org.sangria-graphql" %% "sangria-argonaut" % "1.0.0" % Test,

  "org.sangria-graphql" %% "sangria-ion" % "1.0.0" % Test,
  "org.sangria-graphql" %% "sangria-monix" % "1.0.0" % Test,
  "org.sangria-graphql" %% "sangria-rxscala" % "1.0.0" % Test,
  "eu.timepit" %% "refined" % "0.8.2" % Test,

  // CATs
  "net.jcazevedo" %% "moultingyaml" % "0.4.0" % Test,
  "io.github.lukehutch" % "fast-classpath-scanner" % "2.4.5" % Test
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
