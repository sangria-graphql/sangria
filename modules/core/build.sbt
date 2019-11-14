name := "sangria"
description := "Scala GraphQL implementation"

mimaPreviousArtifacts := Set("org.sangria-graphql" %% "sangria" % "1.4.2")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

libraryDependencies ++= Seq(
  // AST Parser
  "org.parboiled" %% "parboiled" % "2.1.8",

  // AST Visitor
  "org.sangria-graphql" %% "macro-visit" % "0.1.2",

  // Marshalling
  "org.sangria-graphql" %% "sangria-marshalling-api" % "1.0.4",

  // Streaming
  "org.sangria-graphql" %% "sangria-streaming-api" % "1.0.1",

  // Macros
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  // Testing
  "co.fs2" %% "fs2-core" % "2.1.0" % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.2" % Test,
  "org.sangria-graphql" %% "sangria-spray-json" % "1.0.2" % Test,
  "org.sangria-graphql" %% "sangria-argonaut" % "1.0.1" % Test,
  "org.sangria-graphql" %% "sangria-ion" % "2.0.0" % Test,
  "org.sangria-graphql" %% "sangria-monix" % "2.0.0" % Test,
  "eu.timepit" %% "refined" % "0.9.10" % Test,

  // CATs
  "net.jcazevedo" %% "moultingyaml" % "0.4.1" % Test,
  "io.github.classgraph" % "classgraph" % "4.8.53" % Test
)

// Publishing
releaseCrossBuild := true
releasePublishArtifactsAction := PgpKeys.publishSigned.value
publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := (_ => false)
publishTo := Some(
  if (isSnapshot.value)
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
