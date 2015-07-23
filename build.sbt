name := "sangria"
organization := "com.github.olegilyenko"
version := "0.0.2-SNAPSHOT"

description := "Scala GraphQL server and client library"
homepage := Some(url("https://github.com/OlegIlyenko/sangria"))
licenses := Seq("Apache License, ASL Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.11.7"
scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.parboiled" %% "parboiled" % "2.1.0",
  "io.spray" %%  "spray-json" % "1.3.2" % "optional", // todo extract in different library
  "org.json4s" %% "json4s-native" % "3.2.11" % "optional", // todo extract in different library
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

git.remoteRepo := "git@github.com:OlegIlyenko/sangria.git"

// Publishing

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := (_ => false)
publishTo := Some(
  if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

// Site and docs

site.settings
site.includeScaladoc()
ghpages.settings

// nice *magenta* prompt!

shellPrompt in ThisBuild := { state =>
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}

// Additional meta-info

startYear := Some(2015)
organizationHomepage := Some(url("https://github.com/OlegIlyenko/sangria"))
scmInfo := Some(ScmInfo(
  browseUrl = url("https://github.com/OlegIlyenko/sangria.git"),
  connection = "scm:git:git@github.com:OlegIlyenko/sangria.git"
))
pomExtra := <xml:group>
  <developers>
    <developer>
      <id>OlegIlyenko</id>
      <name>Oleg Ilyenko</name>
    </developer>
  </developers>
</xml:group>