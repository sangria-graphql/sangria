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
  IncompatibleMethTypeProblem,
  IncompatibleResultTypeProblem,
  MissingClassProblem,
  MissingTypesProblem,
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
    "sangria.marshalling.QueryAstInputUnmarshaller.getMapKeys"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.validation.rules.AstAndDef"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.validation.rules.AstAndDef$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.validation.rules.Conflict"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.validation.rules.Conflict$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.validation.rules.PairSet"),
  ProblemFilters.exclude[Problem]("sangria.validation.rules.experimental.*"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.AstSchemaBuilder.defaultWithLegacyCommentDescriptions"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.useLegacyCommentDescriptions"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.schema.LegacyCommentDescriptionsResolver"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.schema.LegacyCommentDescriptionsResolver$"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.ResolverBasedAstSchemaBuilder.useLegacyCommentDescriptions"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.SchemaFilter.<init>$default$4"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.SchemaFilter.apply$default$4"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.renderer.SchemaFilter.apply"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.SchemaFilter.legacyCommentDescriptions"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.SchemaFilter.withLegacyCommentDescriptions"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.renderer.SchemaFilter.copy"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.SchemaFilter.copy$default$4"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.renderer.SchemaFilter.this"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.SchemaRenderer.transformLegacyCommentDescriptions"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.parser.ParserConfig.apply$default$*"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem](
    "sangria.parser.ParserConfig.apply$default$*"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.ParserConfig.<init>$default$*"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem](
    "sangria.parser.ParserConfig.<init>$default$*"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.parser.ParserConfig.apply"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.ParserConfig.legacyImplementsInterface"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.ParserConfig.withLegacyImplementsInterface"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.parser.ParserConfig.copy"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem](
    "sangria.parser.ParserConfig.copy$default$*"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.parser.ParserConfig.copy$default$*"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.parser.ParserConfig.this"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.QueryParser.legacyImplementsInterface"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.parser.QueryParser.this"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.renderer.QueryRendererConfig.apply"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.QueryRendererConfig.legacyImplementsInterface"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.renderer.QueryRendererConfig.copy"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.renderer.QueryRendererConfig.copy$default$*"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.renderer.QueryRendererConfig.this"),
  ProblemFilters.exclude[MissingTypesProblem]("sangria.renderer.QueryRendererConfig$"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.ParserConfig.legacyEmptyFields"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.ParserConfig.withLegacyEmptyFields"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.parser.QueryParser.legacyEmptyFields"),
  ProblemFilters.exclude[MissingTypesProblem]("sangria.execution.ExecutionPath"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.execution.ExecutionPath.unapply"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.execution.ExecutionPath.apply"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.productElementNames"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("sangria.execution.ExecutionPath.path"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.cacheKeyPath"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("sangria.execution.ExecutionPath.cacheKey"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.execution.ExecutionPath.copy"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.copy$default$*"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.productPrefix"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.productArity"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.productElement"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.productIterator"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.execution.ExecutionPath.canEqual"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.productElementName"),
  ProblemFilters.exclude[IncompatibleMethTypeProblem]("sangria.execution.ExecutionPath.this"),
  ProblemFilters.exclude[DirectMissingMethodProblem]("sangria.execution.ExecutionPath.this"),
  ProblemFilters.exclude[MissingTypesProblem]("sangria.execution.ExecutionPath$")
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
      "co.fs2" %% "fs2-core" % "2.5.10" % Test,
      "org.scalatest" %% "scalatest" % "3.2.10" % Test,
      "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.4" % Test,
      "org.sangria-graphql" %% "sangria-spray-json" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-argonaut" % "1.0.2" % Test,
      "org.sangria-graphql" %% "sangria-ion" % "2.0.1" % Test,
      "org.sangria-graphql" %% "sangria-monix" % "2.0.1" % Test,
      "eu.timepit" %% "refined" % "0.9.27" % Test,
      // CATs
      "net.jcazevedo" %% "moultingyaml" % "0.4.2" % Test,
      "io.github.classgraph" % "classgraph" % "4.8.128" % Test
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
