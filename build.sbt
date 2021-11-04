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

  // changed `Some` return to `Option`:
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildObjectType"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildInputObjectType"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildInterfaceType"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildUnionType"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildScalarType"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildEnumType"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildEnumValue"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildField"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildInputField"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildArgument"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.DefaultAstSchemaBuilder.buildDirective"),
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.schema.ResolverBasedAstSchemaBuilder.buildScalarType"),

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
  ProblemFilters.exclude[DirectMissingMethodProblem](
    "sangria.execution.ExecutionPath.cacheKeyPath"),
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
  ProblemFilters.exclude[MissingTypesProblem]("sangria.execution.ExecutionPath$"),

  // removed the `ast` package to its own library
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Argument"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Argument$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.AstLocation"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.AstLocation$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.AstNode"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.AstNode$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.BigDecimalValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.BigDecimalValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.BigIntValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.BigIntValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.BooleanValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.BooleanValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Comment"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Comment$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ConditionalFragment"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Definition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Directive"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Directive$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.DirectiveDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.DirectiveDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.DirectiveLocation"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.DirectiveLocation$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Document"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Document$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumTypeExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumValueDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.EnumValueDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Field"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Field$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FieldDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FieldDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FloatValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FloatValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FragmentDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FragmentDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FragmentSpread"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.FragmentSpread$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InlineFragment"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InlineFragment$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputDocument"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputDocument$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputObjectTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputObjectTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputObjectTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputObjectTypeExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputValueDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InputValueDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.IntValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.IntValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InterfaceTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InterfaceTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InterfaceTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.InterfaceTypeExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ListType"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ListType$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ListValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ListValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NameValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NamedType"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NamedType$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NotNullType"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NotNullType$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NullValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.NullValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectField"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectField$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectLikeTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectTypeExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ObjectValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationType"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationType$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationType$Mutation$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationType$Query$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationType$Subscription$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.OperationTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ScalarTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ScalarTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ScalarTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ScalarTypeExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.ScalarValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.SchemaAstNode"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.SchemaDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.SchemaDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.SchemaExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.SchemaExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Selection"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.SelectionContainer"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.StringValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.StringValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Type"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.TypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.TypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.TypeSystemDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.TypeSystemExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.UnionTypeDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.UnionTypeDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.UnionTypeExtensionDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.UnionTypeExtensionDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.Value"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.VariableDefinition"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.VariableDefinition$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.VariableValue"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.VariableValue$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.WithArguments"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.WithComments"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.WithDescription"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.WithDirectives"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.ast.WithTrailingComments"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.parser.AggregateSourceMapper"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.parser.AggregateSourceMapper$"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.parser.DefaultSourceMapper"),
  ProblemFilters.exclude[MissingClassProblem]("sangria.parser.SourceMapper"),

  // added type annotation
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("sangria.renderer.QueryRenderer.renderCommentLines"),
  ProblemFilters.exclude[IncompatibleResultTypeProblem]("sangria.renderer.QueryRenderer.renderCommentLines"),
)

lazy val root = project
  .in(file("."))
  .withId("sangria-root")
  .aggregate(ast, core, benchmarks)
  .settings(inThisBuild(projectInfo))
  .settings(
    scalacSettings ++ shellSettings ++ noPublishSettings
  )
  .disablePlugins(MimaPlugin)

lazy val ast = project.in(file("modules/ast"))
  .withId("sangria-ast")
  .settings(scalacSettings ++ shellSettings)
  .settings(
    name := "sangria-ast",
    description := "Scala GraphQL AST representation",

    //FIXME This really shouldn't depend on any other libraries.
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.3.0",
    )
  )

lazy val core = project
  .in(file("modules/core"))
  .withId("sangria-core")
  .dependsOn(ast)
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
      "org.sangria-graphql" %% "sangria-marshalling-api" % "1.0.7",
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
      "io.github.classgraph" % "classgraph" % "4.8.129" % Test
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
