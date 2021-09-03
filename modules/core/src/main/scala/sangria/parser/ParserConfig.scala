package sangria.parser

import org.parboiled2.ParserInput

import java.util.UUID

case class ParserConfig(
  legacyImplementsInterface: Boolean = false,
  legacyEmptyFields: Boolean = false,
  experimentalFragmentVariables: Boolean = false,
  sourceIdFn: ParserInput => String = ParserConfig.defaultSourceIdFn,
  sourceMapperFn: (String, ParserInput) => Option[SourceMapper] = ParserConfig.defaultSourceMapperFn,
  parseLocations: Boolean = true,
  parseComments: Boolean = true
) {
  @deprecated("Use new syntax: `type Foo implements Bar & Baz`", "1.4.0")
  def withLegacyImplementsInterface: ParserConfig = copy(legacyImplementsInterface = true)

  @deprecated("Use new syntax: `type Foo` instead of legacy `type Foo {}`", "1.4.0")
  def withLegacyEmptyFields: ParserConfig = copy(legacyEmptyFields = true)

  def withExperimentalFragmentVariables: ParserConfig = copy(experimentalFragmentVariables = true)

  /** Return a new configuration that uses [[ParserConfig#emptySourceIdFn the empty source ID generator]]. */
  def withEmptySourceId: ParserConfig = copy(sourceIdFn = ParserConfig.emptySourceIdFn)

  def withSourceMapper(fn: (String, ParserInput) => Option[SourceMapper]): ParserConfig =
    copy(sourceMapperFn = fn)

  /** Return a new configuration that uses [[ParserConfig#emptySourceMapperFn the empty source mapper]]. */
  def withoutSourceMapper: ParserConfig = copy(sourceMapperFn = ParserConfig.emptySourceMapperFn)

  def withoutLocations: ParserConfig = copy(parseLocations = false)

  def withoutComments: ParserConfig = copy(parseComments = false)
}

object ParserConfig {
  lazy val default: ParserConfig = ParserConfig()

  /** Function that always generates an empty identifier. */
  lazy val emptySourceIdFn: ParserInput => String = _ => ""

  /** Function that generates a random identifier for each input. */
  lazy val defaultSourceIdFn: ParserInput => String = _ => UUID.randomUUID().toString

  lazy val emptySourceMapperFn: (String, ParserInput) => Option[SourceMapper] = (_, _) => None
  lazy val defaultSourceMapperFn: (String, ParserInput) => Option[SourceMapper] =
    (id, input) => Some(new DefaultSourceMapper(id, input))
}
