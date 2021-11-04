package sangria.parser

import org.parboiled2.ParserInput
import sangria.ast.{DefaultSourceMapper, SourceMapperInput}

import java.util.UUID

case class ParserConfig(
    experimentalFragmentVariables: Boolean = false,
    sourceIdFn: ParserInput => String = ParserConfig.defaultSourceIdFn,
    sourceMapperFn: ParserConfig.CodeSourceToSourceMapperFunction =
      ParserConfig.defaultSourceMapperFn,
    parseLocations: Boolean = true,
    parseComments: Boolean = true
) {
  def withExperimentalFragmentVariables: ParserConfig = copy(experimentalFragmentVariables = true)

  /** Return a new configuration that uses
    * [[ParserConfig#emptySourceIdFn the empty source ID generator]].
    */
  def withEmptySourceId: ParserConfig = copy(sourceIdFn = ParserConfig.emptySourceIdFn)

  def withSourceMapper(
      fn: (String, ParserInput) => Option[sangria.ast.SourceMapper]): ParserConfig =
    copy(sourceMapperFn = fn)

  /** Return a new configuration that uses
    * [[ParserConfig#emptySourceMapperFn the empty source mapper]].
    */
  def withoutSourceMapper: ParserConfig = copy(sourceMapperFn = ParserConfig.emptySourceMapperFn)

  def withoutLocations: ParserConfig = copy(parseLocations = false)

  def withoutComments: ParserConfig = copy(parseComments = false)
}

object ParserConfig {

  /** Mapping from a GraphQL code source to a `SourceMapper`.
    *
    * A [[SourceMapper]] provides methods for displaying GraphQL source code. This type is the type
    * of a function that returns a `SourceMapper`, given an identifier for the source code, and a
    * Parboiled2 `ParserInput` from which the source code will be extracted.
    *
    * In practice, the identifiers are typically randomly-generated, the input source is from a
    * string, and the chosen `SourceMapper` doesn't depend on either. In most cases, the
    * [[DefaultSourceMapper default]] is returned. This function type is mostly used to allow
    * Sangria users to replace the default with their own custom `SourceMapper`.
    */
  type CodeSourceToSourceMapperFunction = (String, ParserInput) => Option[sangria.ast.SourceMapper]

  /** Return the given Parboiled2 parser input, wrapped with our [[SourceMapperInput]].
    *
    * This utility method makes it easier to write quasiquotes.
    */
  private[sangria] def parboiledToSourceMapper(input: ParserInput): SourceMapperInput =
    new SourceMapperInput {
      override def source: String = input.sliceString(0, input.length)
      override def getLine(line: Int): String = input.getLine(line)
    }

  lazy val default: ParserConfig = ParserConfig()

  /** Function that always generates an empty identifier. */
  lazy val emptySourceIdFn: ParserInput => String = _ => ""

  /** Function that generates a random identifier for each input. */
  lazy val defaultSourceIdFn: ParserInput => String = _ => UUID.randomUUID().toString

  /** Function that returns no `SourceMapper`. */
  lazy val emptySourceMapperFn: CodeSourceToSourceMapperFunction = (_, _) => None

  /** Function that returns the [[DefaultSourceMapper default `SourceMapper`]]. */
  lazy val defaultSourceMapperFn: CodeSourceToSourceMapperFunction =
    (id, input) => Some(new DefaultSourceMapper(id, parboiledToSourceMapper(input)))
}
