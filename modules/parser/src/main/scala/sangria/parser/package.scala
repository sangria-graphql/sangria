package sangria

/** GraphQL parser for Scala.
  *
  * A parser for the GraphQL language. It returns nodes from its corresponding AST library.
  *
  * The entry point of this library is the [[QueryParser]] object's methods.
  *
  * @see
  *   The parser conforms roughly to the [[https://spec.graphql.org/June2018/ June 2018 spec]].
  */
package object parser {

  @deprecated("please use `sangria.ast.SourceMapper`", "3.0.0")
  type SourceMapper = sangria.ast.SourceMapper
}
