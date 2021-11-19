/** Sangria GraphQL server library.
  *
  * Sangria is a library that provides parsing, validation, execution and other services for GraphQL
  * queries.
  *
  * It typically requires other libraries to build a complete GraphQL service—including, perhaps,
  * ones that provide a HTTP service interface and a database interface—as well as custom code to
  * bridge the gap between data representations that are natural to the database vs. to the GraphQL
  * schema.
  *
  * @see
  *   [[https://sangria-graphql.github.io/ the Sangria home page]]
  */
package object sangria {

  /** "Since" field for 3.0.0 deprecations. */
  private[sangria] final val since3_0_0 = "Sangria 3.0.0"
}
