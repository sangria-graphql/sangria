package sangria

/** Scala classes to represent the GraphQL AST (abstract syntax tree).
  *
  * Though we call them “AST”, these types represent something closer to a concrete syntax tree,
  * since they are close to the original parse and can hold information about source code location
  * and comments.
  *
  * The root of the AST is the [[Document]] type, so that would be the place to start understanding
  * the AST. The AST closely follows the
  * [[https://spec.graphql.org/June2018/#sec-Language specification]].
  *
  * The GraphQL language includes an
  * [[https://en.wikipedia.org/wiki/Interface_description_language IDL]]
  * ([[https://spec.graphql.org/June2018/#sec-Type-System the type system definition]]) used to
  * describe a GraphQL service’s type system. Tools may use this IDL to provide utilities such as
  * client code generation or service boot‐strapping. The Scala types that represent the parsed
  * nodes of the IDL constitute almost half of the Scala types in this package. Tools which only
  * seek to provide GraphQL query execution may not need to parse the type system definition nodes.
  *
  * @groupname value
  *   Value nodes
  * @groupdesc value
  *   Types that represent input value nodes in the GraphQL AST.
  * @groupprio value
  *   1
  *
  * @groupname scalar
  *   Scalar value nodes
  * @groupdesc scalar
  *   Types that represent scalar input value nodes in the GraphQL AST.
  * @groupprio scalar
  *   2
  *
  * @groupname typesystem
  *   Type system definition nodes
  * @groupdesc typesystem
  *   The GraphQL type system describes the capabilities of a GraphQL server and is used to
  *   determine if a query is valid. The type system also describes the input types of query
  *   variables to determine if values provided at runtime are valid. The GraphQL type system for a
  *   Sangria server is defined in terms of these classes, with the [[SchemaDefinition]] being the
  *   most important top-level class.
  * @groupprio typesystem
  *   3
  */
package object ast {}
