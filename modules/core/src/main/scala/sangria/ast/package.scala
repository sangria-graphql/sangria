package sangria

/** Scala classes to represent the GraphQL AST (abstract syntax tree).
  *
  * The root of the AST is the [[Document]] type, so that would be the place to start understanding
  * the AST. The AST closely follows the
  * [[https://spec.graphql.org/June2018/#sec-Language specification]].
  *
  * @groupname value
  *   Value Nodes
  * @groupdesc value
  *   Types that represent input value nodes in the GraphQL AST.
  * @groupprio value
  *   1
  *
  * @groupname scalar
  *   Scalar Value Nodes
  * @groupdesc scalar
  *   Types that represent scalar input value nodes in the GraphQL AST.
  * @groupprio scalar
  *   2
  */
package object ast {}
