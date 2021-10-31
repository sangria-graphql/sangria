package sangria.ast

/** A type of [[OperationDefinition operation]] in a GraphQL model.
  *
  * Every GraphQL operation is either a [[OperationType#Query query]],
  * [[OperationType#Mutation mutation]] or [[OperationType#Subscription subscription]].
  *
  * @see
  *   [[https://spec.graphql.org/June2018/#OperationType]]
  */
sealed trait OperationType

object OperationType {

  /** A read-only fetch. */
  case object Query extends OperationType

  /** A write followed by a fetch. */
  case object Mutation extends OperationType

  /** A long‐lived request that fetches data in response to source events. */
  case object Subscription extends OperationType
}
