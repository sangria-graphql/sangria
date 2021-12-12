package sangria.schema

import sangria.execution.deferred.Deferred

case class MappingDeferred[A, +B](deferred: Deferred[A], mapFn: A => (B, Vector[Throwable]))
    extends Deferred[B]
