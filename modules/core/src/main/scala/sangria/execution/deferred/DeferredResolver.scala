package sangria.execution.deferred

import sangria.ast
import sangria.execution.DeferredWithInfo
import sangria.schema.{Args, Field}

import com.twitter.util.Future

trait DeferredResolver[-Ctx] {
  def includeDeferredFromField: Option[(Field[_, _], Vector[ast.Field], Args, Double) => Boolean] =
    None

  def groupDeferred[T <: DeferredWithInfo](deferred: Vector[T]): Vector[Vector[T]] =
    Vector(deferred)

  def initialQueryState: Any = ()

  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx, queryState: Any): Vector[Future[Any]]
}

object DeferredResolver {
  val empty = new DeferredResolver[Any] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any) =
      deferred.map(d => Future.exception(UnsupportedDeferError(d)))
  }

  def fetchers[Ctx](fetchers: Fetcher[Ctx, _, _, _]*): DeferredResolver[Ctx] =
    new FetcherBasedDeferredResolver[Ctx](fetchers.toVector, None)

  def fetchersWithFallback[Ctx](
      fallback: DeferredResolver[Ctx],
      fetchers: Fetcher[Ctx, _, _, _]*): DeferredResolver[Ctx] =
    new FetcherBasedDeferredResolver[Ctx](fetchers.toVector, Some(fallback))
}

trait Deferred[+T]

case class UnsupportedDeferError(deferred: Deferred[Any])
    extends Exception(s"Deferred resolver is not defined for deferred value: $deferred.")
