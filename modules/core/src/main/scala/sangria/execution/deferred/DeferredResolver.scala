package sangria.execution.deferred

import sangria.ast
import sangria.effect.Effect
import sangria.execution.DeferredWithInfo
import sangria.schema.{Args, Field}

trait DeferredResolver[-Ctx, F[_]] {

  def includeDeferredFromField
      : Option[(Field[_, _, F], Vector[ast.Field], Args, Double) => Boolean] =
    None

  def groupDeferred[T <: DeferredWithInfo[F]](deferred: Vector[T]): Vector[Vector[T]] =
    Vector(deferred)

  def initialQueryState: Any = ()

  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx, queryState: Any): Vector[F[Any]]
}

object DeferredResolver {
  def empty[F[_]: Effect] = new DeferredResolver[Any, F] {
    override def resolve(
        deferred: Vector[Deferred[Any]],
        ctx: Any,
        queryState: Any): Vector[F[Any]] =
      deferred.map(d => Effect[F]().failed(UnsupportedDeferError(d)))
  }

  def fetchers[Ctx, F[_]](fetchers: Fetcher[Ctx, _, _, _]*): DeferredResolver[Ctx, F] =
    new FetcherBasedDeferredResolver[Ctx, F](fetchers.toVector, None)

  def fetchersWithFallback[Ctx, F[_]](
      fallback: DeferredResolver[Ctx, F],
      fetchers: Fetcher[Ctx, _, _, _]*): DeferredResolver[Ctx, F] =
    new FetcherBasedDeferredResolver[Ctx, F](fetchers.toVector, Some(fallback))
}

trait Deferred[+T]

case class UnsupportedDeferError(deferred: Deferred[Any])
    extends Exception(s"Deferred resolver is not defined for deferred value: $deferred.")
