package sangria.execution.deferred

import sangria.ast
import sangria.execution.DeferredWithInfo
import sangria.schema.{Args, Field}

import scala.concurrent.{ExecutionContext, Future}

trait DeferredResolver[-Ctx] {
  def includeDeferredFromField: Option[(Field[_, _], Vector[ast.Field], Args, Double) ⇒ Boolean] = None

  def groupDeferred[T <: DeferredWithInfo](deferred: Vector[T]): Vector[Vector[T]] =
    Vector(deferred)

  def initialQueryState: Any = ()

  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx, queryState: Any)(implicit ec: ExecutionContext): Vector[Future[Any]]
}

object DeferredResolver {
  val empty = new DeferredResolver[Any] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) =
      deferred map (d ⇒ Future.failed(UnsupportedDeferError(d)))
  }

  def fetchers[Ctx](fetchers: Fetcher[Ctx, _, _]*): DeferredResolver[Ctx] =
    new FetcherBasedDeferredResolver[Ctx](fetchers.toVector)
}

trait Deferred[+T]

trait DeferredOne[Id, +T] extends Deferred[T] {
  def id: Id
}

trait DeferredOpt[Id, +T] extends Deferred[Option[T]] {
  def id: Id
}

trait DeferredSeq[Id, +T] extends Deferred[Seq[T]] {
  def ids: Seq[Id]
}

case class UnsupportedDeferError(deferred: Deferred[Any])
    extends Exception(s"Deferred resolver is not defined for deferred value: $deferred.")
