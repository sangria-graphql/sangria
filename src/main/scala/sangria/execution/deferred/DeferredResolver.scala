package sangria.execution.deferred

import sangria.ast
import sangria.execution.DeferredWithInfo
import sangria.schema.{Args, Field}

import scala.concurrent.Future

trait DeferredResolver[-Ctx] {
  def includeDeferredFromField: Option[(Field[_, _], Vector[ast.Field], Args, Double) ⇒ Boolean] = None

  def groupDeferred[T <: DeferredWithInfo](deferred: Vector[T]): Vector[Vector[T]] =
    Vector(deferred)

  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx): Vector[Future[Any]]
}

object DeferredResolver {
  val empty = new DeferredResolver[Any] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any) = deferred map (_ ⇒ Future.failed(UnsupportedDeferError))
  }
}

trait Deferred[+T]

case object UnsupportedDeferError extends Exception
