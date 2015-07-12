package sangria.schema

import language.implicitConversions

trait Deferred[+T]

object Deferred {
  implicit def defaultDefer[T](value: T): Deferred[T] = Eager(value)
}

case class Eager[T](value: T) extends Deferred[T]

case class Context[Ctx, Val](value: Val, ctx: Ctx, args: Map[String, Any]) {
  def arg[T](arg: Argument[T]) = args(arg.name).asInstanceOf[T]
  def argOpt[T](arg: Argument[T]) = args.get(arg.name).asInstanceOf[T]
}

trait DeferredResolver {
  def resolve(deferred: List[Deferred[_]]): List[Any]
}
