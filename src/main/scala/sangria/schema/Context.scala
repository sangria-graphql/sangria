package sangria.schema

import language.implicitConversions

trait Op[+Ctx, +T]

object Op {
  implicit def defaultOp[Ctx, T](value: T): Op[Ctx, T] = Value(value)
}

case class Value[Ctx, Val](value: Val) extends Op[Ctx, Val]
case class UpdateCtx[Ctx, Val](newCtx: Ctx, value: Val) extends Op[Ctx, Val]

trait Deferred[Ctx, +T] extends Op[Ctx, T]

case class Context[Ctx, Val](value: Val, ctx: Ctx, args: Map[String, Any], schema: Schema[Ctx, Val]) {
  def arg[T](arg: Argument[T]) = args(arg.name).asInstanceOf[T]
  def argOpt[T](arg: Argument[T]) = args.get(arg.name).asInstanceOf[T]
}

trait DeferredResolver {
  def resolve(deferred: List[Deferred[_, _]]): List[Any]
}
