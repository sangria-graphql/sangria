package sangria.execution

import language.{implicitConversions, existentials}

import sangria.ast
import sangria.integration.InputUnmarshaller
import sangria.schema.{Action, Context}

trait Middleware {
  type QueryVal

  def beforeQuery(context: MiddlewareQueryContext[_, _]): QueryVal
  def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[_, _]): Unit
}

trait MiddlewareBeforeField extends Middleware {
  type FieldVal

  def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]): (FieldVal, Option[Action[_, _]])

  lazy val continue: (Unit, Option[Action[_, _]]) = (Unit, None)
  def continue(fieldVal: FieldVal): (FieldVal, Option[Action[_, _]]) = (fieldVal, None)
}

trait MiddlewareAfterField extends Middleware with MiddlewareBeforeField {
  def afterField(queryVal: QueryVal, fieldVal: FieldVal, value: Any, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]): Option[Any]
}

trait MiddlewareErrorField extends Middleware with MiddlewareBeforeField {
  def fieldError(queryVal: QueryVal, fieldVal: FieldVal, error: Throwable, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]): Unit
}

case class MiddlewareQueryContext[Ctx, Input](
  executor: Executor[_, _],
  queryAst: ast.Document,
  operationName: Option[String],
  variables: Input,
  inputUnmarshaller: InputUnmarshaller[Input])

trait FieldTag

case class StringTag(name: String)

object FieldTag {
  implicit def stringTag(s: String): StringTag = StringTag(s)
  implicit def symbolTag(s: Symbol): StringTag = StringTag(s.name)
}


