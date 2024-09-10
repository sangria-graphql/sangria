package sangria.execution

import language.implicitConversions
import sangria.marshalling.InputUnmarshaller

import sangria.ast
import sangria.schema.{Action, Context, InputType}
import sangria.streaming.SubscriptionStream
import sangria.validation.Violation

trait Middleware[-Ctx] {
  type QueryVal

  def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _]): QueryVal
  def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _]): Unit
}

object Middleware {
  def composeFromScalarMiddleware[Ctx](
      middleware: List[Middleware[Ctx]],
      userContext: Ctx): Option[(Any, InputType[_]) => Option[Either[Violation, Any]]] = {
    val relevant =
      middleware.collect { case m: MiddlewareFromScalar[Ctx] =>
        m
      }

    if (relevant.nonEmpty)
      Some { (v, tpe) =>
        var changed = false
        var violation: Violation = null

        val newValue =
          relevant.foldLeft(v) {
            case (acc, _) if violation != null => acc
            case (acc, m) =>
              m.fromScalar(acc, tpe, userContext) match {
                case Some(Left(viol)) =>
                  violation = viol
                  acc
                case Some(Right(newAcc)) =>
                  changed = true
                  newAcc
                case None =>
                  acc
              }
          }

        if (violation != null) Some(Left(violation))
        else if (changed) Some(Right(newValue))
        else None
      }
    else None
  }

  def composeToScalarMiddleware[Ctx](
      middleware: List[Middleware[Ctx]],
      userContext: Ctx): Option[(Any, InputType[_]) => Option[Any]] = {
    val relevant =
      middleware.collect { case m: MiddlewareToScalar[Ctx] =>
        m
      }

    if (relevant.nonEmpty)
      Some { (v, tpe) =>
        var changed = false

        val newValue =
          relevant.foldRight(v) { case (m, acc) =>
            m.toScalar(acc, tpe, userContext) match {
              case Some(newAcc) =>
                changed = true
                newAcc
              case None => acc
            }
          }

        if (changed) Some(newValue)
        else None
      }
    else None
  }

  def simpleExtension[Ctx](
      extensionFn: MiddlewareQueryContext[Ctx, _, _] => ast.Value): Middleware[Ctx] =
    new SimpleAstBasedExtensionMiddleware[Ctx](extensionFn)
}

trait MiddlewareBeforeField[Ctx] extends Middleware[Ctx] {
  type FieldVal

  def beforeField(
      queryVal: QueryVal,
      mctx: MiddlewareQueryContext[Ctx, _, _],
      ctx: Context[Ctx, _]): BeforeFieldResult[Ctx, FieldVal]

  lazy val continue: BeforeFieldResult[Ctx, Unit] = BeforeFieldResult(())
  def continue(fieldVal: FieldVal): BeforeFieldResult[Ctx, FieldVal] = BeforeFieldResult(fieldVal)
  def overrideAction(actionOverride: Action[Ctx, _]): BeforeFieldResult[Ctx, Unit] =
    BeforeFieldResult((), Some(actionOverride))
}

trait MiddlewareAfterField[Ctx] extends MiddlewareBeforeField[Ctx] {
  def afterField(
      queryVal: QueryVal,
      fieldVal: FieldVal,
      value: Any,
      mctx: MiddlewareQueryContext[Ctx, _, _],
      ctx: Context[Ctx, _]): Option[Any]
}

trait MiddlewareToScalar[Ctx] extends Middleware[Ctx] {
  def toScalar(value: Any, inputType: InputType[_], ctx: Ctx): Option[Any]
}

trait MiddlewareFromScalar[Ctx] extends Middleware[Ctx] {
  def fromScalar(value: Any, inputType: InputType[_], ctx: Ctx): Option[Either[Violation, Any]]
}

trait MiddlewareErrorField[Ctx] extends MiddlewareBeforeField[Ctx] {
  def fieldError(
      queryVal: QueryVal,
      fieldVal: FieldVal,
      error: Throwable,
      mctx: MiddlewareQueryContext[Ctx, _, _],
      ctx: Context[Ctx, _]): Unit
}

trait MiddlewareExtension[Ctx] extends Middleware[Ctx] {
  def afterQueryExtensions(
      queryVal: QueryVal,
      context: MiddlewareQueryContext[Ctx, _, _]): Vector[Extension[_]]
}

case class MiddlewareQueryContext[+Ctx, RootVal, Input](
    ctx: Ctx,
    executor: Executor[_ <: Ctx, RootVal],
    queryAst: ast.Document,
    operationName: Option[String],
    variables: Input,
    inputUnmarshaller: InputUnmarshaller[Input],
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement)

case class BeforeFieldResult[Ctx, FieldVal](
    fieldVal: FieldVal = (),
    actionOverride: Option[Action[Ctx, _]] = None,
    attachment: Option[MiddlewareAttachment] = None)

trait MiddlewareAttachment

object BeforeFieldResult {
  // backwards compatibility
  implicit def fromTuple2[Ctx, FieldVal](
      tuple: (FieldVal, Option[Action[Ctx, _]])): BeforeFieldResult[Ctx, FieldVal] =
    BeforeFieldResult(tuple._1, tuple._2)

  def attachment[Ctx](a: MiddlewareAttachment): BeforeFieldResult[Ctx, Unit] =
    BeforeFieldResult[Ctx, Unit]((), attachment = Some(a))
}

trait FieldTag

case class StringTag(name: String)

object FieldTag {
  implicit def stringTag(s: String): StringTag = StringTag(s)
  implicit def symbolTag(s: Symbol): StringTag = StringTag(s.name)
}

case class SubscriptionField[S[_]](stream: SubscriptionStream[S]) extends FieldTag

case class Extension[In](data: In)(implicit val iu: InputUnmarshaller[In])
