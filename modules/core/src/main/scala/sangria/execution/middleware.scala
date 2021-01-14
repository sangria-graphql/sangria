package sangria.execution

import language.implicitConversions
import sangria.marshalling.InputUnmarshaller
import sangria.ast
import sangria.effect.Effect
import sangria.schema.{Action, Context, InputType}
import sangria.streaming.SubscriptionStream
import sangria.validation.Violation

trait Middleware[-Ctx, F[_]] {
  type QueryVal

  def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _, F]): QueryVal
  def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _, F]): Unit
}

object Middleware {
  def composeFromScalarMiddleware[Ctx, F[_]](
      middleware: List[Middleware[Ctx, F]],
      userContext: Ctx): Option[(Any, InputType[_]) => Option[Either[Violation, Any]]] = {
    val relevant =
      middleware.collect { case m: MiddlewareFromScalar[Ctx, F] =>
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

  def composeToScalarMiddleware[Ctx, F[_]](
      middleware: List[Middleware[Ctx, F]],
      userContext: Ctx): Option[(Any, InputType[_]) => Option[Any]] = {
    val relevant =
      middleware.collect { case m: MiddlewareToScalar[Ctx, F] =>
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

  def simpleExtension[Ctx, F[_]](
      extensionFn: MiddlewareQueryContext[Ctx, _, _, F] => ast.Value): Middleware[Ctx, F] =
    new SimpleAstBasedExtensionMiddleware[Ctx, F](extensionFn)
}

trait MiddlewareBeforeField[Ctx, F[_]] extends Middleware[Ctx, F] {
  type FieldVal

  def beforeField(
      queryVal: QueryVal,
      mctx: MiddlewareQueryContext[Ctx, _, _, F],
      ctx: Context[Ctx, _, F]): BeforeFieldResult[Ctx, FieldVal, F]

  lazy val continue: BeforeFieldResult[Ctx, Unit, F] = BeforeFieldResult(())
  def continue(fieldVal: FieldVal): BeforeFieldResult[Ctx, FieldVal, F] = BeforeFieldResult(
    fieldVal)
  def overrideAction(actionOverride: Action[Ctx, _, F]): BeforeFieldResult[Ctx, Unit, F] =
    BeforeFieldResult((), Some(actionOverride))
}

trait MiddlewareAfterField[Ctx, F[_]] extends MiddlewareBeforeField[Ctx, F] {
  def afterField(
      queryVal: QueryVal,
      fieldVal: FieldVal,
      value: Any,
      mctx: MiddlewareQueryContext[Ctx, _, _, F],
      ctx: Context[Ctx, _, F]): Option[Any]
}

trait MiddlewareToScalar[Ctx, F[_]] extends Middleware[Ctx, F] {
  def toScalar(value: Any, inputType: InputType[_], ctx: Ctx): Option[Any]
}

trait MiddlewareFromScalar[Ctx, F[_]] extends Middleware[Ctx, F] {
  def fromScalar(value: Any, inputType: InputType[_], ctx: Ctx): Option[Either[Violation, Any]]
}

trait MiddlewareErrorField[Ctx, F[_]] extends MiddlewareBeforeField[Ctx, F] {
  def fieldError(
      queryVal: QueryVal,
      fieldVal: FieldVal,
      error: Throwable,
      mctx: MiddlewareQueryContext[Ctx, _, _, F],
      ctx: Context[Ctx, _, F]): Unit
}

trait MiddlewareExtension[Ctx, F[_]] extends Middleware[Ctx, F] {
  def afterQueryExtensions(
      queryVal: QueryVal,
      context: MiddlewareQueryContext[Ctx, _, _, F]): Vector[Extension[_]]
}

case class MiddlewareQueryContext[+Ctx, RootVal, Input, F[_]: Effect](
    ctx: Ctx,
    executor: Executor[_ <: Ctx, RootVal, F],
    queryAst: ast.Document,
    operationName: Option[String],
    variables: Input,
    inputUnmarshaller: InputUnmarshaller[Input],
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement)

case class BeforeFieldResult[Ctx, FieldVal, F[_]](
    fieldVal: FieldVal = (),
    actionOverride: Option[Action[Ctx, _, F]] = None,
    attachment: Option[MiddlewareAttachment] = None)

trait MiddlewareAttachment

object BeforeFieldResult {
  // backwards compatibility
  implicit def fromTuple2[Ctx, FieldVal, F[_]](
      tuple: (FieldVal, Option[Action[Ctx, _, F]])): BeforeFieldResult[Ctx, FieldVal, F] =
    BeforeFieldResult(tuple._1, tuple._2)

  def attachment[Ctx, F[_]](a: MiddlewareAttachment): BeforeFieldResult[Ctx, Unit, F] =
    BeforeFieldResult[Ctx, Unit, F]((), attachment = Some(a))
}

trait FieldTag

case class StringTag(name: String)

object FieldTag {
  implicit def stringTag(s: String): StringTag = StringTag(s)
  implicit def symbolTag(s: Symbol): StringTag = StringTag(s.name)
}

case class SubscriptionField[S[_]](stream: SubscriptionStream[S]) extends FieldTag

case class Extension[In](data: In)(implicit val iu: InputUnmarshaller[In])
