package sangria.schema

import sangria.effect.Effect
import sangria.effect.Effect._

import language.implicitConversions
import sangria.execution._
import sangria.marshalling._
import sangria.parser.SourceMapper
import sangria.{ast, introspection}
import sangria.execution.deferred.Deferred
import sangria.streaming.SubscriptionStream
import sangria.util.Cache

import scala.reflect.ClassTag
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

sealed trait Action[+Ctx, +Val, F[_]] {
  def map[NewVal](fn: Val => NewVal): Action[Ctx, NewVal, F]
}
sealed trait LeafAction[+Ctx, +Val, F[_]] extends Action[Ctx, Val, F] {
  def map[NewVal](fn: Val => NewVal): LeafAction[Ctx, NewVal, F]
}
sealed trait ReduceAction[+Ctx, +Val, F[_]] extends Action[Ctx, Val, F] {
  def map[NewVal](fn: Val => NewVal): LeafAction[Ctx, NewVal, F]
}

object ReduceAction {
  implicit def futureAction[Ctx, Val, F[_]: Effect](value: F[Val]): ReduceAction[Ctx, Val, F] =
    FutureValue(value)
  implicit def tryAction[Ctx, Val](value: Try[Val]): ReduceAction[Ctx, Val, Any] = TryValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): ReduceAction[Ctx, Val, Any] = Value(value)
}

object Action extends LowPrioActions {
  def sequence[Ctx, Val, F[_]](
      actions: Seq[LeafAction[Ctx, Val, F]]): SequenceLeafAction[Ctx, Val, F] =
    SequenceLeafAction[Ctx, Val, F](actions)

  def apply[Ctx, Val, F[_]](a: Action[Ctx, Val, F]): Action[Ctx, Val, F] = a

  implicit def deferredAction[Ctx, Val, F[_]](value: Deferred[Val]): LeafAction[Ctx, Val, F] =
    DeferredValue(value)
  implicit def tryAction[Ctx, Val, F[_]](value: Try[Val]): LeafAction[Ctx, Val, F] = TryValue(value)
}

trait LowPrioActions extends LowestPrioActions {
  implicit def deferredFutureAction[Ctx, Val, F[_]: Effect](
      value: F[Deferred[Val]]): LeafAction[Ctx, Val, F] =
    DeferredFutureValue(value)
}

trait LowestPrioActions {
  implicit def futureAction[Ctx, Val, F[_]: Effect](value: F[Val]): LeafAction[Ctx, Val, F] =
    FutureValue(value)
  implicit def defaultAction[Ctx, Val, F[_]](value: Val): LeafAction[Ctx, Val, F] = Value(value)
}

object LeafAction {
  def sequence[Ctx, Val, F[_]](
      actions: Seq[LeafAction[Ctx, Val, F]]): SequenceLeafAction[Ctx, Val, F] =
    SequenceLeafAction[Ctx, Val, F](actions)

  def apply[Ctx, Val, F[_]](a: LeafAction[Ctx, Val, F]) = a
}

case class Value[Ctx, Val, F[_]](value: Val)
    extends LeafAction[Ctx, Val, F]
    with ReduceAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): LeafAction[Ctx, NewVal, F] =
    try Value(fn(value))
    catch {
      case NonFatal(e) => TryValue(Failure(e))
    }
}

case class TryValue[Ctx, Val, F[_]](value: Try[Val])
    extends LeafAction[Ctx, Val, F]
    with ReduceAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): TryValue[Ctx, NewVal, F] =
    TryValue(value.map(fn))
}

case class PartialValue[Ctx, Val, F[_]](value: Val, errors: Vector[Throwable])
    extends LeafAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): LeafAction[Ctx, NewVal, F] =
    try PartialValue(fn(value), errors)
    catch {
      case NonFatal(e) => TryValue(Failure(e))
    }
}

case class FutureValue[Ctx, Val, F[_]: Effect](value: F[Val])
    extends LeafAction[Ctx, Val, F]
    with ReduceAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): FutureValue[Ctx, NewVal, F] =
    FutureValue(value.map(fn))
}

case class PartialFutureValue[Ctx, Val, F[_]: Effect](value: F[PartialValue[Ctx, Val, F]])
    extends LeafAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): PartialFutureValue[Ctx, NewVal, F] =
    PartialFutureValue(value.map(_.map(fn) match {
      case v: PartialValue[Ctx, NewVal, F] => v
      case TryValue(Failure(e)) => throw e
      case v => throw new IllegalStateException("Unexpected result from `PartialValue.map`: " + v)
    }))
}

case class DeferredValue[Ctx, Val, F[_]](value: Deferred[Val]) extends LeafAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): DeferredValue[Ctx, NewVal, F] =
    DeferredValue(MappingDeferred(value, (v: Val) => (fn(v), Vector.empty)))

  def mapWithErrors[NewVal](fn: Val => (NewVal, Vector[Throwable])): DeferredValue[Ctx, NewVal, F] =
    DeferredValue(MappingDeferred(value, fn))
}

case class DeferredFutureValue[Ctx, Val, F[_]: Effect](value: F[Deferred[Val]])
    extends LeafAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): DeferredFutureValue[Ctx, NewVal, F] =
    DeferredFutureValue(value.map(MappingDeferred(_, (v: Val) => (fn(v), Vector.empty))))

  def mapWithErrors[NewVal](
      fn: Val => (NewVal, Vector[Throwable])): DeferredFutureValue[Ctx, NewVal, F] =
    DeferredFutureValue(value.map(MappingDeferred(_, fn)))
}

case class SequenceLeafAction[Ctx, Val, F[_]](value: Seq[LeafAction[Ctx, Val, F]])
    extends LeafAction[Ctx, Seq[Val], F] {
  override def map[NewVal](fn: Seq[Val] => NewVal): MappedSequenceLeafAction[Ctx, Val, NewVal, F] =
    new MappedSequenceLeafAction[Ctx, Val, NewVal, F](this, fn)
}

class MappedSequenceLeafAction[Ctx, Val, NewVal, F[_]](
    val action: SequenceLeafAction[Ctx, Val, F],
    val mapFn: Seq[Val] => NewVal)
    extends LeafAction[Ctx, NewVal, F] {
  override def map[NewNewVal](
      fn: NewVal => NewNewVal): MappedSequenceLeafAction[Ctx, Val, NewNewVal, F] =
    new MappedSequenceLeafAction[Ctx, Val, NewNewVal, F](action, v => fn(mapFn(v)))
}

class UpdateCtx[Ctx, Val, F[_]](val action: LeafAction[Ctx, Val, F], val nextCtx: Val => Ctx)
    extends Action[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): MappedUpdateCtx[Ctx, Val, NewVal, F] =
    new MappedUpdateCtx[Ctx, Val, NewVal, F](action, nextCtx, fn)
}

class MappedUpdateCtx[Ctx, Val, NewVal, F[_]](
    val action: LeafAction[Ctx, Val, F],
    val nextCtx: Val => Ctx,
    val mapFn: Val => NewVal)
    extends Action[Ctx, NewVal, F] {
  override def map[NewNewVal](fn: NewVal => NewNewVal): MappedUpdateCtx[Ctx, Val, NewNewVal, F] =
    new MappedUpdateCtx[Ctx, Val, NewNewVal, F](action, nextCtx, v => fn(mapFn(v)))
}

object UpdateCtx {
  def apply[Ctx, Val, F[_]](action: LeafAction[Ctx, Val, F])(
      newCtx: Val => Ctx): UpdateCtx[Ctx, Val, F] =
    new UpdateCtx(action, newCtx)
}

private[sangria] case class SubscriptionValue[Ctx, Val, S[_], F[_]](
    source: Val,
    stream: SubscriptionStream[S])
    extends LeafAction[Ctx, Val, F] {
  override def map[NewVal](fn: Val => NewVal): SubscriptionValue[Ctx, NewVal, S, F] =
    throw new IllegalStateException(
      "`map` is not supported subscription actions. Action is only intended for internal use.")
}

case class ProjectionName(name: String) extends FieldTag
case object ProjectionExclude extends FieldTag

trait Projector[Ctx, Val, Res, F[_]] extends (Context[Ctx, Val, F] => Action[Ctx, Res, F]) {
  val maxLevel: Int = Integer.MAX_VALUE
  def apply(ctx: Context[Ctx, Val, F], projected: Vector[ProjectedName]): Action[Ctx, Res, F]
}

object Projector {
  def apply[Ctx, Val, Res, F[_]](
      fn: (Context[Ctx, Val, F], Vector[ProjectedName]) => Action[Ctx, Res, F]) =
    new Projector[Ctx, Val, Res, F] {
      def apply(ctx: Context[Ctx, Val, F], projected: Vector[ProjectedName]) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val, F]) = throw new IllegalStateException(
        "Default apply should not be called on projector!")
    }

  def apply[Ctx, Val, Res, F[_]](
      levels: Int,
      fn: (Context[Ctx, Val, F], Vector[ProjectedName]) => Action[Ctx, Res, F]) =
    new Projector[Ctx, Val, Res, F] {
      override val maxLevel = levels
      def apply(ctx: Context[Ctx, Val, F], projected: Vector[ProjectedName]) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val, F]) = throw new IllegalStateException(
        "Default apply should not be called on projector!")
    }
}

case class ProjectedName(name: String, children: Vector[ProjectedName] = Vector.empty) {
  lazy val asVector = {
    def loop(name: ProjectedName): Vector[Vector[String]] =
      Vector(name.name) +: (name.children.flatMap(loop).map(name.name +: _))

    loop(this)
  }
}

case class MappingDeferred[A, +B](deferred: Deferred[A], mapFn: A => (B, Vector[Throwable]))
    extends Deferred[B]

trait WithArguments {
  def args: Args

  def arg[T](arg: Argument[T]): T = args.arg(arg)
  def arg[T](name: String): T = args.arg(name)

  def argOpt[T](name: String): Option[T] = args.argOpt(name)
  def argOpt[T](arg: Argument[T]): Option[T] = args.argOpt(arg)

  def argDefinedInQuery(name: String): Boolean = args.argDefinedInQuery(name)
  def argDefinedInQuery(arg: Argument[_]): Boolean = args.argDefinedInQuery(arg)

  def withArgs[A1, R](arg1: Argument[A1])(fn: A1 => R): R = args.withArgs(arg1)(fn)
  def withArgs[A1, A2, R](arg1: Argument[A1], arg2: Argument[A2])(fn: (A1, A2) => R): R =
    args.withArgs(arg1, arg2)(fn)
  def withArgs[A1, A2, A3, R](arg1: Argument[A1], arg2: Argument[A2], arg3: Argument[A3])(
      fn: (A1, A2, A3) => R): R = args.withArgs(arg1, arg2, arg3)(fn)
  def withArgs[A1, A2, A3, A4, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4])(fn: (A1, A2, A3, A4) => R): R = args.withArgs(arg1, arg2, arg3, arg4)(fn)
  def withArgs[A1, A2, A3, A4, A5, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5])(fn: (A1, A2, A3, A4, A5) => R): R =
    args.withArgs(arg1, arg2, arg3, arg4, arg5)(fn)
  def withArgs[A1, A2, A3, A4, A5, A6, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5],
      arg6: Argument[A6])(fn: (A1, A2, A3, A4, A5, A6) => R): R =
    args.withArgs(arg1, arg2, arg3, arg4, arg5, arg6)(fn)
  def withArgs[A1, A2, A3, A4, A5, A6, A7, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5],
      arg6: Argument[A6],
      arg7: Argument[A7])(fn: (A1, A2, A3, A4, A5, A6, A7) => R): R =
    args.withArgs(arg1, arg2, arg3, arg4, arg5, arg6, arg7)(fn)
  def withArgs[A1, A2, A3, A4, A5, A6, A7, A8, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5],
      arg6: Argument[A6],
      arg7: Argument[A7],
      arg8: Argument[A8])(fn: (A1, A2, A3, A4, A5, A6, A7, A8) => R): R =
    args.withArgs(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)(fn)
}

trait WithInputTypeRendering[Ctx] {
  def ctx: Ctx
  def sourceMapper: Option[SourceMapper]
  def deprecationTracker: DeprecationTracker
  def marshaller: ResultMarshaller

  private lazy val coercionHelper =
    new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(ctx))

  def renderInputValueCompact[T](value: (_, ToInput[_, _]), tpe: InputType[T]): Option[String] =
    DefaultValueRenderer.renderInputValueCompact(value, tpe, coercionHelper)
}

case class DefaultValueParser[T, F[_]](
    schema: Schema[_, _, F],
    parser: InputParser[T],
    toInput: ToInput[T, _])

object DefaultValueParser {
  def forType[T, F[_]](
      schema: Schema[_, _, F])(implicit parser: InputParser[T], toInput: ToInput[T, _]) =
    DefaultValueParser[T, F](schema, parser, toInput)
}

object DefaultValueRenderer {
  implicit val marshaller = sangria.marshalling.queryAst.queryAstResultMarshaller

  def renderInputValueCompact[T, Ctx](
      value: (_, ToInput[_, _]),
      tpe: InputType[T],
      coercionHelper: ValueCoercionHelper[Ctx]): Option[String] =
    renderInputValue(value, tpe, coercionHelper).map(marshaller.renderCompact)

  def renderInputValuePretty[T, Ctx](
      value: (_, ToInput[_, _]),
      tpe: InputType[T],
      coercionHelper: ValueCoercionHelper[Ctx]): Option[String] =
    renderInputValue(value, tpe, coercionHelper).map(marshaller.renderPretty)

  def renderInputValue[T, Ctx](
      value: (_, ToInput[_, _]),
      tpe: InputType[T],
      coercionHelper: ValueCoercionHelper[Ctx]): Option[marshaller.Node] = {
    val (v, toInput) = value.asInstanceOf[(Any, ToInput[Any, Any])]
    val (inputValue, iu) = toInput.toInput(v)

    if (!iu.isDefined(inputValue))
      None
    else
      coercionHelper.coerceInputValue(
        tpe,
        Nil,
        inputValue,
        None,
        None,
        CoercedScalaResultMarshaller.default,
        CoercedScalaResultMarshaller.default,
        isArgument = false)(iu) match {
        case Right(Trinary.Defined(coerced)) => Some(renderCoercedInputValue(tpe, coerced))
        case _ => None
      }
  }

  def renderCoercedInputValueCompact[T](value: Any, tpe: InputType[T]): String =
    marshaller.renderCompact(renderCoercedInputValue(tpe, value))

  def renderCoercedInputValuePretty[T](value: Any, tpe: InputType[T]): String =
    marshaller.renderPretty(renderCoercedInputValue(tpe, value))

  def renderCoercedInputValue(t: InputType[_], v: Any): marshaller.Node = t match {
    case _ if v == null => marshaller.nullNode
    case s: ScalarType[Any @unchecked] =>
      Resolver.marshalScalarValue(
        s.coerceOutput(v, marshaller.capabilities),
        marshaller,
        s.name,
        s.scalarInfo)
    case s: ScalarAlias[Any @unchecked, Any @unchecked] =>
      renderCoercedInputValue(s.aliasFor, s.toScalar(v))
    case e: EnumType[Any @unchecked] =>
      Resolver.marshalEnumValue(e.coerceOutput(v), marshaller, e.name)
    case io: InputObjectType[_] =>
      val mapValue = v.asInstanceOf[Map[String, Any]]

      val builder = io.fields.foldLeft(marshaller.emptyMapNode(io.fields.map(_.name))) {
        case (acc, field) if mapValue contains field.name =>
          marshaller.addMapNodeElem(
            acc,
            field.name,
            renderCoercedInputValue(field.fieldType, mapValue(field.name)),
            optional = false)
        case (acc, _) => acc
      }

      marshaller.mapNode(builder)
    case l: ListInputType[_] =>
      val listValue = v.asInstanceOf[Seq[Any]]

      marshaller.mapAndMarshal[Any](listValue, renderCoercedInputValue(l.ofType, _))
    case o: OptionInputType[_] =>
      v match {
        case Some(optVal) => renderCoercedInputValue(o.ofType, optVal)
        case None => marshaller.nullNode
        case other => renderCoercedInputValue(o.ofType, other)
      }
  }
}

case class Context[Ctx, Val, F[_]](
    value: Val,
    ctx: Ctx,
    args: Args,
    schema: Schema[Ctx, Val, F],
    field: Field[Ctx, Val, F],
    parentType: ObjectType[Ctx, Any, F],
    marshaller: ResultMarshaller,
    query: ast.Document,
    sourceMapper: Option[SourceMapper],
    deprecationTracker: DeprecationTracker,
    astFields: Vector[ast.Field],
    path: ExecutionPath,
    deferredResolverState: Any,
    middlewareAttachments: Vector[MiddlewareAttachment] = Vector.empty
) extends WithArguments
    with WithInputTypeRendering[Ctx] {
  def isIntrospection: Boolean = introspection.isIntrospection(parentType, field)

  def attachment[T <: MiddlewareAttachment: ClassTag]: Option[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass

    middlewareAttachments.collectFirst {
      case a if clazz.isAssignableFrom(a.getClass) => a.asInstanceOf[T]
    }
  }

  def attachments[T <: MiddlewareAttachment: ClassTag]: Vector[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass

    middlewareAttachments.collect {
      case a if clazz.isAssignableFrom(a.getClass) => a.asInstanceOf[T]
    }
  }
}

case class Args(
    raw: Map[String, Any],
    argsWithDefault: Set[String],
    optionalArgs: Set[String],
    undefinedArgs: Set[String],
    defaultInfo: Cache[String, Any]) {
  private def getAsOptional[T](name: String): Option[T] =
    raw.get(name).asInstanceOf[Option[Option[T]]].flatten

  private def invariantExplicitlyNull(name: String) =
    throw new IllegalArgumentException(
      s"Optional argument '$name' accessed as a non-optional argument (it has a default value), but query explicitly set argument to `null`.")

  private def invariantNotProvided(name: String) =
    throw new IllegalArgumentException(
      s"Optional argument '$name' accessed as a non-optional argument, but it was not provided in the query and argument does not define a default value.")

  def arg[T](arg: Argument[T]): T =
    if (optionalArgs.contains(arg.name) && argsWithDefault.contains(arg.name) && defaultInfo
        .contains(arg.name))
      getAsOptional[T](arg.name).getOrElse(defaultInfo(arg.name).asInstanceOf[T])
    else if (optionalArgs.contains(arg.name) && argsWithDefault.contains(arg.name))
      getAsOptional[T](arg.name).getOrElse(invariantExplicitlyNull(arg.name))
    else if (optionalArgs.contains(arg.name))
      getAsOptional[Any](arg.name).asInstanceOf[T]
    else
      raw(arg.name).asInstanceOf[T]

  def arg[T](name: String): T =
    if (optionalArgs.contains(name) && argsWithDefault.contains(name) && defaultInfo.contains(name))
      getAsOptional[T](name).getOrElse(defaultInfo(name).asInstanceOf[T])
    else if (optionalArgs.contains(name) && argsWithDefault.contains(name))
      getAsOptional[T](name).getOrElse(invariantExplicitlyNull(name))
    else if (optionalArgs.contains(name))
      getAsOptional[T](name).getOrElse(invariantNotProvided(name))
    else
      raw(name).asInstanceOf[T]

  def argOpt[T](name: String): Option[T] = getAsOptional(name)

  def argOpt[T](arg: Argument[T]): Option[T] =
    if (optionalArgs.contains(arg.name))
      getAsOptional[T](arg.name)
    else
      raw.get(arg.name).asInstanceOf[Option[T]]

  def argDefinedInQuery(name: String): Boolean = !undefinedArgs.contains(name)
  def argDefinedInQuery(arg: Argument[_]): Boolean = argDefinedInQuery(arg.name)

  def withArgs[A1, R](arg1: Argument[A1])(fn: A1 => R): R = fn(arg(arg1))
  def withArgs[A1, A2, R](arg1: Argument[A1], arg2: Argument[A2])(fn: (A1, A2) => R): R =
    fn(arg(arg1), arg(arg2))
  def withArgs[A1, A2, A3, R](arg1: Argument[A1], arg2: Argument[A2], arg3: Argument[A3])(
      fn: (A1, A2, A3) => R): R = fn(arg(arg1), arg(arg2), arg(arg3))
  def withArgs[A1, A2, A3, A4, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4])(fn: (A1, A2, A3, A4) => R): R =
    fn(arg(arg1), arg(arg2), arg(arg3), arg(arg4))
  def withArgs[A1, A2, A3, A4, A5, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5])(fn: (A1, A2, A3, A4, A5) => R): R =
    fn(arg(arg1), arg(arg2), arg(arg3), arg(arg4), arg(arg5))
  def withArgs[A1, A2, A3, A4, A5, A6, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5],
      arg6: Argument[A6])(fn: (A1, A2, A3, A4, A5, A6) => R): R =
    fn(arg(arg1), arg(arg2), arg(arg3), arg(arg4), arg(arg5), arg(arg6))
  def withArgs[A1, A2, A3, A4, A5, A6, A7, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5],
      arg6: Argument[A6],
      arg7: Argument[A7])(fn: (A1, A2, A3, A4, A5, A6, A7) => R): R =
    fn(arg(arg1), arg(arg2), arg(arg3), arg(arg4), arg(arg5), arg(arg6), arg(arg7))
  def withArgs[A1, A2, A3, A4, A5, A6, A7, A8, R](
      arg1: Argument[A1],
      arg2: Argument[A2],
      arg3: Argument[A3],
      arg4: Argument[A4],
      arg5: Argument[A5],
      arg6: Argument[A6],
      arg7: Argument[A7],
      arg8: Argument[A8])(fn: (A1, A2, A3, A4, A5, A6, A7, A8) => R): R =
    fn(arg(arg1), arg(arg2), arg(arg3), arg(arg4), arg(arg5), arg(arg6), arg(arg7), arg(arg8))
}

object Args {
  val empty = new Args(Map.empty, Set.empty, Set.empty, Set.empty, Cache.empty)

  def apply(definitions: List[Argument[_]], values: (String, Any)*): Args =
    apply(definitions, values.toMap)

  def apply(definitions: List[Argument[_]], values: Map[String, Any]): Args =
    apply(definitions, input = ScalaInput.scalaInput(values))

  def apply[In: InputUnmarshaller](definitions: List[Argument[_]], input: In): Args = {
    import sangria.marshalling.queryAst._

    val iu = implicitly[InputUnmarshaller[In]]

    if (!iu.isMapNode(input)) {
      throw new IllegalArgumentException("The input expected to be a map-like data structure")
    } else {
      val argsValues =
        iu.getMapKeys(input).flatMap(key => definitions.find(_.name == key)).map { arg =>
          val astValue = iu
            .getRootMapValue(input, arg.name)
            .flatMap(x => this.convert[In, ast.Value](x, arg.argumentType))

          ast.Argument(name = arg.name, value = astValue.getOrElse(ast.NullValue()))
        }

      ValueCollector
        .getArgumentValues(
          ValueCoercionHelper.default,
          None,
          definitions,
          argsValues.toVector,
          Map.empty,
          ExceptionHandler.empty)
        .get
    }
  }

  def apply(schemaElem: HasArguments, astElem: ast.WithArguments): Args = {
    import sangria.marshalling.queryAst._

    apply(
      schemaElem.arguments,
      ast.ObjectValue(
        astElem.arguments.map(arg => ast.ObjectField(arg.name, arg.value))): ast.Value)
  }

  private def convert[In: InputUnmarshaller, Out: ResultMarshallerForType](
      value: In,
      tpe: InputType[_]): Option[Out] = {
    val rm = implicitly[ResultMarshallerForType[Out]]

    ValueCoercionHelper.default.coerceInputValue(
      tpe,
      List("stub"),
      value,
      None,
      None,
      rm.marshaller,
      rm.marshaller,
      isArgument = false) match {
      case Right(v) => v.toOption.asInstanceOf[Option[Out]]
      case Left(violations) => throw AttributeCoercionError(violations, ExceptionHandler.empty)
    }
  }
}

case class DirectiveContext(selection: ast.WithDirectives, directive: Directive, args: Args)
    extends WithArguments
