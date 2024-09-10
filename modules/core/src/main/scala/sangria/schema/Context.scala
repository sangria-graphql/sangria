package sangria.schema

import sangria.ast.SourceMapper
import sangria.execution._
import sangria.execution.deferred.Deferred
import sangria.marshalling._
import sangria.util.Cache
import sangria.{ast, introspection}

import scala.reflect.ClassTag

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

/** @tparam Ctx
  *   Type of the context object that was passed to Sangria's execution method.
  */
trait WithInputTypeRendering[Ctx] {

  /** The context object that was passed to Sangria's execution method. */
  def ctx: Ctx
  def sourceMapper: Option[SourceMapper]
  def deprecationTracker: Option[DeprecationTracker]
  def marshaller: ResultMarshaller

  private lazy val coercionHelper =
    new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(ctx))

  def renderInputValueCompact[T](value: (_, ToInput[_, _]), tpe: InputType[T]): Option[String] =
    DefaultValueRenderer.renderInputValueCompact(value, tpe, coercionHelper)
}

/** The context of a field during schema resolution.
  *
  * When a GraphQL request is executed by a Sangria server, each [[Field field]] in the request is
  * resolved to determine the data that should be returned. An instance of this class provides the
  * context for a particular field's resolution.
  *
  * @param value
  *   The object to which the field belongs.
  * @param ctx
  *   The context object that was passed to Sangria's execution method.
  * @tparam Ctx
  *   Type of the context object that was passed to Sangria's execution method.
  * @tparam Val
  *   Type of the object to which the field belongs.
  */
case class Context[Ctx, Val](
    value: Val,
    ctx: Ctx,
    args: Args,
    schema: Schema[Ctx, Val],
    field: Field[Ctx, Val],
    parentType: ObjectType[Ctx, Any],
    marshaller: ResultMarshaller,
    query: ast.Document,
    sourceMapper: Option[SourceMapper],
    deprecationTracker: Option[DeprecationTracker],
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

  private def getAsOptionalOption[T](name: String): Option[T] =
    raw.get(name).asInstanceOf[Option[T]]

  private def invariantExplicitlyNull(name: String) =
    throw new IllegalArgumentException(
      s"Optional argument '$name' accessed as a non-optional argument (it has a default value), but query explicitly set argument to `null`.")

  private def invariantNotProvided(name: String) =
    throw new IllegalArgumentException(
      s"Optional argument '$name' accessed as a non-optional argument, but it was not provided in the query and argument does not define a default value.")

  def arg[T](arg: Argument[T]): T = {
    val name = arg.name
    if (!optionalArgs.contains(name))
      raw(name).asInstanceOf[T]
    else if (!argsWithDefault.contains(name))
      getAsOptional[Any](name).asInstanceOf[T]
    else if (!defaultInfo.contains(name))
      getAsOptional[T](name).getOrElse(invariantExplicitlyNull(name))
    else
      getAsOptional[T](name).getOrElse(defaultInfo(name).asInstanceOf[T])
  }

  /** Retrieve a mandatory (non-optional) argument. If an optional argument without a default is
    * specified an [[IllegalArgumentException]] will be thrown
    *
    * @param name
    *   the name of the argument
    * @return
    *   the argument value
    * @throws IllegalArgumentException
    *   when an optional argument without a default is specified
    */
  def arg[T](name: String): T =
    if (!optionalArgs.contains(name))
      raw(name).asInstanceOf[T]
    else if (!argsWithDefault.contains(name))
      getAsOptional[T](name).getOrElse(invariantNotProvided(name))
    else if (!defaultInfo.contains(name))
      getAsOptional[T](name).getOrElse(invariantExplicitlyNull(name))
    else
      getAsOptional[T](name).getOrElse(defaultInfo(name).asInstanceOf[T])

  /** Retrieve an optional argument. If a non-optional argument is specified an error will be thrown
    *
    * @param name
    *   the name of the argument
    * @return
    *   An [[Option]] containing the argument if present, or None if absent
    */
  def argOpt[T](name: String): Option[T] = getAsOptional(name)

  def argOpt[T](arg: Argument[T]): Option[T] = {
    val name = arg.name
    if (optionalArgs.contains(name)) {
      if (arg.argumentType.isOptional && arg.defaultValue.isEmpty) getAsOptionalOption[T](name)
      else getAsOptional[T](name)
    } else
      raw.get(name).asInstanceOf[Option[T]]
  }

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

  def apply[In: InputUnmarshaller](
      definitions: List[Argument[_]],
      input: In,
      variables: Option[Map[String, VariableValue]] = None): Args = {
    import sangria.marshalling.queryAst._

    val iu = implicitly[InputUnmarshaller[In]]

    if (!iu.isMapNode(input)) {
      throw new IllegalArgumentException("The input expected to be a map-like data structure")
    } else {
      val argsValues =
        iu.getMapKeys(input).flatMap(key => definitions.find(_.name == key)).map { arg =>
          val astValue = iu
            .getRootMapValue(input, arg.name)
            .flatMap(x => this.convert[In, ast.Value](x, arg.argumentType, variables))

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

  def apply(
      schemaElem: HasArguments,
      astElem: ast.WithArguments,
      variables: Map[String, VariableValue]): Args = {
    import sangria.marshalling.queryAst._

    apply(
      schemaElem.arguments,
      ast.ObjectValue(
        astElem.arguments.map(arg => ast.ObjectField(arg.name, arg.value))): ast.Value,
      Some(variables)
    )
  }

  private def convert[In: InputUnmarshaller, Out: ResultMarshallerForType](
      value: In,
      tpe: InputType[_],
      variables: Option[Map[String, VariableValue]]): Option[Out] = {
    val rm = implicitly[ResultMarshallerForType[Out]]

    ValueCoercionHelper.default.coerceInputValue(
      tpe,
      List("stub"),
      value,
      None,
      variables,
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
