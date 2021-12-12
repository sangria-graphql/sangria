package sangria.schema

import language.{higherKinds, implicitConversions}
import sangria.execution._
import sangria.marshalling._
import sangria.ast.SourceMapper
import sangria.{ast, introspection}
import sangria.util.Cache

import scala.reflect.ClassTag

case class ProjectionName(name: String) extends FieldTag
case object ProjectionExclude extends FieldTag

trait Projector[Ctx, Val, Res] extends (Context[Ctx, Val] => Action[Ctx, Res]) {
  val maxLevel: Int = Integer.MAX_VALUE
  def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res]
}

object Projector {
  def apply[Ctx, Val, Res](fn: (Context[Ctx, Val], Vector[ProjectedName]) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res] =
        fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException(
        "Default apply should not be called on projector!")
    }

  def apply[Ctx, Val, Res](
      levels: Int,
      fn: (Context[Ctx, Val], Vector[ProjectedName]) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      override val maxLevel: Int = levels
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res] =
        fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException(
        "Default apply should not be called on projector!")
    }
}

case class ProjectedName(
    name: String,
    children: Vector[ProjectedName] = Vector.empty,
    args: Args = Args.empty) {
  lazy val asVector: Vector[Vector[String]] = {
    def loop(name: ProjectedName): Vector[Vector[String]] =
      Vector(name.name) +: (name.children.flatMap(loop).map(name.name +: _))

    loop(this)
  }
}

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
  def deprecationTracker: DeprecationTracker
  def marshaller: ResultMarshaller

  private lazy val coercionHelper =
    new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(ctx))

  def renderInputValueCompact[T](value: (_, ToInput[_, _]), tpe: InputType[T]): Option[String] =
    DefaultValueRenderer.renderInputValueCompact(value, tpe, coercionHelper)
}

case class DefaultValueParser[T](
    schema: Schema[_, _],
    parser: InputParser[T],
    toInput: ToInput[T, _])

object DefaultValueParser {
  def forType[T](schema: Schema[_, _])(implicit
      parser: InputParser[T],
      toInput: ToInput[T, _]): DefaultValueParser[T] =
    DefaultValueParser[T](schema, parser, toInput)
}

object DefaultValueRenderer {
  implicit val marshaller: QueryAstResultMarshaller =
    sangria.marshalling.queryAst.queryAstResultMarshaller

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
      variables: Option[Map[String, VariableValue]] = None): Option[Out] = {
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
