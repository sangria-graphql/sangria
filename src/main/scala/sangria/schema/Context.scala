package sangria.schema

import sangria.execution.{FieldTag, DeprecationTracker, ValueCoercionHelper, Resolver}
import sangria.marshalling.{CoercedScalaResultMarshaller, InputUnmarshaller, ToInput, ResultMarshaller}
import sangria.parser.SourceMapper

import language.implicitConversions

import sangria.ast

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

sealed trait Action[+Ctx, +Val] {
  def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): Action[Ctx, NewVal]
}
sealed trait LeafAction[+Ctx, +Val] extends Action[Ctx, Val] {
  def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): LeafAction[Ctx, NewVal]
}
sealed trait ReduceAction[+Ctx, +Val] extends Action[Ctx, Val] {
  def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): LeafAction[Ctx, NewVal]
}

object ReduceAction {
  implicit def futureAction[Ctx, Val](value: Future[Val]): ReduceAction[Ctx, Val] = FutureValue(value)
  implicit def tryAction[Ctx, Val](value: Try[Val]): ReduceAction[Ctx, Val] = TryValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): ReduceAction[Ctx, Val] = Value(value)
}

object Action {
  implicit def deferredAction[Ctx, Val](value: Deferred[Val]): LeafAction[Ctx, Val] = DeferredValue(value)
  implicit def deferredFutureAction[Ctx, Val, D <: Deferred[Val]](value: Future[D])(implicit ev: D <:< Deferred[Val]): LeafAction[Ctx, Val] = DeferredFutureValue(value)

  implicit def futureAction[Ctx, Val](value: Future[Val]): LeafAction[Ctx, Val] = FutureValue(value)
  implicit def tryAction[Ctx, Val](value: Try[Val]): LeafAction[Ctx, Val] = TryValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): LeafAction[Ctx, Val] = Value(value)
}

case class Value[Ctx, Val](value: Val) extends LeafAction[Ctx, Val] with ReduceAction[Ctx, Val] {
  override def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): Value[Ctx, NewVal] =
    Value(fn(value))
}

case class TryValue[Ctx, Val](value: Try[Val]) extends LeafAction[Ctx, Val] with ReduceAction[Ctx, Val] {
  override def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): TryValue[Ctx, NewVal] =
    TryValue(value map fn)
}

case class FutureValue[Ctx, Val](value: Future[Val]) extends LeafAction[Ctx, Val] with ReduceAction[Ctx, Val] {
  override def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): FutureValue[Ctx, NewVal] =
    FutureValue(value map fn)
}

case class DeferredValue[Ctx, Val](value: Deferred[Val]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): DeferredValue[Ctx, NewVal] =
    DeferredValue(MappingDeferred(value, fn))
}

case class DeferredFutureValue[Ctx, Val](value: Future[Deferred[Val]]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): DeferredFutureValue[Ctx, NewVal] =
    DeferredFutureValue(value map (MappingDeferred(_, fn)))
}

class UpdateCtx[Ctx, Val](val action: LeafAction[Ctx, Val], val nextCtx: Val ⇒ Ctx) extends Action[Ctx, Val] {
  override def map[NewVal](fn: Val ⇒ NewVal)(implicit ec: ExecutionContext): MappedUpdateCtx[Ctx, Val, NewVal] =
    new MappedUpdateCtx[Ctx, Val, NewVal](action, nextCtx, fn)
}

class MappedUpdateCtx[Ctx, Val, NewVal](val action: LeafAction[Ctx, Val], val nextCtx: Val ⇒ Ctx, val mapFn: Val ⇒ NewVal) extends Action[Ctx, NewVal] {
  override def map[NewNewVal](fn: NewVal ⇒ NewNewVal)(implicit ec: ExecutionContext): MappedUpdateCtx[Ctx, Val, NewNewVal] =
    new MappedUpdateCtx[Ctx, Val, NewNewVal](action, nextCtx, v ⇒ fn(mapFn(v)))
}

object UpdateCtx {
  def apply[Ctx, Val](action: LeafAction[Ctx, Val])(newCtx: Val ⇒ Ctx): UpdateCtx[Ctx, Val] = new UpdateCtx(action, newCtx)
}

case class ProjectionName(name: String) extends FieldTag
case object ProjectionExclude extends FieldTag

trait Projector[Ctx, Val, Res] extends (Context[Ctx, Val] ⇒ Action[Ctx, Res]) {
  val maxLevel: Int = Integer.MAX_VALUE
  def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res]
}

object Projector {
  def apply[Ctx, Val, Res](fn: (Context[Ctx, Val], Vector[ProjectedName]) ⇒ Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException("Default apply should not be called on projector!")
    }

  def apply[Ctx, Val, Res](levels: Int, fn: (Context[Ctx, Val], Vector[ProjectedName]) ⇒ Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      override val maxLevel = levels
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException("Default apply should not be called on projector!")
    }
}

case class ProjectedName(name: String, children: Vector[ProjectedName] = Vector.empty) {
  lazy val asVector = {
    def loop(name: ProjectedName): Vector[Vector[String]] =
      Vector(name.name) +: (name.children flatMap loop map (name.name +: _))

    loop(this)
  }
}

trait Deferred[+T]

case class MappingDeferred[A, +B](deferred: Deferred[A], mapFn: A ⇒ B) extends Deferred[B]

trait WithArguments {
  def args: Args
  def arg[T](arg: Argument[T]): T = args.arg(arg)
  def arg[T](name: String): T = args.arg(name)
  def argOpt[T](name: String): Option[T] = args.argOpt(name)
}

trait WithInputTypeRendering[Ctx] {
  def ctx: Ctx
  def sourceMapper: Option[SourceMapper]
  def deprecationTracker: DeprecationTracker
  def marshaller: ResultMarshaller

  private lazy val coercionHelper = new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(ctx))

  def renderInputValueCompact[T](value: (_, ToInput[_, _]), tpe: InputType[T], m: ResultMarshaller = marshaller): String =
    DefaultValueRenderer.renderInputValueCompact(value, tpe, coercionHelper)(m)

  def renderInputValuePretty[T](value: (_, ToInput[_, _]), tpe: InputType[T], m: ResultMarshaller = marshaller): String =
    DefaultValueRenderer.renderInputValuePretty(value, tpe, coercionHelper)(m)

  def renderCoercedInputValueCompact[T](value: Any, tpe: InputType[T], m: ResultMarshaller = marshaller): String =
    DefaultValueRenderer.renderCoercedInputValueCompact(value, tpe)(m)

  def renderCoercedInputValuePretty[T](value: Any, tpe: InputType[T], m: ResultMarshaller = marshaller): String =
    DefaultValueRenderer.renderCoercedInputValuePretty(value, tpe)(m)
}

object DefaultValueRenderer {
  def renderInputValueCompact[T, Ctx](value: (_, ToInput[_, _]), tpe: InputType[T], coercionHelper: ValueCoercionHelper[Ctx])(implicit m: ResultMarshaller): String =
    m.renderCompact(renderInputValue(value, tpe, coercionHelper))

  def renderInputValuePretty[T, Ctx](value: (_, ToInput[_, _]), tpe: InputType[T], coercionHelper: ValueCoercionHelper[Ctx])(implicit m: ResultMarshaller): String =
    m.renderPretty(renderInputValue(value, tpe, coercionHelper))

  def renderInputValue[T, Ctx](value: (_, ToInput[_, _]), tpe: InputType[T], coercionHelper: ValueCoercionHelper[Ctx])(implicit m: ResultMarshaller): m.Node = {
    def loop(t: InputType[_], v: Any): m.Node = t match {
      case _ if v == null ⇒ m.nullNode
      case s: ScalarType[Any @unchecked] ⇒ Resolver.marshalScalarValue(s.coerceOutput(v, m.capabilities), m, s.name, s.scalarInfo)
      case e: EnumType[Any @unchecked] ⇒ Resolver.marshalEnumValue(e.coerceOutput(v), m, e.name)
      case io: InputObjectType[_] ⇒
        val mapValue = v.asInstanceOf[Map[String, Any]]

        val builder = io.fields.foldLeft(m.emptyMapNode(io.fields.map(_.name))) {
          case (acc, field) if mapValue contains field.name ⇒
            m.addMapNodeElem(acc, field.name, loop(field.fieldType, mapValue(field.name)), optional = false)
          case (acc, _) ⇒ acc
        }

        m.mapNode(builder)
      case l: ListInputType[_] ⇒
        val listValue = v.asInstanceOf[Seq[Any]]

        m.mapAndMarshal[Any](listValue, loop(l.ofType, _))
      case o: OptionInputType[_] ⇒ v match {
        case Some(optVal) ⇒ loop(o.ofType, optVal)
        case None ⇒ m.nullNode
        case other ⇒ loop(o.ofType, other)
      }
    }

    val (v, toInput) = value.asInstanceOf[(Any, ToInput[Any, Any])]
    val (inputValue, iu) = toInput.toInput(v)

    coercionHelper.coerceInputValue(tpe, Nil, inputValue, None, CoercedScalaResultMarshaller.default, CoercedScalaResultMarshaller.default)(iu) match {
      case Right(Some(coerced)) ⇒ renderCoercedInputValue(tpe, coerced)
      case _ ⇒ m.nullNode
    }
  }

  def renderCoercedInputValueCompact[T](value: Any, tpe: InputType[T])(implicit m: ResultMarshaller): String =
    m.renderCompact(renderCoercedInputValue(tpe, value))

  def renderCoercedInputValuePretty[T](value: Any, tpe: InputType[T])(implicit m: ResultMarshaller): String =
    m.renderPretty(renderCoercedInputValue(tpe, value))

  def renderCoercedInputValue(t: InputType[_], v: Any)(implicit m: ResultMarshaller): m.Node = t match {
    case _ if v == null ⇒ m.nullNode
    case s: ScalarType[Any @unchecked] ⇒ Resolver.marshalScalarValue(s.coerceOutput(v, m.capabilities), m, s.name, s.scalarInfo)
    case e: EnumType[Any @unchecked] ⇒ Resolver.marshalEnumValue(e.coerceOutput(v), m, e.name)
    case io: InputObjectType[_] ⇒
      val mapValue = v.asInstanceOf[Map[String, Any]]

      val builder = io.fields.foldLeft(m.emptyMapNode(io.fields.map(_.name))) {
        case (acc, field) if mapValue contains field.name ⇒
          m.addMapNodeElem(acc, field.name, renderCoercedInputValue(field.fieldType, mapValue(field.name)), optional = false)
        case (acc, _) ⇒ acc
      }

      m.mapNode(builder)
    case l: ListInputType[_] ⇒
      val listValue = v.asInstanceOf[Seq[Any]]

      m.mapAndMarshal[Any](listValue, renderCoercedInputValue(l.ofType, _))
    case o: OptionInputType[_] ⇒ v match {
      case Some(optVal) ⇒ renderCoercedInputValue(o.ofType, optVal)
      case None ⇒ m.nullNode
      case other ⇒ renderCoercedInputValue(o.ofType, other)
    }
  }
}

case class Context[Ctx, Val](
  value: Val,
  ctx: Ctx,
  args: Args,
  schema: Schema[Ctx, Val],
  field: Field[Ctx, Val],
  parentType: ObjectType[Ctx, Any],
  marshaller: ResultMarshaller,
  sourceMapper: Option[SourceMapper],
  deprecationTracker: DeprecationTracker,
  astFields: Vector[ast.Field],
  path: Vector[String]) extends WithArguments with WithInputTypeRendering[Ctx]

case class Args(raw: Map[String, Any]) extends AnyVal {
  def arg[T](arg: Argument[T]): T = raw.get(arg.name).fold(None.asInstanceOf[T])(_.asInstanceOf[T])
  def arg[T](name: String): T = raw(name).asInstanceOf[T]
  def argOpt[T](name: String): Option[T] = raw.get(name).asInstanceOf[Option[Option[T]]].flatten
}

object Args {
  val empty = Args(Map.empty)
}

case class DirectiveContext(selection: ast.WithDirectives, directive: Directive, args: Args) extends WithArguments

trait DeferredResolver[-Ctx] {
  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx): Vector[Future[Any]]
}

object DeferredResolver {
  val empty = new DeferredResolver[Any] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any) = deferred map (_ ⇒ Future.failed(UnsupportedDeferError))
  }
}

case object UnsupportedDeferError extends Exception
