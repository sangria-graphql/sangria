package sangria.schema

import sangria.ast
import sangria.ast.SourceMapper
import sangria.execution.{DeprecationTracker, ValueCoercionHelper}
import sangria.marshalling.{ResultMarshaller, ToInput}

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

case class DirectiveContext(selection: ast.WithDirectives, directive: Directive, args: Args)
  extends WithArguments
