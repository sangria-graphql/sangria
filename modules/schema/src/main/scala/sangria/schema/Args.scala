package sangria.schema

import sangria.ast
import sangria.execution.{
  AttributeCoercionError,
  ExceptionHandler,
  ValueCoercionHelper,
  ValueCollector,
  VariableValue
}
import sangria.marshalling.{InputUnmarshaller, ResultMarshallerForType, ScalaInput}
import sangria.util.Cache

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

  def apply[In: InputUnmarshaller](
      definitions: List[Argument[_]],
      input: In,
      variables: Option[Map[String, VariableValue]] = None): Args = {
    import sangria.marshalling.queryAstCore._

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
    import sangria.marshalling.queryAstCore._

    apply(
      schemaElem.arguments,
      ast.ObjectValue(
        astElem.arguments.map(arg => ast.ObjectField(arg.name, arg.value))): ast.Value)
  }

  def apply(
      schemaElem: HasArguments,
      astElem: ast.WithArguments,
      variables: Map[String, VariableValue]): Args = {
    import sangria.marshalling.queryAstCore._

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
