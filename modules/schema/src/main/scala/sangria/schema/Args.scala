package sangria.schema

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
