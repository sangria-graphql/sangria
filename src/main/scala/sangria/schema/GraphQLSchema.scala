package sangria.schema

import sangria.ast
import sangria.validation.Violation

case class Schema[Ctx, Res](
  query: ObjectType[Ctx, Res],
  mutation: Option[ObjectType[Ctx, Res]] = None,
  directives: List[Directive] = IncludeDirective :: SkipDirective :: Nil)

sealed trait Type

sealed trait InputType[+T] extends Type

sealed trait OutputType[+T] extends Type

sealed trait LeafType extends Type
sealed trait CompositeType extends Type
sealed trait AbstractType extends Type
sealed trait NullableType
sealed trait UnmodifiedType

case class ScalarType[T](
  name: String,
  description: Option[String] = None,
  coerceOutput: T => ast.ScalarValue,
  coerceInput: ast.ScalarValue => Either[Violation, T]) extends InputType[T] with OutputType[T] with LeafType with NullableType with UnmodifiedType

case class ObjectType[Ctx, Val] private (
  name: String,
  description: Option[String],
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, _]]
) extends OutputType[Val] with CompositeType with NullableType with UnmodifiedType {
  lazy val fields = fieldsFn()
}

object ObjectType {
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = () => fields, Nil)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn = () => fields, Nil)
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = () => fields, interfaces)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = () => fields, interfaces)

  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn, Nil)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn, Nil)
  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn, interfaces)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn, interfaces)
}

// todo implementations
case class InterfaceType[Ctx, Val] private (
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, Val]] = Nil
) extends OutputType[Val] with CompositeType with AbstractType with NullableType with UnmodifiedType {
  lazy val fields = fieldsFn()
}

object InterfaceType {
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = () => fields, Nil)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn = () => fields, Nil)
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = () => fields, interfaces)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = () => fields, interfaces)

  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn, Nil)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn, Nil)
  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn, interfaces)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn, interfaces)
}

case class UnionType(
  name: String,
  description: Option[String] = None,
  types: List[ObjectType[TODO_UNION, TODO_UNION]]) extends OutputType[TODO_UNION] with CompositeType with AbstractType with NullableType with UnmodifiedType

case class Field[Ctx, Val] private (
  name: String,
  fieldType: OutputType[_],
  description: Option[String],
  arguments: List[Argument[_]],
  resolve: Context[Ctx, Val] => Deferred[_],
  deprecationReason: Option[String])

object Field {
  def apply[Ctx, Val, Res, Out](
      name: String,
      fieldType: OutputType[Out],
      description: Option[String] = None,
      arguments: List[Argument[_]] = Nil,
      resolve: Context[Ctx, Val] => Deferred[Res],
      deprecationReason: Option[String] = None)(implicit ev: Res <:< Out) =
    Field[Ctx, Val](name, fieldType, description, arguments, resolve, deprecationReason)
}

case class Argument[T](
  name: String,
  argumentType: InputType[T],
  description: Option[String] = None,
  defaultValue: Option[T] = None)

case class EnumType[T](
  name: String,
  description: Option[String] = None,
  values: List[EnumValue[T]]) extends InputType[T] with OutputType[T] with LeafType with NullableType with UnmodifiedType

case class EnumValue[+T](
  name: String,
  description: Option[String] = None,
  value: T,
  deprecationReason: Option[String] = None)

case class InputObjectType[T] private (
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[InputObjectField[_]]
) extends InputType[T] with NullableType with UnmodifiedType {
  lazy val fields = fieldsFn()
}

object InputObjectType {
  def apply[T](name: String, fields: List[InputObjectField[_]]): InputObjectType[T] = InputObjectType(name, None, fieldsFn = () => fields)
  def apply[T](name: String, description: String, fields: List[InputObjectField[_]]): InputObjectType[T] = InputObjectType(name, Some(description), fieldsFn = () => fields)

  def apply[T](name: String, fieldsFn: () => List[InputObjectField[_]]): InputObjectType[T] = InputObjectType(name, None, fieldsFn)
  def apply[T](name: String, description: String, fieldsFn: () => List[InputObjectField[_]]): InputObjectType[T] = InputObjectType(name, Some(description), fieldsFn)
}

case class InputObjectField[T](
  name: String,
  fieldType: InputType[T],
  description: Option[String] = None,
  defaultValue: Option[T] = None)

case class ListType[T](ofType: OutputType[T]) extends OutputType[Seq[T]] with NullableType
case class ListInputType[T](ofType: InputType[T]) extends InputType[Seq[T]] with NullableType

case class OptionType[T](ofType: OutputType[T]) extends OutputType[Option[T]]
case class OptionInputType[T](ofType: InputType[T]) extends InputType[Option[T]]

case class Directive(
  name: String,
  description: Option[String] = None,
  arguments: List[Argument[_]] = Nil,
  onOperation: Boolean,
  onFragment: Boolean,
  onField: Boolean)