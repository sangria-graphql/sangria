package sangria.schema

import sangria.ast
import sangria.validation.Violation

case class Schema(
  query: ObjectType,
  mutation: Option[ObjectType] = None,
  directives: List[Directive] = IncludeDirective :: SkipDirective :: Nil)

sealed trait Type

sealed trait InputType[T] extends Type

sealed trait OutputType extends Type

sealed trait LeafType extends Type
sealed trait CompositeType extends Type
sealed trait AbstractType extends Type
sealed trait NullableType
sealed trait UnmodifiedType

case class ScalarType[T](
  name: String,
  description: Option[String] = None,
  coerceOutput: T => ast.ScalarValue,
  coerceInput: ast.ScalarValue => Either[Violation, T]) extends InputType[T] with OutputType with LeafType with NullableType with UnmodifiedType

case class ObjectType private (
  name: String,
  description: Option[String],
  fieldsFn: () => List[Field],
  interfaces: List[InterfaceType]
) extends OutputType with CompositeType with NullableType with UnmodifiedType {
  lazy val fields = fieldsFn()
}

object ObjectType {
  def apply(name: String, fields: List[Field]): ObjectType = ObjectType(name, None, fieldsFn = () => fields, Nil)
  def apply(name: String, description: String, fields: List[Field]): ObjectType = ObjectType(name, Some(description), fieldsFn = () => fields, Nil)
  def apply(name: String, fields: List[Field], interfaces: List[InterfaceType]): ObjectType = ObjectType(name, None, fieldsFn = () => fields, interfaces)
  def apply(name: String, description: String, fields: List[Field], interfaces: List[InterfaceType]): ObjectType = ObjectType(name, None, fieldsFn = () => fields, interfaces)

  def apply(name: String, fieldsFn: () => List[Field]): ObjectType = ObjectType(name, None, fieldsFn, Nil)
  def apply(name: String, description: String, fieldsFn: () => List[Field]): ObjectType = ObjectType(name, Some(description), fieldsFn, Nil)
  def apply(name: String, fieldsFn: () => List[Field], interfaces: List[InterfaceType]): ObjectType = ObjectType(name, None, fieldsFn, interfaces)
  def apply(name: String, description: String, fieldsFn: () => List[Field], interfaces: List[InterfaceType]): ObjectType = ObjectType(name, None, fieldsFn, interfaces)
}

// todo implementations
case class InterfaceType private (
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[Field],
  interfaces: List[InterfaceType] = Nil
) extends OutputType with CompositeType with AbstractType with NullableType with UnmodifiedType {
  lazy val fields = fieldsFn()
}

object InterfaceType {
  def apply(name: String, fields: List[Field]): InterfaceType = InterfaceType(name, None, fieldsFn = () => fields, Nil)
  def apply(name: String, description: String, fields: List[Field]): InterfaceType = InterfaceType(name, Some(description), fieldsFn = () => fields, Nil)
  def apply(name: String, fields: List[Field], interfaces: List[InterfaceType]): InterfaceType = InterfaceType(name, None, fieldsFn = () => fields, interfaces)
  def apply(name: String, description: String, fields: List[Field], interfaces: List[InterfaceType]): InterfaceType = InterfaceType(name, None, fieldsFn = () => fields, interfaces)

  def apply(name: String, fieldsFn: () => List[Field]): InterfaceType = InterfaceType(name, None, fieldsFn, Nil)
  def apply(name: String, description: String, fieldsFn: () => List[Field]): InterfaceType = InterfaceType(name, Some(description), fieldsFn, Nil)
  def apply(name: String, fieldsFn: () => List[Field], interfaces: List[InterfaceType]): InterfaceType = InterfaceType(name, None, fieldsFn, interfaces)
  def apply(name: String, description: String, fieldsFn: () => List[Field], interfaces: List[InterfaceType]): InterfaceType = InterfaceType(name, None, fieldsFn, interfaces)
}

case class UnionType(
  name: String,
  description: Option[String] = None,
  types: List[ObjectType]) extends OutputType with CompositeType with AbstractType with NullableType with UnmodifiedType

// todo resolve
case class Field(
  name: String,
  fieldType: OutputType,
  description: Option[String] = None,
  arguments: List[Argument[_]] = Nil,
  deprecationReason: Option[String] = None)

case class Argument[T](
  name: String,
  argumentType: InputType[T],
  description: Option[String] = None,
  defaultValue: Option[T] = None)

case class EnumType[T](
  name: String,
  description: Option[String] = None,
  values: List[EnumValue[T]]) extends InputType[T] with OutputType with LeafType with NullableType with UnmodifiedType

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

case class ListType(ofType: OutputType) extends OutputType with NullableType
case class ListInputType[T](ofType: InputType[T]) extends InputType[T] with NullableType

case class NonNullType(ofType: OutputType) extends OutputType
case class NonNullInputType[T](ofType: InputType[T]) extends InputType[T] with OutputType

case class Directive(
  name: String,
  description: Option[String] = None,
  arguments: List[Argument[_]] = Nil,
  onOperation: Boolean,
  onFragment: Boolean,
  onField: Boolean)