package sangria.schema

import sangria.ast
import sangria.validation.{EnumValueCoercionViolation, EnumCoercionViolation, Violation}

import scala.util.Success

sealed trait Type

sealed trait InputType[+T] extends Type

sealed trait OutputType[+T] extends Type

sealed trait LeafType extends Type
sealed trait CompositeType extends Type
sealed trait AbstractType extends Type
sealed trait NullableType
sealed trait UnmodifiedType

sealed trait Named {
  def name: String
  def description: Option[String]
}

case class ScalarType[T](
  name: String,
  description: Option[String] = None,
  coerceUserInput: Any => Either[Violation, T],
  coerceOutput: T => ast.Value,
  coerceInput: ast.Value => Either[Violation, T]) extends InputType[T] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named

sealed trait ObjectLikeType[Ctx, Val] extends OutputType[Val] with CompositeType with NullableType with UnmodifiedType with Named {
  def interfaces: List[InterfaceType[Ctx, _]]
  def fields: List[Field[Ctx, Val]]
}

case class ObjectType[Ctx, Val] private (
  name: String,
  description: Option[String],
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, _]]
) extends ObjectLikeType[Ctx, Val] {
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
  interfaces: List[InterfaceType[Ctx, _]]
) extends ObjectLikeType[Ctx, Val] with AbstractType {
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

case class UnionType[Ctx](
  name: String,
  description: Option[String] = None,
  types: List[ObjectType[Ctx, Any]]) extends OutputType[Any] with CompositeType with AbstractType with NullableType with UnmodifiedType with Named

case class Field[Ctx, Val] private (
  name: String,
  fieldType: OutputType[_],
  description: Option[String],
  arguments: List[Argument[_]],
  resolve: Context[Ctx, Val] => Action[_, _],
  deprecationReason: Option[String]) extends Named

object Field {
  def apply[Ctx, Val, Res, Out](
      name: String,
      fieldType: OutputType[Out],
      description: Option[String] = None,
      arguments: List[Argument[_]] = Nil,
      resolve: Context[Ctx, Val] => Action[Ctx, Res],
      deprecationReason: Option[String] = None)(implicit ev: Res <:< Out) =
    Field[Ctx, Val](name, fieldType, description, arguments, resolve, deprecationReason)
}

case class Argument[T](
  name: String,
  argumentType: InputType[T],
  description: Option[String] = None,
  defaultValue: Option[T] = None) extends Named

case class EnumType[T](
    name: String,
    description: Option[String] = None,
    values: List[EnumValue[T]]) extends InputType[T] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named {
  lazy val byName = values groupBy (_.name) mapValues (_.head.value)
  lazy val byValue = values groupBy (_.value) mapValues (_.head.name)

  def coerceUserInput(value: Any): Either[Violation, T] = value match {
    case name: String => byName get name map (Right(_)) getOrElse Left(EnumValueCoercionViolation(name))
    case v if byValue exists (_ == v) => Right(v.asInstanceOf[T])
    case _ => Left(EnumCoercionViolation)
  }

  def coerceInput(value: ast.Value): Either[Violation, T] = value match {
    case ast.EnumValue(name, _) => byName get name map (Right(_)) getOrElse Left(EnumValueCoercionViolation(name))
    case _ => Left(EnumCoercionViolation)
  }


  def coerceOutput(value: T) = ast.EnumValue(byValue(value))
}

case class EnumValue[+T](
  name: String,
  description: Option[String] = None,
  value: T,
  deprecationReason: Option[String] = None) extends Named

case class InputObjectType[T] private (
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[InputObjectField[_]]
) extends InputType[T] with NullableType with UnmodifiedType with Named {
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
  defaultValue: Option[T] = None) extends Named

case class ListType[T](ofType: OutputType[T]) extends OutputType[Seq[T]] with NullableType
case class ListInputType[T](ofType: InputType[T]) extends InputType[Seq[T]] with NullableType

case class OptionType[T](ofType: OutputType[T]) extends OutputType[Option[T]]
case class OptionInputType[T](ofType: InputType[T]) extends InputType[Option[T]]

case class Directive(
  name: String,
  description: Option[String] = None,
  arguments: List[Argument[_]] = Nil,
  shouldInclude: DirectiveContext => Boolean,
  onOperation: Boolean,
  onFragment: Boolean,
  onField: Boolean)

case class Schema[Ctx, Val](
    query: ObjectType[Ctx, Val],
    mutation: Option[ObjectType[Ctx, Val]] = None,
    directives: List[Directive] = BuiltinDirectives) {
  lazy val types: Map[String, Type with Named] = {
    def updated(name: String, tpe: Type with Named, result: Map[String, Type with Named]) =
      if (result contains name) result else result.updated(name, tpe)

    def collectTypes(tpe: Type, result: Map[String, Type with Named]): Map[String, Type with Named] = {
      tpe match {
        case t: Named if result contains t.name => result
        case OptionType(ofType) => collectTypes(ofType, result)
        case OptionInputType(ofType) => collectTypes(ofType, result)
        case ListType(ofType) => collectTypes(ofType, result)
        case ListInputType(ofType) => collectTypes(ofType, result)

        case t @ ScalarType(name, _, _, _, _) => updated(name, t, result)
        case t @ EnumType(name, _, _) => updated(name, t, result)
        case t @ InputObjectType(name, _, _) =>
          t.fields.foldLeft(updated(name, t, result)) {case (acc, field) => collectTypes(field.fieldType, acc)}
        case t: ObjectLikeType[_, _] =>
          val own = t.fields.foldLeft(updated(t.name, t, result)) {
            case (acc, field) =>
              field.arguments.foldLeft(collectTypes(field.fieldType, acc)) {
                case (aacc, arg) => collectTypes(arg.argumentType, aacc)
              }
          }

          t.interfaces.foldLeft(own) {case (acc, interface) => collectTypes(interface, acc)}
        case t @ UnionType(name, _, types) =>
          types.foldLeft(updated(name, t, result)) {case (acc, tpe) => collectTypes(tpe, acc)}
      }
    }

    val queryTypes = collectTypes(query, Map.empty)
    val queryAndMutTypes = mutation map (collectTypes(_, queryTypes)) getOrElse queryTypes


    queryAndMutTypes ++ (BuiltinScalars map (s => s.name -> s))
  }

  lazy val inputTypes = types collect {case (name, tpe: InputType[_]) => name -> tpe}
  lazy val outputTypes = types collect {case (name, tpe: OutputType[_]) => name -> tpe}
  lazy val scalarTypes = types collect {case (name, tpe: ScalarType[_]) => name -> tpe}
  lazy val unionTypes: Map[String, UnionType[_]] =
    types.filter(_.isInstanceOf[UnionType[_]]).mapValues(_.asInstanceOf[UnionType[_]])

  lazy val directivesByName = directives groupBy (_.name) mapValues (_.head)

  def getInputType(tpe: ast.Type): Option[InputType[_]] = tpe match {
    case ast.ConcreteType(name, _) => inputTypes get name map (OptionInputType(_))
    case ast.NotNullType(ofType, _) => getInputType(ofType) collect {case OptionInputType(ot) => ot}
    case ast.ListType(ofType, _) => getInputType(ofType) map (ListInputType(_))
  }

  def getOutputType(tpe: ast.Type): Option[OutputType[_]] = tpe match {
    case ast.ConcreteType(name, _) => outputTypes get name map (OptionType(_))
    case ast.NotNullType(ofType, _) => getOutputType(ofType) collect {case OptionType(ot) => ot}
    case ast.ListType(ofType, _) => getOutputType(ofType) map (ListType(_))
  }

  lazy val directImplementations: Map[String, List[ObjectLikeType[_, _]]] = {
    types.values.toList
      .collect{case objectLike: ObjectLikeType[_, _] => objectLike}
      .flatMap(objectLike => objectLike.interfaces map (_.name -> objectLike))
      .groupBy(_._1)
      .mapValues(_ map (_._2))
  }

  lazy val implementations: Map[String, List[ObjectType[_, _]]] = {
    def findConcreteTypes(tpe: ObjectLikeType[_, _]): List[ObjectType[_, _]] = tpe match {
      case obj: ObjectType[_, _] => obj :: Nil
      case interface: InterfaceType[_, _] => directImplementations(interface.name) flatMap findConcreteTypes
    }

    directImplementations map {
      case (name, directImpls) => name -> directImpls.flatMap(findConcreteTypes).groupBy(_.name).map(_._2.head).toList
    }
  }

  lazy val possibleTypes: Map[String, List[ObjectType[_, _]]] =
    implementations ++ unionTypes.values.map(ut => ut.name -> ut.types)

  def isPossibleType(baseTypeName: String, tpe: ObjectType[_, _]) =
    possibleTypes get baseTypeName exists (_ exists (_.name == tpe.name))
}