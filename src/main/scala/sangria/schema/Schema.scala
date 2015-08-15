package sangria.schema

import sangria.{introspection, ast}
import sangria.validation.{EnumValueCoercionViolation, EnumCoercionViolation, Violation}
import sangria.introspection.{SchemaMetaField, TypeMetaField, TypeNameMetaField}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

sealed trait Type

sealed trait InputType[+T] extends Type

sealed trait OutputType[+T] extends Type

sealed trait LeafType extends Type
sealed trait CompositeType[T] extends Type with Named with OutputType[T]
sealed trait AbstractType extends Type with Named {
  def name: String

  def typeOf[Ctx](value: Any, schema: Schema[Ctx, _]): Option[ObjectType[Ctx, _]] =
    schema.possibleTypes get name flatMap (_.find(_ isInstanceOf value).asInstanceOf[Option[ObjectType[Ctx, _]]])
}

sealed trait NullableType
sealed trait UnmodifiedType

sealed trait Named {
  def name: String
  def description: Option[String]
}

object Named {
  private[schema] def checkFields[T <: Seq[Named]](fields: T): T =
    if (fields.map(_.name).toSet.size != fields.size)
      throw new IllegalArgumentException("All fields within a Type should have unique names!")
    else fields

  private[schema] def checkFieldsFn[T <: Seq[Named]](fields: T): () => T =
    if (fields.map(_.name).toSet.size != fields.size)
      throw new IllegalArgumentException("All fields within a Type should have unique names!")
    else () => fields

  private[schema] def checkFields[T <: Seq[Named]](fieldsFn: () => T): () => T =
    () => checkFields(fieldsFn())
}

case class ScalarType[T](
  name: String,
  description: Option[String] = None,
  coerceUserInput: Any => Either[Violation, T],
  coerceOutput: T => ast.Value,
  coerceInput: ast.Value => Either[Violation, T]) extends InputType[T] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named

sealed trait ObjectLikeType[Ctx, Val] extends OutputType[Val] with CompositeType[Val] with NullableType with UnmodifiedType with Named {
  def interfaces: List[InterfaceType[Ctx, _]]

  def fieldsFn: () => List[Field[Ctx, Val]]

  private lazy val ownFields = fieldsFn()

  def removeDuplicates[T, E](list: List[T], valueFn: T => E) =
    list.foldLeft((Nil, Nil): (List[E], List[T])) {
      case (a @ (visited, acc), e) if visited contains valueFn(e) => a
      case ((visited, acc), e) => (visited :+ valueFn(e), acc :+ e)
    }._2

  lazy val fields: List[Field[Ctx, _]] = removeDuplicates(
    ownFields ++ interfaces.flatMap(i => i.fields.asInstanceOf[List[Field[Ctx, _]]]),
    (e: Field[Ctx, _]) => e.name)

  private lazy val fieldsByName = fields groupBy (_.name) mapValues (_.head)

  def getField(schema: Schema[_, _], fieldName: String): Option[Field[Ctx, _]] =
    if (fieldName == SchemaMetaField.name && name == schema.query.name) Some(SchemaMetaField.asInstanceOf[Field[Ctx, _]])
    else if (fieldName == TypeMetaField.name && name == schema.query.name) Some(TypeMetaField.asInstanceOf[Field[Ctx, _]])
    else if (fieldName == TypeNameMetaField.name) Some(TypeNameMetaField.asInstanceOf[Field[Ctx, _]])
    else fieldsByName get fieldName
}

case class ObjectType[Ctx, Val: ClassTag] private (
  name: String,
  description: Option[String],
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, _]]
) extends ObjectLikeType[Ctx, Val] {

  def isInstanceOf(value: Any) = implicitly[ClassTag[Val]].runtimeClass.isAssignableFrom(value.getClass)
}

object ObjectType {
  def apply[Ctx, Val: ClassTag](name: String, fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = Named.checkFieldsFn(fields), Nil)
  def apply[Ctx, Val: ClassTag](name: String, description: String, fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn = Named.checkFieldsFn(fields), Nil)
  def apply[Ctx, Val: ClassTag](name: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = Named.checkFieldsFn(fields), interfaces)
  def apply[Ctx, Val: ClassTag](name: String, description: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn = Named.checkFieldsFn(fields), interfaces)

  def apply[Ctx, Val: ClassTag](name: String, fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, Named.checkFields(fieldsFn), Nil)
  def apply[Ctx, Val: ClassTag](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), Named.checkFields(fieldsFn), Nil)
  def apply[Ctx, Val: ClassTag](name: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, Named.checkFields(fieldsFn), interfaces)
  def apply[Ctx, Val: ClassTag](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), Named.checkFields(fieldsFn), interfaces)
}

case class InterfaceType[Ctx, Val] private (
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, _]]
) extends ObjectLikeType[Ctx, Val] with AbstractType

object InterfaceType {
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = Named.checkFieldsFn(fields), Nil)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn = Named.checkFieldsFn(fields), Nil)
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = Named.checkFieldsFn(fields), interfaces)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn = Named.checkFieldsFn(fields), interfaces)

  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, Named.checkFields(fieldsFn), Nil)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), Named.checkFields(fieldsFn), Nil)
  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, Named.checkFields(fieldsFn), interfaces)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[InterfaceType[Ctx, _ >: Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), Named.checkFields(fieldsFn), interfaces)
}

case class UnionType[Ctx](
  name: String,
  description: Option[String] = None,
  types: List[ObjectType[Ctx, _]]) extends OutputType[Any] with CompositeType[Any] with AbstractType with NullableType with UnmodifiedType

case class Field[Ctx, Val] private (
  name: String,
  fieldType: OutputType[_],
  description: Option[String],
  arguments: List[Argument[_]],
  resolve: Context[Ctx, Val] => Action[Ctx, _],
  deprecationReason: Option[String]) extends Named with HasArguments

object Field {
  def apply[Ctx, Val, Res, Out](
      name: String,
      fieldType: OutputType[Out],
      description: Option[String] = None,
      arguments: List[Argument[_]] = Nil,
      resolve: Context[Ctx, Val] => Action[Ctx, Res],
      deprecationReason: Option[String] = None)(implicit ev: ValidOutType[Res, Out]) =
    Field[Ctx, Val](name, fieldType, description, arguments, resolve, deprecationReason)
}

@implicitNotFound(msg = "${Res} is invalid type for the resulting GraphQL type ${Out}.")
trait ValidOutType[-Res, +Out]

object ValidOutType {
  val valid = new ValidOutType[Any, Any] {}

  implicit def validSubclass[Res, Out](implicit ev: Res <:< Out) = valid.asInstanceOf[ValidOutType[Res, Out]]
  implicit def validNothing[Out] = valid.asInstanceOf[ValidOutType[Nothing, Out]]
  implicit def validOption[Res, Out](implicit ev: Res <:< Out) = valid.asInstanceOf[ValidOutType[Res, Option[Out]]]
  implicit def validSeq[Res, Out](implicit ev: Res <:< Out) = valid.asInstanceOf[ValidOutType[Res, Seq[Out]]]
}

trait InputValue[T] {
  def name: String
  def inputValueType: InputType[_]
  def description: Option[String]
  def defaultValue: Option[_]
}

case class Argument[T] private (
    name: String,
    argumentType: InputType[_],
    description: Option[String],
    defaultValue: Option[_]) extends InputValue[T] with Named {
  def inputValueType = argumentType
}

object Argument {
  def apply[T, Default](
      name: String,
      argumentType: InputType[T],
      description: String,
      defaultValue: Default)(implicit ev: ValidOutType[Default, T], res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, Some(description), Some(defaultValue))

  def apply[T, Default](
      name: String,
      argumentType: InputType[T],
      defaultValue: Default)(implicit ev: ValidOutType[Default, T], res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, None, Some(defaultValue))

  def apply[T](
      name: String,
      argumentType: InputType[T],
      description: String)(implicit res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, Some(description), None)

  def apply[T](
      name: String,
      argumentType: InputType[T])(implicit res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, None, None)
}

trait ArgumentType[T] {
  type Res
}

object ArgumentType extends ArgumentTypeLowPrio {
  implicit def optionArgTpe[T] = new ArgumentType[Option[T]] {
    type Res = T
  }
}

trait ArgumentTypeLowPrio {
  implicit def defaultArgTpe[T] = new ArgumentType[T] {
    type Res = T
  }
}

case class EnumType[T](
    name: String,
    description: Option[String] = None,
    values: List[EnumValue[T]]) extends InputType[T] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named {
  lazy val byName = values groupBy (_.name) mapValues (_.head)
  lazy val byValue = values groupBy (_.value) mapValues (_.head)

  def coerceUserInput(value: Any): Either[Violation, (T, Boolean)] = value match {
    case name: String => byName get name map (v => Right(v.value -> v.deprecationReason.isDefined)) getOrElse Left(EnumValueCoercionViolation(name))
    case v if byValue exists (_._1 == v) => Right(v.asInstanceOf[T] -> byValue(v.asInstanceOf[T]).deprecationReason.isDefined)
    case _ => Left(EnumCoercionViolation)
  }

  def coerceInput(value: ast.Value): Either[Violation, (T, Boolean)] = value match {
    case ast.EnumValue(name, _) => byName get name map (v => Right(v.value -> v.deprecationReason.isDefined)) getOrElse Left(EnumValueCoercionViolation(name))
    case _ => Left(EnumCoercionViolation)
  }

  def coerceOutput(value: T) = ast.EnumValue(byValue(value).name)
}

case class EnumValue[+T](
  name: String,
  description: Option[String] = None,
  value: T,
  deprecationReason: Option[String] = None) extends Named

case class InputObjectType[T] private (
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[InputField[_]]
) extends InputType[T] with NullableType with UnmodifiedType with Named {
  lazy val fields = fieldsFn()
  lazy val fieldsByName = fields groupBy(_.name) mapValues(_.head)
}

object InputObjectType {
  type InputObjectRes = Map[String, Any]
  def apply(name: String, fields: List[InputField[_]]): InputObjectType[InputObjectRes] =
    InputObjectType(name, None, fieldsFn = Named.checkFieldsFn(fields))
  def apply(name: String, description: String, fields: List[InputField[_]]): InputObjectType[InputObjectRes] =
    InputObjectType(name, Some(description), fieldsFn = Named.checkFieldsFn(fields))

  def apply(name: String, fieldsFn: () => List[InputField[_]]): InputObjectType[InputObjectRes] =
    InputObjectType(name, None, Named.checkFields(fieldsFn))
  def apply(name: String, description: String, fieldsFn: () => List[InputField[_]]): InputObjectType[InputObjectRes] =
    InputObjectType(name, Some(description), Named.checkFields(fieldsFn))
}

case class InputField[T](
    name: String,
    fieldType: InputType[T],
    description: Option[String] = None,
    defaultValue: Option[T] = None) extends InputValue[T] with Named {
  def inputValueType = fieldType
}

case class ListType[T](ofType: OutputType[T]) extends OutputType[Seq[T]] with NullableType
case class ListInputType[T](ofType: InputType[T]) extends InputType[Seq[T]] with NullableType

case class OptionType[T](ofType: OutputType[T]) extends OutputType[Option[T]]
case class OptionInputType[T](ofType: InputType[T]) extends InputType[Option[T]]

sealed trait HasArguments {
  def arguments: List[Argument[_]]
}

case class Directive(
  name: String,
  description: Option[String] = None,
  arguments: List[Argument[_]] = Nil,
  shouldInclude: DirectiveContext => Boolean,
  onOperation: Boolean,
  onFragment: Boolean,
  onField: Boolean) extends HasArguments

case class Schema[Ctx, Val](
    query: ObjectType[Ctx, Val],
    mutation: Option[ObjectType[Ctx, Val]] = None,
    additionalTypes: List[Type with Named] = Nil,
    directives: List[Directive] = BuiltinDirectives) {
  lazy val types: Map[String, (Int, Type with Named)] = {
    def updated(priority: Int, name: String, tpe: Type with Named, result: Map[String, (Int, Type with Named)]) =
      if (result contains name) result else result.updated(name, priority -> tpe)

    def collectTypes(priority: Int, tpe: Type, result: Map[String, (Int, Type with Named)]): Map[String, (Int, Type with Named)] = {
      tpe match {
        case t: Named if result contains t.name => result
        case OptionType(ofType) => collectTypes(priority, ofType, result)
        case OptionInputType(ofType) => collectTypes(priority, ofType, result)
        case ListType(ofType) => collectTypes(priority, ofType, result)
        case ListInputType(ofType) => collectTypes(priority, ofType, result)

        case t @ ScalarType(name, _, _, _, _) => updated(priority, name, t, result)
        case t @ EnumType(name, _, _) => updated(priority, name, t, result)
        case t @ InputObjectType(name, _, _) =>
          t.fields.foldLeft(updated(priority, name, t, result)) {case (acc, field) => collectTypes(priority, field.fieldType, acc)}
        case t: ObjectLikeType[_, _] =>
          val own = t.fields.foldLeft(updated(priority, t.name, t, result)) {
            case (acc, field) =>
              field.arguments.foldLeft(collectTypes(priority, field.fieldType, acc)) {
                case (aacc, arg) => collectTypes(priority, arg.argumentType, aacc)
              }
          }

          t.interfaces.foldLeft(own) {case (acc, interface) => collectTypes(priority, interface, acc)}
        case t @ UnionType(name, _, types) =>
          types.foldLeft(updated(priority, name, t, result)) {case (acc, tpe) => collectTypes(priority, tpe, acc)}
      }
    }

    val schemaTypes = collectTypes(3, introspection.__Schema, Map(BuiltinScalars map (s => s.name -> (4, s)): _*))
    val queryTypes = collectTypes(2, query, schemaTypes)
    val queryTypesWithAdditions = queryTypes ++ additionalTypes.map(t => t.name -> (1, t))
    val queryAndMutTypes = mutation map (collectTypes(1, _, queryTypesWithAdditions)) getOrElse queryTypesWithAdditions

    queryAndMutTypes
  }

  lazy val typeList = types.values.toList.sortBy(t => t._1 + t._2.name).map(_._2)

  lazy val allTypes = types collect {case (name, (_, tpe)) => name -> tpe}
  lazy val inputTypes = types collect {case (name, (_, tpe: InputType[_])) => name -> tpe}
  lazy val outputTypes = types collect {case (name, (_, tpe: OutputType[_])) => name -> tpe}
  lazy val scalarTypes = types collect {case (name, (_, tpe: ScalarType[_])) => name -> tpe}
  lazy val unionTypes: Map[String, UnionType[_]] =
    types.filter(_._2._2.isInstanceOf[UnionType[_]]).mapValues(_._2.asInstanceOf[UnionType[_]])

  lazy val directivesByName = directives groupBy (_.name) mapValues (_.head)

  def getInputType(tpe: ast.Type): Option[InputType[_]] = tpe match {
    case ast.NamedType(name, _) => inputTypes get name map (OptionInputType(_))
    case ast.NotNullType(ofType, _) => getInputType(ofType) collect {case OptionInputType(ot) => ot}
    case ast.ListType(ofType, _) => getInputType(ofType) map (t => OptionInputType(ListInputType(t)))
  }

  def getOutputType(tpe: ast.Type, topLevel: Boolean = false): Option[OutputType[_]] = tpe match {
    case ast.NamedType(name, _) => outputTypes get name map (ot => if (topLevel) ot else OptionType(ot))
    case ast.NotNullType(ofType, _) => getOutputType(ofType) collect {case OptionType(ot) => ot}
    case ast.ListType(ofType, _) => getOutputType(ofType) map (ListType(_))
  }

  lazy val directImplementations: Map[String, List[ObjectLikeType[_, _]]] = {
    typeList
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