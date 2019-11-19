package sangria.schema

import sangria.ast
import sangria.ast.{AstNode, Document}

import language.implicitConversions
import sangria.execution.{FieldTag, SubscriptionField}
import sangria.marshalling.FromInput.{CoercedScalaResult, InputObjectResult}
import sangria.marshalling._
import sangria.{ast, introspection}
import sangria.validation._
import sangria.introspection._
import sangria.renderer.{SchemaFilter, SchemaRenderer}
import sangria.streaming.SubscriptionStreamLike

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import sangria.util.tag._

sealed trait Type {
  def namedType: Type with Named = {
    def getNamedType(tpe: Type): Type with Named =
      tpe match {
        case OptionInputType(ofType) => getNamedType(ofType)
        case OptionType(ofType) => getNamedType(ofType)
        case ListInputType(ofType) => getNamedType(ofType)
        case ListType(ofType) => getNamedType(ofType)
        case n: Named => n
        case t => throw new IllegalStateException("Expected named type, but got: " + t)
      }

    getNamedType(this)
  }
}

sealed trait InputType[+T] extends Type {
  lazy val isOptional = this match {
    case _: OptionInputType[_] => true
    case _ => false
  }

  lazy val isList = this match {
    case _: ListInputType[_] => true
    case _ => false
  }

  lazy val isNamed = !(isOptional && isList)

  lazy val nonOptionalType = this match {
    case tpe: OptionInputType[_] => tpe.ofType
    case tpe => tpe
  }

  def namedInputType: InputType[_] = namedType.asInstanceOf[InputType[_]]
}

sealed trait OutputType[+T] extends Type

sealed trait LeafType extends Type with Named with HasAstInfo
sealed trait CompositeType[T] extends Type with Named with OutputType[T]
sealed trait AbstractType extends Type with Named {
  def name: String

  def typeOf[Ctx](value: Any, schema: Schema[Ctx, _]): Option[ObjectType[Ctx, _]] =
    schema.possibleTypes get name flatMap (_.find(_ isInstanceOf value).asInstanceOf[Option[ObjectType[Ctx, _]]])
}

sealed trait MappedAbstractType[T] extends Type with AbstractType with OutputType[T] {
  def contraMap(value: T): Any
}

sealed trait NullableType
sealed trait UnmodifiedType

sealed trait HasDescription {
  def description: Option[String]
}

sealed trait Named extends HasDescription {
  def name: String

  def rename(newName: String): this.type
}

sealed trait HasDeprecation {
  def deprecationReason: Option[String]
}

sealed trait HasAstInfo {
  def astDirectives: Vector[ast.Directive]
  def astNodes: Vector[ast.AstNode]
}

object Named {
  val NameRegexp = """^[_a-zA-Z][_a-zA-Z0-9]*$""".r

  def isValidName(name: String): Boolean = NameRegexp.pattern.matcher(name).matches()
}

/**
  * Defines a GraphQL scalar value type.
  *
  * `coerceOutput` is allowed to return following scala values:
  *
  *   - String
  *   - Boolean
  *   - Int
  *   - Long
  *   - Float
  *   - Double
  *   - scala.BigInt
  *   - scala.BigDecimal
  *   - sangria.ast.Value (it would be converted to raw scala value before given to a marshalling API)
  *
  * It may also return other values as well as long as underlying marshalling library supports them.
  *
  * You can provide additional meta-information to marshalling API with `scalarInfo`.
  */
case class ScalarType[T](
  name: String,
  description: Option[String] = None,
  coerceUserInput: Any => Either[Violation, T],
  coerceOutput: (T, Set[MarshallerCapability]) => Any,
  coerceInput: ast.Value => Either[Violation, T],
  complexity: Double = 0.0D,
  scalarInfo: Set[ScalarValueInfo] = Set.empty,
  astDirectives: Vector[ast.Directive] = Vector.empty,
  astNodes: Vector[ast.AstNode] = Vector.empty
) extends InputType[T @@ CoercedScalaResult] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named {
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.TypeDefinition = SchemaRenderer.renderType(this)
}

case class ScalarAlias[T, ST](
  aliasFor: ScalarType[ST],
  toScalar: T => ST,
  fromScalar: ST => Either[Violation, T]
) extends InputType[T @@ CoercedScalaResult] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named {
  def name = aliasFor.name
  def description = aliasFor.description
  def rename(newName: String) = copy(aliasFor = aliasFor.rename(newName)).asInstanceOf[this.type]

  def astDirectives = aliasFor.astDirectives
  def astNodes = aliasFor.astNodes
  def toAst: ast.TypeDefinition = SchemaRenderer.renderType(this)
}

sealed trait ObjectLikeType[Ctx, Val] extends OutputType[Val] with CompositeType[Val] with NullableType with UnmodifiedType with Named with HasAstInfo {
  def interfaces: List[InterfaceType[Ctx, _]]
  def fieldsFn: () => List[Field[Ctx, Val]]

  lazy val ownFields = fieldsFn().toVector

  private def removeDuplicates[T, E](list: Vector[T], valueFn: T => E) =
    list.foldLeft((Vector.empty, Vector.empty): (Vector[E], Vector[T])) {
      case (a @ (visited, acc), e) if visited contains valueFn(e) => a
      case ((visited, acc), e) => (visited :+ valueFn(e), acc :+ e)
    }._2

  lazy val allInterfaces: Vector[InterfaceType[Ctx, _]] =
    removeDuplicates(interfaces.toVector.flatMap(i => i +: i.allInterfaces), (i: InterfaceType[Ctx, _]) => i.name)

  lazy val fields: Vector[Field[Ctx, _]] = ownFields ++ interfaces.flatMap(i => i.fields.asInstanceOf[Vector[Field[Ctx, _]]])

  lazy val uniqueFields: Vector[Field[Ctx, _]] = removeDuplicates(fields, (e: Field[Ctx, _]) => e.name)

  lazy val fieldsByName: Map[String, Vector[Field[Ctx, _]]] = fields groupBy (_.name)

  def getField(schema: Schema[_, _], fieldName: String): Vector[Field[Ctx, _]] =
    if (sangria.introspection.MetaFieldNames contains fieldName)
      if (fieldName == SchemaMetaField.name && name == schema.query.name) Vector(SchemaMetaField.asInstanceOf[Field[Ctx, _]])
      else if (fieldName == TypeMetaField.name && name == schema.query.name) Vector(TypeMetaField.asInstanceOf[Field[Ctx, _]])
      else if (fieldName == TypeNameMetaField.name) Vector(TypeNameMetaField.asInstanceOf[Field[Ctx, _]])
      else Vector.empty
    else fieldsByName.getOrElse(fieldName, Vector.empty)

  def toAst: ast.TypeDefinition = SchemaRenderer.renderType(this)
}

case class ObjectType[Ctx, Val: ClassTag] (
  name: String,
  description: Option[String],
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, _]],
  instanceCheck: (Any, Class[_], ObjectType[Ctx, Val]) => Boolean,
  astDirectives: Vector[ast.Directive],
  astNodes: Vector[ast.AstNode]
) extends ObjectLikeType[Ctx, Val] {
  lazy val valClass = implicitly[ClassTag[Val]].runtimeClass

  def withInstanceCheck(fn: (Any, Class[_], ObjectType[Ctx, Val]) => Boolean) =
    copy(instanceCheck = fn)

  def isInstanceOf(value: Any) = instanceCheck(value, valClass, this)

  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
}

object ObjectType {
  def apply[Ctx, Val: ClassTag](name: String, fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = () => fields, Nil, instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)
  def apply[Ctx, Val: ClassTag](name: String, description: String, fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn = () => fields, Nil, instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)
  def apply[Ctx, Val: ClassTag](name: String, interfaces: List[PossibleInterface[Ctx, Val]], fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn = () => fields, interfaces map (_.interfaceType), instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)
  def apply[Ctx, Val: ClassTag](name: String, description: String, interfaces: List[PossibleInterface[Ctx, Val]], fields: List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn = () => fields, interfaces map (_.interfaceType), instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)

  def apply[Ctx, Val: ClassTag](name: String, fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn, Nil, instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)
  def apply[Ctx, Val: ClassTag](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn, Nil, instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)
  def apply[Ctx, Val: ClassTag](name: String, interfaces: List[PossibleInterface[Ctx, Val]], fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, None, fieldsFn, interfaces map (_.interfaceType), instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)
  def apply[Ctx, Val: ClassTag](name: String, description: String, interfaces: List[PossibleInterface[Ctx, Val]], fieldsFn: () => List[Field[Ctx, Val]]): ObjectType[Ctx, Val] =
    ObjectType(name, Some(description), fieldsFn, interfaces map (_.interfaceType), instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)

  def createFromMacro[Ctx, Val: ClassTag](name: String, description: Option[String], interfaces: List[InterfaceType[Ctx, _]], fieldsFn: () => List[Field[Ctx, Val]]) =
    ObjectType(name, description, fieldsFn, interfaces, instanceCheck = defaultInstanceCheck, Vector.empty, Vector.empty)

  implicit def acceptUnitCtx[Ctx, Val](objectType: ObjectType[Unit, Val]): ObjectType[Ctx, Val] =
    objectType.asInstanceOf[ObjectType[Ctx, Val]]

  def defaultInstanceCheck[Ctx, Val]: (Any, Class[_], ObjectType[Ctx, Val]) => Boolean =
    (value, valClass, tpe) => valClass.isAssignableFrom(value.getClass)
}

case class InterfaceType[Ctx, Val](
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[Field[Ctx, Val]],
  interfaces: List[InterfaceType[Ctx, _]],
  manualPossibleTypes: () => List[ObjectType[_, _]],
  astDirectives: Vector[ast.Directive],
  astNodes: Vector[ast.AstNode] = Vector.empty
) extends ObjectLikeType[Ctx, Val] with AbstractType {
  def withPossibleTypes(possible: PossibleObject[Ctx, Val]*) = copy(manualPossibleTypes = () => possible.toList map (_.objectType))
  def withPossibleTypes(possible: () => List[PossibleObject[Ctx, Val]]) = copy(manualPossibleTypes = () => possible() map (_.objectType))
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
}

object InterfaceType {
  val emptyPossibleTypes: () => List[ObjectType[_, _]] = () => Nil

  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = () => fields, Nil, emptyPossibleTypes, Vector.empty, Vector.empty)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn = () => fields, Nil, emptyPossibleTypes, Vector.empty, Vector.empty)
  def apply[Ctx, Val](name: String, fields: List[Field[Ctx, Val]], interfaces: List[PossibleInterface[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn = () => fields, interfaces map (_.interfaceType), emptyPossibleTypes, Vector.empty, Vector.empty)
  def apply[Ctx, Val](name: String, description: String, fields: List[Field[Ctx, Val]], interfaces: List[PossibleInterface[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn = () => fields, interfaces map (_.interfaceType), emptyPossibleTypes, Vector.empty, Vector.empty)

  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn, Nil, emptyPossibleTypes, Vector.empty, Vector.empty)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn, Nil, emptyPossibleTypes, Vector.empty, Vector.empty)
  def apply[Ctx, Val](name: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[PossibleInterface[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, None, fieldsFn, interfaces map (_.interfaceType), emptyPossibleTypes, Vector.empty, Vector.empty)
  def apply[Ctx, Val](name: String, description: String, fieldsFn: () => List[Field[Ctx, Val]], interfaces: List[PossibleInterface[Ctx, Val]]): InterfaceType[Ctx, Val] =
    InterfaceType(name, Some(description), fieldsFn, interfaces map (_.interfaceType), emptyPossibleTypes, Vector.empty, Vector.empty)
}

case class PossibleInterface[Ctx, Concrete](interfaceType: InterfaceType[Ctx, _])

object PossibleInterface extends PossibleInterfaceLowPrioImplicits {
  def apply[Ctx, Abstract, Concrete](interface: InterfaceType[Ctx, Abstract])(implicit ev: PossibleType[Abstract, Concrete]): PossibleInterface[Ctx, Concrete] =
    PossibleInterface[Ctx, Concrete](interface)
  implicit def convert[Ctx, Abstract, Concrete](interface: InterfaceType[Ctx, Abstract])(implicit ev: PossibleType[Abstract, Concrete]): PossibleInterface[Ctx, Concrete] =
    PossibleInterface[Ctx, Concrete](interface)
}

trait PossibleInterfaceLowPrioImplicits {
  implicit def applyUnit[Ctx, Abstract, Concrete](interface: InterfaceType[Ctx, Abstract])(implicit ev: PossibleType[Abstract, Concrete]): PossibleInterface[Unit, Concrete] =
    PossibleInterface[Unit, Concrete](interface.asInstanceOf[InterfaceType[Unit, Abstract]])
}

case class PossibleObject[Ctx, Abstract](objectType: ObjectType[Ctx, _])

object PossibleObject {
  implicit def apply[Ctx, Abstract, Concrete](obj: ObjectType[Ctx, Concrete])(implicit ev: PossibleType[Abstract, Concrete]): PossibleObject[Ctx, Abstract] =
    PossibleObject[Ctx, Abstract](obj)

  implicit def applyUnit[Ctx, Abstract, Concrete](obj: ObjectType[Unit, Concrete])(implicit ev: PossibleType[Abstract, Concrete]): PossibleObject[Ctx, Abstract] =
    PossibleObject[Ctx, Abstract](obj.asInstanceOf[ObjectType[Ctx, Concrete]])
}

trait PossibleType[AbstrType, ConcreteType]

object PossibleType {
  private object SingletonPossibleType extends PossibleType[AnyRef, AnyRef]

  def create[AbstrType, ConcreteType] = SingletonPossibleType.asInstanceOf[PossibleType[AbstrType, ConcreteType]]

  implicit def InheritanceBasedPossibleType[Abstract, Concrete](implicit ev: Concrete <:< Abstract): PossibleType[Abstract, Concrete] =
    create[Abstract, Concrete]
}

case class UnionType[Ctx](
    name: String,
    description: Option[String] = None,
    types: List[ObjectType[Ctx, _]],
    astDirectives: Vector[ast.Directive] = Vector.empty,
    astNodes: Vector[ast.AstNode] = Vector.empty) extends OutputType[Any] with CompositeType[Any] with AbstractType with NullableType with UnmodifiedType with HasAstInfo {
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.TypeDefinition = SchemaRenderer.renderType(this)

  /**
    * Creates a type-safe version of union type which might be useful in cases where the value is wrapped in a type like `Either`.
    */
  def mapValue[T](func: T => Any): OutputType[T] = new UnionType[Ctx](name, description, types, astDirectives, astNodes) with MappedAbstractType[T] {
    override def contraMap(value: T): Any = func(value)
  }.asInstanceOf[OutputType[T]]
}

case class Field[Ctx, Val](
    name: String,
    fieldType: OutputType[_],
    description: Option[String],
    arguments: List[Argument[_]],
    resolve: Context[Ctx, Val] => Action[Ctx, _],
    deprecationReason: Option[String],
    tags: List[FieldTag],
    complexity: Option[(Ctx, Args, Double) => Double],
    manualPossibleTypes: () => List[ObjectType[_, _]],
    astDirectives: Vector[ast.Directive],
    astNodes: Vector[ast.AstNode]) extends Named with HasArguments with HasDeprecation with HasAstInfo {
  def withPossibleTypes(possible: PossibleObject[Ctx, Val]*) = copy(manualPossibleTypes = () => possible.toList map (_.objectType))
  def withPossibleTypes(possible: () => List[PossibleObject[Ctx, Val]]) = copy(manualPossibleTypes = () => possible() map (_.objectType))
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.FieldDefinition = SchemaRenderer.renderField(this)
}

object Field {
  def apply[Ctx, Val, Res, Out](
      name: String,
      fieldType: OutputType[Out],
      description: Option[String] = None,
      arguments: List[Argument[_]] = Nil,
      resolve: Context[Ctx, Val] => Action[Ctx, Res],
      possibleTypes: => List[PossibleObject[_, _]] = Nil,
      tags: List[FieldTag] = Nil,
      complexity: Option[(Ctx, Args, Double) => Double] = None,
      deprecationReason: Option[String] = None)(implicit ev: ValidOutType[Res, Out]): Field[Ctx, Val] =
    Field[Ctx, Val](name, fieldType, description, arguments, resolve, deprecationReason, tags, complexity, () => possibleTypes map (_.objectType), Vector.empty, Vector.empty)

  def subs[Ctx, Val, StreamSource, Res, Out](
    name: String,
    fieldType: OutputType[Out],
    description: Option[String] = None,
    arguments: List[Argument[_]] = Nil,
    resolve: Context[Ctx, Val] => StreamSource,
    possibleTypes: => List[PossibleObject[_, _]] = Nil,
    tags: List[FieldTag] = Nil,
    complexity: Option[(Ctx, Args, Double) => Double] = None,
    deprecationReason: Option[String] = None
  )(implicit stream: SubscriptionStreamLike[StreamSource, Action, Ctx, Res, Out]): Field[Ctx, Val] = {
    val s = stream.subscriptionStream

    Field[Ctx, Val](
      name,
      fieldType,
      description,
      arguments,
      ctx => SubscriptionValue[Ctx, StreamSource, stream.StreamSource](resolve(ctx), s),
      deprecationReason,
      SubscriptionField[stream.StreamSource](s) +: tags,
      complexity,
      () => possibleTypes map (_.objectType),
      Vector.empty,
      Vector.empty)
  }
}

@implicitNotFound(msg = "${Res} is invalid type for the resulting GraphQL type ${Out}.")
trait ValidOutType[-Res, +Out]

object ValidOutType {
  private val valid = new ValidOutType[Any, Any] {}

  implicit def validSubclass[Res, Out](implicit ev: Res <:< Out) = valid.asInstanceOf[ValidOutType[Res, Out]]
  implicit def validNothing[Out] = valid.asInstanceOf[ValidOutType[Nothing, Out]]
  implicit def validOption[Res, Out](implicit ev: Res <:< Out) = valid.asInstanceOf[ValidOutType[Res, Option[Out]]]
}

trait InputValue[T] {
  def name: String
  def inputValueType: InputType[_]
  def description: Option[String]
  def defaultValue: Option[(_, ToInput[_, _])]
}

case class Argument[T](
    name: String,
    argumentType: InputType[_],
    description: Option[String],
    defaultValue: Option[(_, ToInput[_, _])],
    fromInput: FromInput[_],
    astDirectives: Vector[ast.Directive],
    astNodes: Vector[ast.AstNode]) extends InputValue[T] with Named with HasAstInfo {
  def inputValueType = argumentType
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.InputValueDefinition = SchemaRenderer.renderArg(this)
}

object Argument {
  def apply[T, Default](
      name: String,
      argumentType: InputType[T],
      description: String,
      defaultValue: Default)(implicit toInput: ToInput[Default, _], fromInput: FromInput[T], res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, Some(description), Some(defaultValue -> toInput), fromInput, Vector.empty, Vector.empty)

  def apply[T, Default](
      name: String,
      argumentType: InputType[T],
      defaultValue: Default)(implicit toInput: ToInput[Default, _], fromInput: FromInput[T], res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, None, Some(defaultValue -> toInput), fromInput, Vector.empty, Vector.empty)

  def apply[T](
      name: String,
      argumentType: InputType[T],
      description: String)(implicit fromInput: FromInput[T], res: WithoutInputTypeTags[T]): Argument[res.Res] =
    Argument(name, argumentType, Some(description), None, fromInput, Vector.empty, Vector.empty)

  def apply[T](
      name: String,
      argumentType: InputType[T])(implicit fromInput: FromInput[T], res: WithoutInputTypeTags[T]): Argument[res.Res] =
    Argument(name, argumentType, None, None, fromInput, Vector.empty, Vector.empty)

  def createWithoutDefault[T](
      name: String,
      argumentType: InputType[T],
      description: Option[String])(implicit fromInput: FromInput[T], res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, description, None, fromInput, Vector.empty, Vector.empty)

  def createWithDefault[T, Default](
      name: String,
      argumentType: InputType[T],
      description: Option[String],
      defaultValue: Default)(implicit toInput: ToInput[Default, _], fromInput: FromInput[T], res: ArgumentType[T]): Argument[res.Res] =
    Argument(name, argumentType, description, Some(defaultValue -> toInput), fromInput, Vector.empty, Vector.empty)
}

trait WithoutInputTypeTags[T] {
  type Res
}

object WithoutInputTypeTags extends WithoutInputTypeTagsLowPrio {
  implicit def coercedArgTpe[T] = new WithoutInputTypeTags[T @@ CoercedScalaResult] {
    type Res = T
  }

  implicit def coercedOptArgTpe[T] = new WithoutInputTypeTags[Option[T @@ CoercedScalaResult]] {
    type Res = Option[T]
  }

  implicit def coercedSeqOptArgTpe[T] = new WithoutInputTypeTags[Seq[Option[T @@ CoercedScalaResult]]] {
    type Res = Seq[Option[T]]
  }

  implicit def coercedOptSeqArgTpe[T] = new WithoutInputTypeTags[Option[Seq[T @@ CoercedScalaResult]]] {
    type Res = Option[Seq[T]]
  }

  implicit def coercedOptSeqOptArgTpe[T] = new WithoutInputTypeTags[Option[Seq[Option[T @@ CoercedScalaResult]]]] {
    type Res = Option[Seq[Option[T]]]
  }

  implicit def ioArgTpe[T] = new WithoutInputTypeTags[T @@ InputObjectResult] {
    type Res = T
  }

  implicit def ioOptArgTpe[T] = new WithoutInputTypeTags[Option[T @@ InputObjectResult]] {
    type Res = Option[T]
  }

  implicit def ioSeqOptArgTpe[T] = new WithoutInputTypeTags[Seq[Option[T @@ InputObjectResult]]] {
    type Res = Seq[Option[T]]
  }

  implicit def ioOptSeqArgTpe[T] = new WithoutInputTypeTags[Option[Seq[T @@ InputObjectResult]]] {
    type Res = Option[Seq[T]]
  }

  implicit def ioOptSeqOptArgTpe[T] = new WithoutInputTypeTags[Option[Seq[Option[T @@ InputObjectResult]]]] {
    type Res = Option[Seq[Option[T]]]
  }
}

trait WithoutInputTypeTagsLowPrio {
  implicit def defaultArgTpe[T] = new WithoutInputTypeTags[T] {
    type Res = T
  }
}

trait ArgumentType[T] {
  type Res
}

object ArgumentType extends ArgumentTypeLowPrio {
  implicit def coercedArgTpe[T] = new ArgumentType[T @@ CoercedScalaResult] {
    type Res = T
  }

  implicit def coercedOptArgTpe[T] = new ArgumentType[Option[T @@ CoercedScalaResult]] {
    type Res = T
  }

  implicit def coercedSeqOptArgTpe[T] = new ArgumentType[Seq[Option[T @@ CoercedScalaResult]]] {
    type Res = Seq[Option[T]]
  }

  implicit def coercedOptSeqArgTpe[T] = new ArgumentType[Option[Seq[T @@ CoercedScalaResult]]] {
    type Res = Seq[T]
  }

  implicit def coercedOptSeqOptArgTpe[T] = new ArgumentType[Option[Seq[Option[T @@ CoercedScalaResult]]]] {
    type Res = Seq[Option[T]]
  }

  implicit def ioArgTpe[T] = new ArgumentType[T @@ InputObjectResult] {
    type Res = T
  }

  implicit def ioOptArgTpe[T] = new ArgumentType[Option[T @@ InputObjectResult]] {
    type Res = T
  }

  implicit def ioSeqOptArgTpe[T] = new ArgumentType[Seq[Option[T @@ InputObjectResult]]] {
    type Res = Seq[Option[T]]
  }

  implicit def ioOptSeqArgTpe[T] = new ArgumentType[Option[Seq[T @@ InputObjectResult]]] {
    type Res = Seq[T]
  }

  implicit def ioOptSeqOptArgTpe[T] = new ArgumentType[Option[Seq[Option[T @@ InputObjectResult]]]] {
    type Res = Seq[Option[T]]
  }
}

trait ArgumentTypeLowPrio extends ArgumentTypeLowestPrio {
  implicit def optionArgTpe[T] = new ArgumentType[Option[T]] {
    type Res = T
  }
}

trait ArgumentTypeLowestPrio {
  implicit def defaultArgTpe[T] = new ArgumentType[T] {
    type Res = T
  }
}

case class EnumType[T](
    name: String,
    description: Option[String] = None,
    values: List[EnumValue[T]],
    astDirectives: Vector[ast.Directive] = Vector.empty,
    astNodes: Vector[ast.AstNode] = Vector.empty) extends InputType[T @@ CoercedScalaResult] with OutputType[T] with LeafType with NullableType with UnmodifiedType with Named with HasAstInfo {
  lazy val byName = values groupBy (_.name) mapValues (_.head)
  lazy val byValue = values groupBy (_.value) mapValues (_.head)

  def coerceUserInput(value: Any): Either[Violation, (T, Boolean)] = value match {
    case valueName: String => byName get valueName map (v => Right(v.value -> v.deprecationReason.isDefined)) getOrElse Left(EnumValueCoercionViolation(valueName, name, values.map(_.name)))
    case v if byValue exists (_._1 == v) => Right(v.asInstanceOf[T] -> byValue(v.asInstanceOf[T]).deprecationReason.isDefined)
    case _ => Left(EnumCoercionViolation)
  }

  def coerceInput(value: ast.Value): Either[Violation, (T, Boolean)] = value match {
    case ast.EnumValue(valueName, _, _) => byName get valueName map (v => Right(v.value -> v.deprecationReason.isDefined)) getOrElse Left(EnumValueCoercionViolation(valueName, name, values.map(_.name)))
    case _ => Left(EnumCoercionViolation)
  }

  def coerceOutput(value: T): String = byValue(value).name

  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.TypeDefinition = SchemaRenderer.renderType(this)
}

case class EnumValue[+T](
    name: String,
    description: Option[String] = None,
    value: T,
    deprecationReason: Option[String] = None,
    astDirectives: Vector[ast.Directive] = Vector.empty,
    astNodes: Vector[ast.AstNode] = Vector.empty) extends Named with HasDeprecation with HasAstInfo {
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.EnumValueDefinition = SchemaRenderer.renderEnumValue(this)
}

case class InputObjectType[T](
  name: String,
  description: Option[String] = None,
  fieldsFn: () => List[InputField[_]],
  astDirectives: Vector[ast.Directive],
  astNodes: Vector[ast.AstNode]
) extends InputType[T @@ InputObjectResult] with NullableType with UnmodifiedType with Named with HasAstInfo {
  lazy val fields = fieldsFn()
  lazy val fieldsByName = fields groupBy(_.name) mapValues(_.head)

  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.TypeDefinition = SchemaRenderer.renderType(this)
}

object InputObjectType {
  type DefaultInput = Map[String, Any]

  def apply[T](name: String, fields: List[InputField[_]])(implicit res: InputObjectDefaultResult[T]): InputObjectType[res.Res] =
    InputObjectType(name, None, fieldsFn = () => fields, Vector.empty, Vector.empty)
  def apply[T](name: String, description: String, fields: List[InputField[_]])(implicit res: InputObjectDefaultResult[T]): InputObjectType[res.Res] =
    InputObjectType(name, Some(description), fieldsFn = () => fields, Vector.empty, Vector.empty)

  def apply[T](name: String, fieldsFn: () => List[InputField[_]])(implicit res: InputObjectDefaultResult[T]): InputObjectType[res.Res] =
    InputObjectType(name, None, fieldsFn, Vector.empty, Vector.empty)
  def apply[T](name: String, description: String, fieldsFn: () => List[InputField[_]])(implicit res: InputObjectDefaultResult[T]): InputObjectType[res.Res] =
    InputObjectType(name, Some(description), fieldsFn, Vector.empty, Vector.empty)

  def createFromMacro[T](name: String, description: Option[String] = None, fieldsFn: () => List[InputField[_]]) =
    InputObjectType[T](name, description, fieldsFn, Vector.empty, Vector.empty)
}

trait InputObjectDefaultResult[T] {
  type Res
}

object InputObjectDefaultResult extends InputObjectDefaultResultLowPrio {
  implicit def nothingResult = new InputObjectDefaultResult[Nothing] {
    override type Res = InputObjectType.DefaultInput
  }
}

trait InputObjectDefaultResultLowPrio {
  implicit def defaultResult[T] = new InputObjectDefaultResult[T] {
    override type Res = T
  }
}

case class InputField[T](
  name: String,
  fieldType: InputType[T],
  description: Option[String],
  defaultValue: Option[(_, ToInput[_, _])],
  astDirectives: Vector[ast.Directive],
  astNodes: Vector[ast.AstNode]
) extends InputValue[T] with Named with HasAstInfo {
  def inputValueType = fieldType
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.InputValueDefinition = SchemaRenderer.renderInputField(this)
}

object InputField {
  def apply[T, Default](name: String, fieldType: InputType[T], description: String, defaultValue: Default)(implicit toInput: ToInput[Default, _], res: WithoutInputTypeTags[T]): InputField[res.Res] =
    InputField(name, fieldType, Some(description), Some(defaultValue -> toInput), Vector.empty, Vector.empty).asInstanceOf[InputField[res.Res]]

  def apply[T, Default](name: String, fieldType: InputType[T], defaultValue: Default)(implicit toInput: ToInput[Default, _], res: WithoutInputTypeTags[T]): InputField[res.Res] =
    InputField(name, fieldType, None, Some(defaultValue -> toInput), Vector.empty, Vector.empty).asInstanceOf[InputField[res.Res]]

  def apply[T](name: String, fieldType: InputType[T], description: String)(implicit res: WithoutInputTypeTags[T]): InputField[res.Res] =
    InputField(name, fieldType, Some(description), None, Vector.empty, Vector.empty).asInstanceOf[InputField[res.Res]]

  def apply[T](name: String, fieldType: InputType[T])(implicit res: WithoutInputTypeTags[T]): InputField[res.Res] =
    InputField(name, fieldType, None, None, Vector.empty, Vector.empty).asInstanceOf[InputField[res.Res]]

  def createFromMacroWithDefault[T, Default](
    name: String, fieldType: InputType[T], description: Option[String], defaultValue: Default
  )(implicit toInput: ToInput[Default, _], res: WithoutInputTypeTags[T]): InputField[res.Res] =
    InputField(name, fieldType, description, Some(defaultValue -> toInput), Vector.empty, Vector.empty).asInstanceOf[InputField[res.Res]]

  def createFromMacroWithoutDefault[T](name: String, fieldType: InputType[T], description: Option[String])(implicit res: WithoutInputTypeTags[T]): InputField[res.Res] =
    InputField(name, fieldType, description, None, Vector.empty, Vector.empty).asInstanceOf[InputField[res.Res]]
}

case class ListType[T](ofType: OutputType[T]) extends OutputType[Seq[T]] with NullableType
case class ListInputType[T](ofType: InputType[T]) extends InputType[Seq[T]] with NullableType

case class OptionType[T](ofType: OutputType[T]) extends OutputType[Option[T]]
case class OptionInputType[T](ofType: InputType[T]) extends InputType[Option[T]]

sealed trait HasArguments {
  def arguments: List[Argument[_]]
}

object DirectiveLocation extends Enumeration {
  val ArgumentDefinition = Value
  val Enum = Value
  val EnumValue = Value
  val Field = Value
  val FieldDefinition = Value
  val FragmentDefinition = Value
  val FragmentSpread = Value
  val InlineFragment = Value
  val InputFieldDefinition = Value
  val InputObject = Value
  val Interface = Value
  val Mutation = Value
  val Object = Value
  val Query = Value
  val Scalar = Value
  val Schema = Value
  val Subscription = Value
  val Union = Value
  val VariableDefinition = Value

  def fromString(location: String): DirectiveLocation.Value = location match {
    case "QUERY" => Query
    case "MUTATION" => Mutation
    case "SUBSCRIPTION" => Subscription
    case "FIELD" => Field
    case "FRAGMENT_DEFINITION" => FragmentDefinition
    case "FRAGMENT_SPREAD" => FragmentSpread
    case "INLINE_FRAGMENT" => InlineFragment
    case "VARIABLE_DEFINITION" => VariableDefinition

    case "SCHEMA" => Schema
    case "SCALAR" => Scalar
    case "OBJECT" => Object
    case "FIELD_DEFINITION" => FieldDefinition
    case "ARGUMENT_DEFINITION" => ArgumentDefinition
    case "INTERFACE" => Interface
    case "UNION" => Union
    case "ENUM" => Enum
    case "ENUM_VALUE" => EnumValue
    case "INPUT_OBJECT" => InputObject
    case "INPUT_FIELD_DEFINITION" => InputFieldDefinition
  }

  def toSpecString(location: DirectiveLocation.Value): String = location match {
    case Query => "QUERY"
    case Mutation => "MUTATION"
    case Subscription => "SUBSCRIPTION"
    case Field => "FIELD"
    case FragmentDefinition => "FRAGMENT_DEFINITION"
    case FragmentSpread => "FRAGMENT_SPREAD"
    case InlineFragment => "INLINE_FRAGMENT"
    case VariableDefinition => "VARIABLE_DEFINITION"

    case Schema => "SCHEMA"
    case Scalar => "SCALAR"
    case Object => "OBJECT"
    case FieldDefinition => "FIELD_DEFINITION"
    case ArgumentDefinition => "ARGUMENT_DEFINITION"
    case Interface => "INTERFACE"
    case Union => "UNION"
    case Enum => "ENUM"
    case EnumValue => "ENUM_VALUE"
    case InputObject => "INPUT_OBJECT"
    case InputFieldDefinition => "INPUT_FIELD_DEFINITION"
  }
}

case class Directive(
    name: String,
    description: Option[String] = None,
    arguments: List[Argument[_]] = Nil,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    shouldInclude: DirectiveContext => Boolean = _ => true) extends HasArguments with Named {
  def rename(newName: String) = copy(name = newName).asInstanceOf[this.type]
  def toAst: ast.DirectiveDefinition = SchemaRenderer.renderDirective(this)
}

case class Schema[Ctx, Val](
    query: ObjectType[Ctx, Val],
    mutation: Option[ObjectType[Ctx, Val]] = None,
    subscription: Option[ObjectType[Ctx, Val]] = None,
    additionalTypes: List[Type with Named] = Nil,
    description: Option[String] = None,
    directives: List[Directive] = BuiltinDirectives,
    validationRules: List[SchemaValidationRule] = SchemaValidationRule.default,
    astDirectives: Vector[ast.Directive] = Vector.empty,
    astNodes: Vector[ast.AstNode] = Vector.empty) extends HasAstInfo with HasDescription {
  def extend(document: ast.Document, builder: AstSchemaBuilder[Ctx] = AstSchemaBuilder.default[Ctx]): Schema[Ctx, Val] =
    AstSchemaMaterializer.extendSchema(this, document, builder)

  def compare(oldSchema: Schema[_, _]): Vector[SchemaChange] =
    SchemaComparator.compare(oldSchema, this)

  lazy val toAst: Document = SchemaRenderer.schemaAst(this)
  def toAst(filter: SchemaFilter): Document = SchemaRenderer.schemaAst(this, filter)

  def renderPretty: String = toAst.renderPretty
  def renderPretty(filter: SchemaFilter): String = toAst(filter).renderPretty

  def renderCompact: String = toAst.renderCompact
  def renderCompact(filter: SchemaFilter): String = toAst(filter).renderCompact

  lazy val types: Map[String, (Int, Type with Named)] = {
    def sameType(t1: Type, t2: Type): Boolean = {
      val sameSangriaType = t1.getClass.getName == t2.getClass.getName

      (t1, t2) match {
        case (ot1: ObjectType[_, _], ot2: ObjectType[_, _]) => sameSangriaType && (ot1.valClass == ot2.valClass)
        case _ => sameSangriaType
      }
    }

    def typeConflict(name: String, t1: Type, t2: Type, parentInfo: String) =
      (t1, t2) match {
        case (ot1: ObjectType[_, _], ot2: ObjectType[_, _]) =>
          throw SchemaValidationException(Vector(ConflictingObjectTypeCaseClassViolation(name, parentInfo)))

        case _ =>
          val conflictingTypes = List(t1, t2).map(_.getClass.getSimpleName)

          throw SchemaValidationException(Vector(ConflictingTypeDefinitionViolation(
            name, conflictingTypes, parentInfo)))
      }

    def updated(priority: Int, name: String, tpe: Type with Named, result: Map[String, (Int, Type with Named)], parentInfo: String) =
      result get name match {
        case Some(found) if !sameType(found._2, tpe) => typeConflict(name, found._2, tpe, parentInfo)
        case Some(_) => result
        case None => result.updated(name, priority -> tpe)
      }

    def collectTypes(parentInfo: String, priority: Int, tpe: Type, result: Map[String, (Int, Type with Named)]): Map[String, (Int, Type with Named)] = {
      tpe match {
        case null =>
          throw new IllegalStateException(
            s"A `null` value was provided instead of type for $parentInfo.\n" +
            "This can happen if you have recursive type definition or circular references within your type graph.\n" +
            "Please use no-arg function to provide fields for such types.\n" +
            "You can find more info in the docs: http://sangria-graphql.org/learn/#circular-references-and-recursive-types")
        case t: Named if result contains t.name =>
          result get t.name match {
            case Some(found) if !sameType(found._2, t) && t.isInstanceOf[ScalarAlias[_, _]] && found._2.isInstanceOf[ScalarType[_]] => result
            case Some(found) if !sameType(found._2, t) => typeConflict(t.name, found._2, t, parentInfo)
            case _ => result
          }
        case OptionType(ofType) => collectTypes(parentInfo, priority, ofType, result)
        case OptionInputType(ofType) => collectTypes(parentInfo, priority, ofType, result)
        case ListType(ofType) => collectTypes(parentInfo, priority, ofType, result)
        case ListInputType(ofType) => collectTypes(parentInfo, priority, ofType, result)

        case t @ ScalarType(name, _, _, _, _, _, _, _, _) if BuiltinScalars.contains(t) => updated(40, name, t, result, parentInfo)
        case t @ ScalarType(name, _, _, _, _, _, _, _, _) => updated(priority, name, t, result, parentInfo)
        case ScalarAlias(aliasFor, _, _) => updated(priority, aliasFor.name, aliasFor, result, parentInfo)
        case t @ EnumType(name, _, _, _, _) => updated(priority, name, t, result, parentInfo)
        case t @ InputObjectType(name, _, _, _, _) =>
          t.fields.foldLeft(updated(priority, name, t, result, parentInfo)) {
            case (acc, field) =>
              collectTypes(s"a field '${field.name}' of '$name' input object type", priority, field.fieldType, acc)
          }
        case t: ObjectLikeType[_, _] =>
          val own = t.fields.foldLeft(updated(priority, t.name, t, result, parentInfo)) {
            case (acc, field) =>
              val fromArgs = field.arguments.foldLeft(collectTypes(s"a field '${field.name}' of '${t.name}' type", priority, field.fieldType, acc)) {
                case (aacc, arg) => collectTypes(s"an argument '${arg.name}' defined in field '${field.name}' of '${t.name}' type", priority, arg.argumentType, aacc)
              }

              field.manualPossibleTypes().foldLeft(fromArgs) {
                case (acc, objectType) => collectTypes(s"a manualPossibleType defined in '${t.name}' type", priority, objectType, acc)
              }
          }

          val withPossible = t match {
            case i: InterfaceType[_, _] =>
              i.manualPossibleTypes().foldLeft(own) {
                case (acc, objectType) => collectTypes(s"a manualPossibleType defined in '${i.name}' type", priority, objectType, acc)
              }
            case _ => own
          }

          t.interfaces.foldLeft(withPossible) {
            case (acc, interface) => collectTypes(s"an interface defined in '${t.name}' type", priority, interface, acc)
          }
        case t @ UnionType(name, _, types, _, _) =>
          types.foldLeft(updated(priority, name, t, result, parentInfo)) {case (acc, tpe) => collectTypes(s"a '$name' type", priority, tpe, acc)}
      }
    }

    val schemaTypes = collectTypes("a '__Schema' type", 30, introspection.__Schema, Map.empty)
    val queryTypes = collectTypes("a query type", 20, query, schemaTypes)
    val queryTypesWithAdditions = additionalTypes.foldLeft(queryTypes){case (acc, tpe) => collectTypes("additional type", 10, tpe, acc)}
    val queryAndSubTypes = mutation map (collectTypes("a mutation type", 10, _, queryTypesWithAdditions)) getOrElse queryTypesWithAdditions
    val queryAndSubAndMutTypes = subscription map (collectTypes("a subscription type", 10, _, queryAndSubTypes)) getOrElse queryAndSubTypes

    queryAndSubAndMutTypes
  }

  lazy val typeList: Vector[Type with Named] = types.values.toVector.sortBy(t => t._1 + t._2.name).map(_._2)
  lazy val availableTypeNames: Vector[String] = typeList map (_.name)

  lazy val allTypes: Map[String, Type with Named] = types collect {case (name, (_, tpe)) => name -> tpe}
  lazy val inputTypes = types collect {case (name, (_, tpe: InputType[_])) => name -> tpe}
  lazy val outputTypes = types collect {case (name, (_, tpe: OutputType[_])) => name -> tpe}
  lazy val scalarTypes = types collect {case (name, (_, tpe: ScalarType[_])) => name -> tpe}
  lazy val unionTypes: Map[String, UnionType[_]] =
    types.filter(_._2._2.isInstanceOf[UnionType[_]]).mapValues(_._2.asInstanceOf[UnionType[_]]).toMap

  lazy val directivesByName = directives groupBy (_.name) mapValues (_.head)

  def getInputType(tpe: ast.Type): Option[InputType[_]] = tpe match {
    case ast.NamedType(name, _) => inputTypes get name map (OptionInputType(_))
    case ast.NotNullType(ofType, _) => getInputType(ofType) collect {case OptionInputType(ot) => ot}
    case ast.ListType(ofType, _) => getInputType(ofType) map (t => OptionInputType(ListInputType(t)))
  }

  def getInputType(tpe: IntrospectionTypeRef): Option[InputType[_]] = tpe match {
    case IntrospectionNamedTypeRef(_, name) => inputTypes get name map (OptionInputType(_))
    case IntrospectionNonNullTypeRef(ofType) => getInputType(ofType) collect {case OptionInputType(ot) => ot}
    case IntrospectionListTypeRef(ofType) => getInputType(ofType) map (t => OptionInputType(ListInputType(t)))
  }

  def getOutputType(tpe: ast.Type, topLevel: Boolean = false): Option[OutputType[_]] = tpe match {
    case ast.NamedType(name, _) => outputTypes get name map (ot => if (topLevel) ot else OptionType(ot))
    case ast.NotNullType(ofType, _) => getOutputType(ofType) collect {case OptionType(ot) => ot}
    case ast.ListType(ofType, _) => getOutputType(ofType) map (ListType(_))
  }

  lazy val directImplementations: Map[String, Vector[ObjectLikeType[_, _]]] = {
    typeList
      .collect{case objectLike: ObjectLikeType[_, _] => objectLike}
      .flatMap(objectLike => objectLike.interfaces map (_.name -> objectLike))
      .groupBy(_._1)
      .mapValues(_ map (_._2))
      .toMap
  }

  lazy val implementations: Map[String, Vector[ObjectType[_, _]]] = {
    def findConcreteTypes(tpe: ObjectLikeType[_, _]): Vector[ObjectType[_, _]] = tpe match {
      case obj: ObjectType[_, _] => Vector(obj)
      case interface: InterfaceType[_, _] => directImplementations(interface.name) flatMap findConcreteTypes
    }

    directImplementations map {
      case (name, directImpls) => name -> directImpls.flatMap(findConcreteTypes).groupBy(_.name).map(_._2.head).toVector
    }
  }

  lazy val possibleTypes: Map[String, Vector[ObjectType[_, _]]] =
    implementations ++ unionTypes.values.map(ut => ut.name -> ut.types.toVector)

  def isPossibleType(baseTypeName: String, tpe: ObjectType[_, _]) =
    possibleTypes get baseTypeName exists (_ exists (_.name == tpe.name))

  def analyzer(query: Document) = SchemaBasedDocumentAnalyzer(this, query)

  SchemaValidationRule.validateWithException(this, validationRules)
}

object Schema {
  def isBuiltInType(typeName: String) =
    BuiltinScalarsByName.contains(typeName) || IntrospectionTypesByName.contains(typeName)

  def isBuiltInGraphQLType(typeName: String) =
    BuiltinGraphQLScalarsByName.contains(typeName) || IntrospectionTypesByName.contains(typeName)

  def isBuiltInSangriaType(typeName: String) =
    BuiltinSangriaScalarsByName.contains(typeName) || IntrospectionTypesByName.contains(typeName)

  def isBuiltInDirective(directiveName: String) =
    BuiltinDirectivesByName.contains(directiveName)

  def isIntrospectionType(typeName: String) =
    IntrospectionTypesByName.contains(typeName)

  def getBuiltInType(typeName: String): Option[Type with Named] =
    BuiltinScalarsByName.get(typeName) orElse IntrospectionTypesByName.get(typeName)

  /**
    * Build a `Schema` for use by client tools.
    *
    * Given the result of a client running the introspection query, creates and
    * returns a `Schema` instance which can be then used with all sangria
    * tools, but cannot be used to execute a query, as introspection does not
    * represent the "resolver", "parse" or "serialize" functions or any other
    * server-internal mechanisms.
    *
    * @param introspectionResult the result of introspection query
    */
  def buildFromIntrospection[T : InputUnmarshaller](introspectionResult: T) =
    IntrospectionSchemaMaterializer.buildSchema[T](introspectionResult)

  /**
    * Build a `Schema` for use by client tools.
    *
    * Given the result of a client running the introspection query, creates and
    * returns a `Schema` instance which can be then used with all sangria
    * tools, but cannot be used to execute a query, as introspection does not
    * represent the "resolver", "parse" or "serialize" functions or any other
    * server-internal mechanisms.
    *
    * @param introspectionResult the result of introspection query
    */
  def buildFromIntrospection[Ctx, T : InputUnmarshaller](introspectionResult: T, builder: IntrospectionSchemaBuilder[Ctx]) =
    IntrospectionSchemaMaterializer.buildSchema[Ctx, T](introspectionResult, builder)

  def buildFromAst(document: ast.Document) =
    AstSchemaMaterializer.buildSchema(document)

  def buildFromAst[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]) =
    AstSchemaMaterializer.buildSchema[Ctx](document, builder)

  def buildStubFromAst(document: ast.Document) =
    AstSchemaMaterializer.buildSchema(Document.emptyStub + document)

  def buildStubFromAst[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]) =
    AstSchemaMaterializer.buildSchema[Ctx](Document.emptyStub + document, builder)

  def buildDefinitions(document: ast.Document) =
    AstSchemaMaterializer.definitions(document)

  def buildDefinitions[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]) =
    AstSchemaMaterializer.definitions[Ctx](document, builder)
}
