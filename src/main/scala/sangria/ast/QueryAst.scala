package sangria.ast

import org.parboiled2.Position
import sangria.parser.SourceMapper
import sangria.renderer.QueryRenderer

import scala.collection.immutable.ListMap

case class Document(definitions: List[Definition], trailingComments: List[Comment] = Nil, position: Option[Position] = None, sourceMapper: Option[SourceMapper] = None) extends AstNode with WithTrailingComments {
  lazy val operations = Map(definitions collect {case op: OperationDefinition ⇒ op.name → op}: _*)
  lazy val fragments = Map(definitions collect {case fragment: FragmentDefinition ⇒ fragment.name → fragment}: _*)
  lazy val source = sourceMapper map (_.source)

  def operationType(operationName: Option[String] = None): Option[OperationType] =
    operation(operationName) map (_.operationType)

  def operation(operationName: Option[String] = None): Option[OperationDefinition] =
    if (operations.size != 1 && operationName.isEmpty)
      None
    else
      operationName flatMap (opName ⇒ operations get Some(opName)) orElse operations.values.headOption

  def withoutSourceMapper = copy(sourceMapper = None)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Document]

  override def equals(other: Any): Boolean = other match {
    case that: Document ⇒
      (that canEqual this) &&
        definitions == that.definitions &&
        position == that.position
    case _ ⇒ false
  }

  /**
    * Merges two documents. The `sourceMapper` is lost along the way.
    */
  def merge(other: Document) = Document.merge(Vector(this, other))

  override def hashCode(): Int =
    Seq(definitions, position).map(_.hashCode()).foldLeft(0)((a, b) ⇒ 31 * a + b)
}

object Document {
  /**
    * Provided a collection of ASTs, presumably each from different files,
    * concatenate the ASTs together into batched AST, useful for validating many
    * GraphQL source files which together represent one conceptual application.
    *
    * The result of the merge will loose the `sourceMapper` and `position` since
    * connection to the original string source is lost.
    */
  def merge(documents: Traversable[Document]): Document =
    Document(documents.toList.flatMap(_.definitions))
}

sealed trait ConditionalFragment extends AstNode {
  def typeConditionOpt: Option[NamedType]
}

sealed trait WithComments extends AstNode {
  def comments: List[Comment]
}

sealed trait WithTrailingComments {
  def trailingComments: List[Comment]
}

sealed trait SelectionContainer extends AstNode with WithComments with WithTrailingComments {
  def selections: List[Selection]
  def position: Option[Position]
}

sealed trait Definition extends AstNode

case class OperationDefinition(
  operationType: OperationType = OperationType.Query,
  name: Option[String] = None,
  variables: List[VariableDefinition] = Nil,
  directives: List[Directive] = Nil,
  selections: List[Selection],
  comments: List[Comment] = Nil,
  trailingComments: List[Comment] = Nil,
  position: Option[Position] = None) extends Definition with WithDirectives with SelectionContainer

case class FragmentDefinition(
    name: String,
    typeCondition: NamedType,
    directives: List[Directive],
    selections: List[Selection],
    comments: List[Comment] = Nil,
    trailingComments: List[Comment] = Nil,
    position: Option[Position] = None) extends Definition with ConditionalFragment with WithDirectives with SelectionContainer {
  lazy val typeConditionOpt = Some(typeCondition)
}

sealed trait OperationType

object OperationType {
  case object Query extends OperationType
  case object Mutation extends OperationType
  case object Subscription extends OperationType
}

case class VariableDefinition(
  name: String,
  tpe: Type,
  defaultValue: Option[Value],
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends AstNode with WithComments

sealed trait Type extends AstNode {
  def namedType: NamedType = {
    @annotation.tailrec
    def loop(tpe: Type): NamedType = tpe match {
      case NotNullType(ofType, _) ⇒ loop(ofType)
      case ListType(ofType, _) ⇒ loop(ofType)
      case named: NamedType ⇒ named
    }

    loop(this)
  }
}

case class NamedType(name: String, position: Option[Position] = None) extends Type
case class NotNullType(ofType: Type, position: Option[Position] = None) extends Type
case class ListType(ofType: Type, position: Option[Position] = None) extends Type

sealed trait Selection extends AstNode with WithDirectives with WithComments

case class Field(
    alias: Option[String],
    name: String,
    arguments: List[Argument],
    directives: List[Directive],
    selections: List[Selection],
    comments: List[Comment] = Nil,
    trailingComments: List[Comment] = Nil,
    position: Option[Position] = None) extends Selection with SelectionContainer {
  lazy val outputName = alias getOrElse name
}

case class FragmentSpread(
  name: String,
  directives: List[Directive],
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends Selection

case class InlineFragment(
    typeCondition: Option[NamedType],
    directives: List[Directive],
    selections: List[Selection],
    comments: List[Comment] = Nil,
    trailingComments: List[Comment] = Nil,
    position: Option[Position] = None) extends Selection with ConditionalFragment with SelectionContainer {
  def typeConditionOpt = typeCondition
}

sealed trait NameValue extends AstNode with WithComments {
  def name: String
  def value: Value
}

sealed trait WithDirectives extends AstNode {
  def directives: List[Directive]
}

case class Directive(name: String, arguments: List[Argument], comments: List[Comment] = Nil, position: Option[Position] = None) extends AstNode
case class Argument(name: String, value: Value, comments: List[Comment] = Nil, position: Option[Position] = None) extends NameValue

sealed trait Value extends AstNode with WithComments {
  override def renderPretty: String = QueryRenderer.render(this, QueryRenderer.PrettyInput)
}

sealed trait ScalarValue extends Value

case class IntValue(value: Int, comments: List[Comment] = Nil, position: Option[Position] = None) extends ScalarValue
case class BigIntValue(value: BigInt, comments: List[Comment] = Nil, position: Option[Position] = None) extends ScalarValue
case class FloatValue(value: Double, comments: List[Comment] = Nil, position: Option[Position] = None) extends ScalarValue
case class BigDecimalValue(value: BigDecimal, comments: List[Comment] = Nil, position: Option[Position] = None) extends ScalarValue
case class StringValue(value: String, comments: List[Comment] = Nil, position: Option[Position] = None) extends ScalarValue
case class BooleanValue(value: Boolean, comments: List[Comment] = Nil, position: Option[Position] = None) extends ScalarValue
case class EnumValue(value: String, comments: List[Comment] = Nil, position: Option[Position] = None) extends Value
case class ListValue(values: List[Value], comments: List[Comment] = Nil, position: Option[Position] = None) extends Value
case class VariableValue(name: String, comments: List[Comment] = Nil, position: Option[Position] = None) extends Value
case class NullValue(comments: List[Comment] = Nil, position: Option[Position] = None) extends Value
case class ObjectValue(fields: List[ObjectField], comments: List[Comment] = Nil, position: Option[Position] = None) extends Value {
  lazy val fieldsByName =
    fields.foldLeft(ListMap.empty[String, Value]) {
      case (acc, field) ⇒ acc + (field.name → field.value)
    }
}

case class ObjectField(name: String, value: Value, comments: List[Comment] = Nil, position: Option[Position] = None) extends NameValue

case class Comment(text: String, position: Option[Position] = None) extends AstNode

// Schema Definition

case class ScalarTypeDefinition(
  name: String,
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeDefinition

case class FieldDefinition(
  name: String,
  fieldType: Type,
  arguments: List[InputValueDefinition],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends SchemaAstNode with WithDirectives

case class InputValueDefinition(
  name: String,
  valueType: Type,
  defaultValue: Option[Value],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends SchemaAstNode with WithDirectives

case class ObjectTypeDefinition(
  name: String,
  interfaces: List[NamedType],
  fields: List[FieldDefinition],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  trailingComments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeDefinition with WithTrailingComments

case class InterfaceTypeDefinition(
  name: String,
  fields: List[FieldDefinition],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  trailingComments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeDefinition with WithTrailingComments

case class UnionTypeDefinition(
  name: String,
  types: List[NamedType],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeDefinition

case class EnumTypeDefinition(
  name: String,
  values: List[EnumValueDefinition],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  trailingComments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeDefinition with WithTrailingComments

case class EnumValueDefinition(
  name: String,
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends SchemaAstNode with WithDirectives

case class InputObjectTypeDefinition(
  name: String,
  fields: List[InputValueDefinition],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  trailingComments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeDefinition with WithTrailingComments

case class TypeExtensionDefinition(
  definition: ObjectTypeDefinition,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeSystemDefinition

case class DirectiveDefinition(
  name: String,
  arguments: List[InputValueDefinition],
  locations: List[DirectiveLocation],
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeSystemDefinition

case class DirectiveLocation(
  name: String,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends SchemaAstNode

case class SchemaDefinition(
  operationTypes: List[OperationTypeDefinition],
  directives: List[Directive] = Nil,
  comments: List[Comment] = Nil,
  trailingComments: List[Comment] = Nil,
  position: Option[Position] = None) extends TypeSystemDefinition with WithTrailingComments with WithDirectives

case class OperationTypeDefinition(
  operation: OperationType,
  tpe: NamedType,
  comments: List[Comment] = Nil,
  position: Option[Position] = None) extends SchemaAstNode

sealed trait AstNode {
  def position: Option[Position]
  def cacheKeyHash: Int = System.identityHashCode(this)

  def renderPretty: String = QueryRenderer.render(this, QueryRenderer.Pretty)
  def renderCompact: String = QueryRenderer.render(this, QueryRenderer.Compact)
}

sealed trait SchemaAstNode extends AstNode with WithComments
sealed trait TypeSystemDefinition extends SchemaAstNode with Definition
sealed trait TypeDefinition extends TypeSystemDefinition with WithDirectives {
  def name: String
}

object AstNode {
  def withoutPosition[T <: AstNode](node: T, stripComments: Boolean = false): T = node match {
    case n: Document ⇒
      n.copy(
        definitions = n.definitions map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None,
        sourceMapper = None).asInstanceOf[T]
    case n: OperationDefinition ⇒
      n.copy(
        variables = n.variables map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        selections = n.selections map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: FragmentDefinition ⇒
      n.copy(
        typeCondition = withoutPosition(n.typeCondition),
        directives = n.directives map (withoutPosition(_, stripComments)),
        selections = n.selections map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: VariableDefinition ⇒
      n.copy(
        tpe = withoutPosition(n.tpe, stripComments),
        defaultValue = n.defaultValue map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: NamedType ⇒ n.copy(position = None).asInstanceOf[T]
    case n: NotNullType ⇒ n.copy(ofType = withoutPosition(n.ofType, stripComments), position = None).asInstanceOf[T]
    case n: ListType ⇒ n.copy(ofType = withoutPosition(n.ofType, stripComments), position = None).asInstanceOf[T]
    case n: Field ⇒
      n.copy(
        arguments = n.arguments map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        selections = n.selections map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: FragmentSpread ⇒
      n.copy(
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: InlineFragment ⇒
      n.copy(
        typeCondition = n.typeCondition map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        selections = n.selections map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: Directive ⇒
      n.copy(
        arguments = n.arguments map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: Argument ⇒
      n.copy(
        value = withoutPosition(n.value, stripComments),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: IntValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: BigIntValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: FloatValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: BigDecimalValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: StringValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: BooleanValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: NullValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: EnumValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: ListValue ⇒
      n.copy(
        values = n.values map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: ObjectValue ⇒
      n.copy(
        fields = n.fields map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: ObjectField ⇒
      n.copy(
        value = withoutPosition(n.value, stripComments),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: VariableValue ⇒ 
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)), 
        position = None).asInstanceOf[T]
    case n: Comment ⇒ n.copy(position = None).asInstanceOf[T]

    case n: ScalarTypeDefinition ⇒
      n.copy(
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: FieldDefinition ⇒
      n.copy(
        fieldType = withoutPosition(n.fieldType, stripComments),
        arguments = n.arguments map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: InputValueDefinition ⇒
      n.copy(
        valueType = withoutPosition(n.valueType, stripComments),
        defaultValue = n.defaultValue map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: ObjectTypeDefinition ⇒
      n.copy(
        interfaces = n.interfaces map (withoutPosition(_, stripComments)),
        fields = n.fields map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: InterfaceTypeDefinition ⇒
      n.copy(
        fields = n.fields map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: UnionTypeDefinition ⇒
      n.copy(
        types = n.types map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: EnumTypeDefinition ⇒
      n.copy(
        values = n.values map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: EnumValueDefinition ⇒
      n.copy(
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: InputObjectTypeDefinition ⇒
      n.copy(
        fields = n.fields map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: TypeExtensionDefinition ⇒
      n.copy(
        definition = withoutPosition(n.definition, stripComments),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: DirectiveDefinition ⇒
      n.copy(
        arguments = n.arguments map (withoutPosition(_, stripComments)),
        locations = n.locations map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: DirectiveLocation ⇒
      n.copy(
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: SchemaDefinition ⇒
      n.copy(
        operationTypes = n.operationTypes map (withoutPosition(_, stripComments)),
        directives = n.directives map (withoutPosition(_, stripComments)),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        trailingComments = if (stripComments) Nil else n.trailingComments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
    case n: OperationTypeDefinition ⇒
      n.copy(
        tpe = withoutPosition(n.tpe, stripComments),
        comments = if (stripComments) Nil else n.comments map (withoutPosition(_, stripComments)),
        position = None).asInstanceOf[T]
  }
}