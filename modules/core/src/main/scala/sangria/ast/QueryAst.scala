package sangria.ast

import sangria.execution.InputDocumentMaterializer
import sangria.marshalling.{FromInput, InputUnmarshaller}
import sangria.parser.{AggregateSourceMapper, DeliveryScheme, SourceMapper}
import sangria.renderer.QueryRenderer
import sangria.validation.DocumentAnalyzer
import sangria.schema.{InputType, Schema}
import sangria.validation.TypeInfo
import sangria.visitor._

import scala.util.control.Breaks._
import scala.collection.immutable.ListMap

/** A complete GraphQL request operated on by a GraphQL service.
  *
  * @param definitions The definitions, which primarily constitute the document.
  * @param sourceMapper
  *
  * @see [[https://spec.graphql.org/June2018/#Document]]
  */
case class Document(
    definitions: Vector[Definition],
    override val trailingComments: Vector[Comment] = Vector.empty,
    override val location: Option[AstLocation] = None,
    sourceMapper: Option[SourceMapper] = None)
    extends AstNode
    with WithTrailingComments {
  /** Map of operation name to its definition. */
  lazy val operations: Map[Option[String], OperationDefinition] = Map(definitions.collect { case op: OperationDefinition =>
    op.name -> op
  }: _*)

  /** Map of fragment name to its definition. */
  lazy val fragments: Map[String, FragmentDefinition] = Map(definitions.collect { case fragment: FragmentDefinition =>
    fragment.name -> fragment
  }: _*)

  lazy val source: Option[String] = sourceMapper.map(_.source)

  def operationType(operationName: Option[String] = None): Option[OperationType] =
    operation(operationName).map(_.operationType)

  /** Return the operation for the given name.
    *
    * @return `None`, if no operations are defined or if the given name is ambiguous
    */
  def operation(operationName: Option[String] = None): Option[OperationDefinition] =
    if (operationName.isEmpty && operations.size != 1)
      None
    else if (operationName.isEmpty && operations.size == 1)
      Some(operations.head._2)
    else
      operationName
        .flatMap(opName => operations.get(Some(opName)))
        .orElse(operations.values.headOption)  //FIXME This appears to return the first operation if the named one doesn't exist?

  def withoutSourceMapper: Document = copy(sourceMapper = None)

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Document]

  /** Merges two documents. The `sourceMapper`s are combined. */
  def merge(other: Document): Document = Document.merge(Vector(this, other))

  /** An alias for `merge`
    */
  def +(other: Document): Document = merge(other)

  lazy val analyzer: DocumentAnalyzer = DocumentAnalyzer(this)

  lazy val separateOperations: Map[Option[String], Document] = analyzer.separateOperations

  def separateOperation(definition: OperationDefinition): Document = analyzer.separateOperation(definition)
  def separateOperation(operationName: Option[String]): Option[Document] = analyzer.separateOperation(operationName)

  override def equals(other: Any): Boolean = other match {
    case that: Document =>
      that.canEqual(this) &&
        definitions == that.definitions &&
        location == that.location
    case _ => false
  }

  private[this] lazy val hash =
    Seq(definitions, location).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def hashCode(): Int = hash
}

object Document {

  /** Provided a collection of ASTs, presumably each from different files, concatenate the ASTs
    * together into batched AST, useful for validating many GraphQL source files which together
    * represent one conceptual application.
    *
    * The result `Document` will retain correlation to the original `sourceMapper`s.
    */
  def merge(documents: Traversable[Document]): Document = {
    val originalSourceMappers = documents.flatMap(_.sourceMapper).toVector
    val sourceMapper =
      if (originalSourceMappers.nonEmpty) Some(AggregateSourceMapper.merge(originalSourceMappers))
      else None

    Document(documents.toVector.flatMap(_.definitions), sourceMapper = sourceMapper)
  }

  /** The most basic, but valid document with a stub `Query` type
    */
  val emptyStub: Document =
    Document(
      Vector(
        ObjectTypeDefinition(
          "Query",
          Vector.empty,
          Vector(FieldDefinition("stub", NamedType("String"), Vector.empty)))))
}

case class InputDocument(
    values: Vector[Value],
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None,
    sourceMapper: Option[SourceMapper] = None)
    extends AstNode
    with WithTrailingComments {
  lazy val source: Option[String] = sourceMapper.map(_.source)

  /** Merges two documents. The `sourceMapper` is lost along the way.
    */
  def merge(other: InputDocument): InputDocument = InputDocument.merge(Vector(this, other))

  /** An alias for `merge`
    */
  def +(other: InputDocument): InputDocument = merge(other)

  def to[T](
      schema: Schema[_, _],
      inputType: InputType[T]
  )(implicit fromInput: FromInput[T], scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    InputDocumentMaterializer.to(schema, this, inputType)

  def to[T, Vars](
      schema: Schema[_, _],
      inputType: InputType[T],
      variables: Vars
  )(implicit
      iu: InputUnmarshaller[Vars],
      fromInput: FromInput[T],
      scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    InputDocumentMaterializer.to(schema, this, inputType, variables)

  def to[T](inputType: InputType[T])(implicit
      fromInput: FromInput[T],
      scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    InputDocumentMaterializer.to(this, inputType)

  def to[T, Vars](
      inputType: InputType[T],
      variables: Vars = InputUnmarshaller.emptyMapVars
  )(implicit
      iu: InputUnmarshaller[Vars],
      fromInput: FromInput[T],
      scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    InputDocumentMaterializer.to(this, inputType, variables)

  override def equals(other: Any): Boolean = other match {
    case that: InputDocument =>
      (that.canEqual(this)) &&
        values == that.values &&
        location == that.location
    case _ => false
  }

  override def hashCode(): Int =
    Seq(values, location).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}

object InputDocument {
  def merge(documents: Traversable[InputDocument]): InputDocument =
    InputDocument(documents.toVector.flatMap(_.values))
}

sealed trait ConditionalFragment extends AstNode {
  def typeConditionOpt: Option[NamedType]
}

sealed trait WithComments extends AstNode {
  def comments: Vector[Comment]
}

sealed trait WithDescription extends AstNode {
  def description: Option[StringValue]
}

sealed trait WithTrailingComments {
  def trailingComments: Vector[Comment]
}

sealed trait SelectionContainer extends AstNode with WithComments with WithTrailingComments {
  def selections: Vector[Selection]
  def location: Option[AstLocation]
}

/** A definition in a [[Document GraphQL document]].
  *
  * A GraphQL document consists primarily of definitions,
  * which are either executable or representative of a GraphQL type system.
  * The executable definitions are [[OperationDefinition operation]] and [[FragmentDefinition fragment definitions]];
  * those that represent a type system fall into [[TypeSystemDefinition definition]]
  * or [[TypeSystemExtensionDefinition extension]] categories.
  *
  * @see [[https://spec.graphql.org/June2018/#Definition]]
  */
sealed trait Definition extends AstNode

/** A definition of a GraphQL operation.
  *
  * Every GraphQL request invokes a specific operation,
  * possibly with values to substitute into the operation's variables.
  *
  * @param name
  *   The name of the operation. Optional only if there is only one operation in the [[Document document]].
  *   Used for selecting the specific operation to invoke in a GraphQL request.
  * @param variables
  *   The variables that must be substituted into the operation.
  *   Values for these must be provided either by their defaults or with the GraphQL request.
  *
  * @see [[https://spec.graphql.org/June2018/#OperationDefinition]]
  */
case class OperationDefinition(
    operationType: OperationType = OperationType.Query,
    name: Option[String] = None,
    variables: Vector[VariableDefinition] = Vector.empty,
    override val directives: Vector[Directive] = Vector.empty,
    override val selections: Vector[Selection],
    override val comments: Vector[Comment] = Vector.empty,
    override val trailingComments: Vector[Comment] = Vector.empty,
    override val location: Option[AstLocation] = None)
    extends Definition
    with WithDirectives
    with SelectionContainer

case class FragmentDefinition(
    name: String,
    typeCondition: NamedType,
    directives: Vector[Directive],
    selections: Vector[Selection],
    variables: Vector[VariableDefinition] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Definition
    with ConditionalFragment
    with WithDirectives
    with SelectionContainer {
  lazy val typeConditionOpt: Option[NamedType] = Some(typeCondition)
}

/** A definition of a variable to an [[OperationDefinition operation]].
  *
  * @param name Name of the variable being defined.
  * @param defaultValue
  *   Value that the variable should assume in an operation if none was provided with the GraphQL request.
  *
  * @see [[https://spec.graphql.org/June2018/#VariableDefinition]]
  */
case class VariableDefinition(
    name: String,
    tpe: Type,
    defaultValue: Option[Value],
    override val directives: Vector[Directive] = Vector.empty,
    override val comments: Vector[Comment] = Vector.empty,
    override val location: Option[AstLocation] = None)
    extends AstNode
    with WithComments
    with WithDirectives

sealed trait Type extends AstNode {
  def namedType: NamedType = {
    @annotation.tailrec
    def loop(tpe: Type): NamedType = tpe match {
      case NotNullType(ofType, _) => loop(ofType)
      case ListType(ofType, _) => loop(ofType)
      case named: NamedType => named
    }

    loop(this)
  }
}

case class NamedType(name: String, location: Option[AstLocation] = None) extends Type
case class NotNullType(ofType: Type, location: Option[AstLocation] = None) extends Type
case class ListType(ofType: Type, location: Option[AstLocation] = None) extends Type

sealed trait WithArguments extends AstNode {
  def arguments: Vector[Argument]
}

sealed trait Selection extends AstNode with WithDirectives with WithComments

case class Field(
    alias: Option[String],
    name: String,
    arguments: Vector[Argument],
    directives: Vector[Directive],
    selections: Vector[Selection],
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Selection
    with SelectionContainer
    with WithArguments {
  lazy val outputName: String = alias.getOrElse(name)
}

case class FragmentSpread(
    name: String,
    directives: Vector[Directive],
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Selection

case class InlineFragment(
    typeCondition: Option[NamedType],
    directives: Vector[Directive],
    selections: Vector[Selection],
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Selection
    with ConditionalFragment
    with SelectionContainer {
  def typeConditionOpt: Option[NamedType] = typeCondition
}

sealed trait NameValue extends AstNode with WithComments {
  def name: String
  def value: Value
}

sealed trait WithDirectives extends AstNode {
  def directives: Vector[Directive]
}

case class Directive(
    name: String,
    arguments: Vector[Argument],
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends AstNode
    with WithArguments
case class Argument(
    name: String,
    value: Value,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends NameValue

/** A value that can be substituted into a GraphQL operation [[VariableDefinition variable]].
  *
  * Called "input values" in the GraphQL spec.
  * Input values can be [[ScalarValue scalars]], [[EnumValue enumeration values]], [[ListValue lists]],
  * [[ObjectValue objects]], or [[NullValue null values]].
  *
  * @see [[https://spec.graphql.org/June2018/#Value]]
  */
sealed trait Value extends AstNode with WithComments {
  override def renderPretty: String = QueryRenderer.render(this, QueryRenderer.PrettyInput)
}

sealed trait ScalarValue extends Value

case class IntValue(
    value: Int,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ScalarValue
case class BigIntValue(
    value: BigInt,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ScalarValue
case class FloatValue(
    value: Double,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ScalarValue
case class BigDecimalValue(
    value: BigDecimal,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ScalarValue
case class StringValue(
    value: String,
    block: Boolean = false,
    blockRawValue: Option[String] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ScalarValue
case class BooleanValue(
    value: Boolean,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ScalarValue
case class EnumValue(
    value: String,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Value
case class ListValue(
    values: Vector[Value],
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Value
case class VariableValue(
    name: String,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Value
case class NullValue(comments: Vector[Comment] = Vector.empty, location: Option[AstLocation] = None)
    extends Value
case class ObjectValue(
    fields: Vector[ObjectField],
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends Value {
  lazy val fieldsByName: Map[String, Value] =
    fields.foldLeft(ListMap.empty[String, Value]) { case (acc, field) =>
      acc + (field.name -> field.value)
    }
}

object ObjectValue {
  def apply(fields: (String, Value)*): ObjectValue = ObjectValue(
    fields.toVector.map(f => ObjectField(f._1, f._2)))
}

case class ObjectField(
    name: String,
    value: Value,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends NameValue

case class Comment(text: String, location: Option[AstLocation] = None) extends AstNode

// Schema Definition

case class ScalarTypeDefinition(
    name: String,
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeDefinition
    with WithDescription {
  def rename(newName: String): ScalarTypeDefinition = copy(name = newName)
}

case class FieldDefinition(
    name: String,
    fieldType: Type,
    arguments: Vector[InputValueDefinition],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends SchemaAstNode
    with WithDirectives
    with WithDescription

case class InputValueDefinition(
    name: String,
    valueType: Type,
    defaultValue: Option[Value],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends SchemaAstNode
    with WithDirectives
    with WithDescription

case class ObjectTypeDefinition(
    name: String,
    interfaces: Vector[NamedType],
    fields: Vector[FieldDefinition],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeDefinition
    with WithTrailingComments
    with WithDescription {
  def rename(newName: String): ObjectTypeDefinition = copy(name = newName)
}

case class InterfaceTypeDefinition(
    name: String,
    fields: Vector[FieldDefinition],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeDefinition
    with WithTrailingComments
    with WithDescription {
  def rename(newName: String): InterfaceTypeDefinition = copy(name = newName)
}

case class UnionTypeDefinition(
    name: String,
    types: Vector[NamedType],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeDefinition
    with WithDescription {
  def rename(newName: String): UnionTypeDefinition = copy(name = newName)
}

case class EnumTypeDefinition(
    name: String,
    values: Vector[EnumValueDefinition],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeDefinition
    with WithTrailingComments
    with WithDescription {
  def rename(newName: String): EnumTypeDefinition = copy(name = newName)
}

case class EnumValueDefinition(
    name: String,
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends SchemaAstNode
    with WithDirectives
    with WithDescription

case class InputObjectTypeDefinition(
    name: String,
    fields: Vector[InputValueDefinition],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeDefinition
    with WithTrailingComments
    with WithDescription {
  def rename(newName: String): InputObjectTypeDefinition = copy(name = newName)
}

case class ObjectTypeExtensionDefinition(
    name: String,
    interfaces: Vector[NamedType],
    fields: Vector[FieldDefinition],
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ObjectLikeTypeExtensionDefinition
    with WithTrailingComments {
  def rename(newName: String): ObjectTypeExtensionDefinition = copy(name = newName)
}

case class InterfaceTypeExtensionDefinition(
    name: String,
    fields: Vector[FieldDefinition],
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends ObjectLikeTypeExtensionDefinition
    with WithTrailingComments {
  def rename(newName: String): InterfaceTypeExtensionDefinition = copy(name = newName)
}

case class InputObjectTypeExtensionDefinition(
    name: String,
    fields: Vector[InputValueDefinition],
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeExtensionDefinition
    with WithTrailingComments {
  def rename(newName: String): InputObjectTypeExtensionDefinition = copy(name = newName)
}

case class EnumTypeExtensionDefinition(
    name: String,
    values: Vector[EnumValueDefinition],
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeExtensionDefinition
    with WithTrailingComments {
  def rename(newName: String): EnumTypeExtensionDefinition = copy(name = newName)
}

case class UnionTypeExtensionDefinition(
    name: String,
    types: Vector[NamedType],
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeExtensionDefinition {
  def rename(newName: String): UnionTypeExtensionDefinition = copy(name = newName)
}

case class ScalarTypeExtensionDefinition(
    name: String,
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeExtensionDefinition {
  def rename(newName: String): ScalarTypeExtensionDefinition = copy(name = newName)
}

case class SchemaExtensionDefinition(
    operationTypes: Vector[OperationTypeDefinition],
    directives: Vector[Directive] = Vector.empty,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeSystemExtensionDefinition
    with WithDirectives
    with WithTrailingComments

case class DirectiveDefinition(
    name: String,
    arguments: Vector[InputValueDefinition],
    locations: Vector[DirectiveLocation],
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeSystemDefinition
    with WithDescription

case class DirectiveLocation(
    name: String,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends SchemaAstNode

case class SchemaDefinition(
    operationTypes: Vector[OperationTypeDefinition],
    directives: Vector[Directive] = Vector.empty,
    description: Option[StringValue] = None,
    comments: Vector[Comment] = Vector.empty,
    trailingComments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends TypeSystemDefinition
    with WithDescription
    with WithTrailingComments
    with WithDirectives

case class OperationTypeDefinition(
    operation: OperationType,
    tpe: NamedType,
    comments: Vector[Comment] = Vector.empty,
    location: Option[AstLocation] = None)
    extends SchemaAstNode

/** A node in the AST of a parsed GraphQL request document. */
sealed trait AstNode {
  /** Location at which this node lexically begins in the GraphQL request source code. */
  def location: Option[AstLocation]
  def cacheKeyHash: Int = System.identityHashCode(this)

  def renderPretty: String = QueryRenderer.render(this, QueryRenderer.Pretty)
  def renderCompact: String = QueryRenderer.render(this, QueryRenderer.Compact)

  def visit(visitor: AstVisitor): this.type =
    AstVisitor.visit(this, visitor)

  def visit(onEnter: AstNode => VisitorCommand, onLeave: AstNode => VisitorCommand): this.type =
    AstVisitor.visit(this, onEnter, onLeave)

  def visitAstWithTypeInfo(schema: Schema[_, _])(visitorFn: TypeInfo => AstVisitor): this.type =
    AstVisitor.visitAstWithTypeInfo[this.type](schema, this)(visitorFn)

  def visitAstWithState[S](schema: Schema[_, _], state: S)(
      visitorFn: (TypeInfo, S) => AstVisitor): S =
    AstVisitor.visitAstWithState(schema, this, state)(visitorFn)
}

sealed trait SchemaAstNode extends AstNode with WithComments
sealed trait TypeSystemDefinition extends SchemaAstNode with Definition
sealed trait TypeSystemExtensionDefinition extends SchemaAstNode with Definition

sealed trait TypeDefinition extends TypeSystemDefinition with WithDirectives with WithDescription {
  def name: String
  def rename(newName: String): TypeDefinition
}

sealed trait TypeExtensionDefinition extends TypeSystemExtensionDefinition with WithDirectives {
  def name: String
  def rename(newName: String): TypeExtensionDefinition
}

sealed trait ObjectLikeTypeExtensionDefinition extends TypeExtensionDefinition {
  def fields: Vector[FieldDefinition]
}

object AstNode {
  def withoutAstLocations[T <: AstNode](node: T, stripComments: Boolean = false): T = {
    val enterComment = (_: Comment) =>
      if (stripComments) VisitorCommand.Delete else VisitorCommand.Continue

    visit[AstNode](
      node,
      Visit[Comment](enterComment),
      VisitAnyField[AstNode, Option[AstLocation]]((_, _) => VisitorCommand.Transform(None)))
      .asInstanceOf[T]
  }
}

trait AstVisitor {
  def onEnter: PartialFunction[AstNode, VisitorCommand] = { case _ => VisitorCommand.Continue }
  def onLeave: PartialFunction[AstNode, VisitorCommand] = { case _ => VisitorCommand.Continue }
}

object AstVisitor {
  import AstVisitorCommand._

  def apply(
      onEnter: PartialFunction[AstNode, VisitorCommand] = { case _ => VisitorCommand.Continue },
      onLeave: PartialFunction[AstNode, VisitorCommand] = { case _ => VisitorCommand.Continue }
  ): DefaultAstVisitor = DefaultAstVisitor(onEnter, onLeave)

  def simple(
      onEnter: PartialFunction[AstNode, Unit] = { case _ => () },
      onLeave: PartialFunction[AstNode, Unit] = { case _ => () }
  ): DefaultAstVisitor = DefaultAstVisitor(
    {
      case node if onEnter.isDefinedAt(node) =>
        onEnter(node)
        VisitorCommand.Continue
    },
    {
      case node if onLeave.isDefinedAt(node) =>
        onLeave(node)
        VisitorCommand.Continue
    }
  )

  def visit[T <: AstNode](root: T, visitor: AstVisitor): T =
    visit(
      root,
      node =>
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue,
      node =>
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
    )

  def visitAstWithTypeInfo[T <: AstNode](schema: Schema[_, _], root: T)(
      visitorFn: TypeInfo => AstVisitor): T = {
    val typeInfo = new TypeInfo(schema)
    val visitor = visitorFn(typeInfo)

    visit(
      root,
      node => {
        typeInfo.enter(node)
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue
      },
      node => {
        typeInfo.leave(node)
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
      }
    )
  }

  def visitAstWithState[S](schema: Schema[_, _], root: AstNode, state: S)(
      visitorFn: (TypeInfo, S) => AstVisitor): S = {
    val typeInfo = new TypeInfo(schema)
    val visitor = visitorFn(typeInfo, state)

    visit(
      root,
      node => {
        typeInfo.enter(node)
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue
      },
      node => {
        typeInfo.leave(node)
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
      }
    )

    state
  }

  def visit[T <: AstNode](
      root: T,
      onEnter: AstNode => VisitorCommand,
      onLeave: AstNode => VisitorCommand): T =
    sangria.visitor.visit[AstNode](root, Visit[AstNode](onEnter, onLeave)).asInstanceOf[T]

  private[sangria] def visitAstRecursive(
      doc: AstNode,
      onEnter: AstNode => AstVisitorCommand.Value = _ => Continue,
      onLeave: AstNode => AstVisitorCommand.Value = _ => Continue): Unit = {

    def breakOrSkip(cmd: AstVisitorCommand.Value) = cmd match {
      case Break => break()
      case Skip => false
      case Continue => true
    }

    def loop(node: AstNode): Unit =
      node match {
        case n @ Document(defs, trailingComments, _, _) =>
          if (breakOrSkip(onEnter(n))) {
            defs.foreach(d => loop(d))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputDocument(defs, trailingComments, _, _) =>
          if (breakOrSkip(onEnter(n))) {
            defs.foreach(d => loop(d))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentDefinition(_, cond, dirs, sels, vars, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(cond)
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            vars.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationDefinition(_, _, vars, dirs, sels, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            vars.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ VariableDefinition(_, tpe, default, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            default.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InlineFragment(cond, dirs, sels, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            cond.foreach(c => loop(c))
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentSpread(_, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NotNullType(ofType, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(ofType)
            breakOrSkip(onLeave(n))
          }
        case n @ ListType(ofType, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(ofType)
            breakOrSkip(onLeave(n))
          }
        case n @ Field(_, _, args, dirs, sels, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Argument(_, v, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectField(_, v, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Directive(_, args, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ListValue(vals, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            vals.foreach(v => loop(v))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectValue(fields, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(f => loop(f))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BigDecimalValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BooleanValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Comment(_, _) =>
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
        case n @ VariableValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NullValue(comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NamedType(_, _) =>
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
        case n @ StringValue(_, _, _, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BigIntValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ IntValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FloatValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }

        // SDL schema definition

        case n @ ScalarTypeDefinition(_, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FieldDefinition(_, fieldType, args, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(fieldType)
            args.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputValueDefinition(_, valueType, default, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(valueType)
            default.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectTypeDefinition(
              _,
              interfaces,
              fields,
              dirs,
              description,
              comment,
              trailingComments,
              _) =>
          if (breakOrSkip(onEnter(n))) {
            interfaces.foreach(d => loop(d))
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InterfaceTypeDefinition(
              _,
              fields,
              dirs,
              description,
              comment,
              trailingComments,
              _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ UnionTypeDefinition(_, types, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            types.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumTypeDefinition(_, values, dirs, description, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            values.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumValueDefinition(_, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputObjectTypeDefinition(
              _,
              fields,
              dirs,
              description,
              comment,
              trailingComments,
              _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectTypeExtensionDefinition(_, ints, fields, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            ints.foreach(d => loop(d))
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InterfaceTypeExtensionDefinition(_, fields, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputObjectTypeExtensionDefinition(_, fields, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ UnionTypeExtensionDefinition(_, types, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            types.foreach(t => loop(t))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumTypeExtensionDefinition(_, values, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            values.foreach(t => loop(t))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ScalarTypeExtensionDefinition(_, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ SchemaExtensionDefinition(ops, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            ops.foreach(op => loop(op))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(c => loop(c))
            breakOrSkip(onLeave(n))
          }
        case n @ DirectiveDefinition(_, args, locations, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d => loop(d))
            locations.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ DirectiveLocation(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ SchemaDefinition(ops, dirs, descr, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            ops.foreach(s => loop(s))
            dirs.foreach(s => loop(s))
            descr.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationTypeDefinition(_, tpe, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
      }

    breakable {
      loop(doc)
    }
  }
}
