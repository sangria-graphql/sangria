package sangria.introspection

import sangria.ast.Document
import sangria.introspection
import sangria.renderer.{QueryRenderer, SchemaFilter, SchemaRenderer}
import sangria.schema.DirectiveLocation

case class IntrospectionSchema(
    queryType: IntrospectionNamedTypeRef,
    mutationType: Option[IntrospectionNamedTypeRef],
    subscriptionType: Option[IntrospectionNamedTypeRef],
    types: Seq[IntrospectionType],
    directives: Seq[IntrospectionDirective],
    description: Option[String]
) {
  def toAst: Document = SchemaRenderer.schemaAstFromIntrospection(this)
  def toAst(filter: SchemaFilter): Document =
    SchemaRenderer.schemaAstFromIntrospection(this, filter)

  def renderPretty: String = QueryRenderer.renderPretty(toAst)
  def renderPretty(filter: SchemaFilter): String = QueryRenderer.renderPretty(toAst(filter))

  def renderCompact: String = QueryRenderer.renderCompact(toAst)
  def renderCompact(filter: SchemaFilter): String = QueryRenderer.renderCompact(toAst(filter))

  lazy val typesByName: Map[String, IntrospectionType] =
    types.groupBy(_.name).map { case (k, v) => (k, v.head) }
}

sealed trait IntrospectionType {
  def kind: TypeKind.Value
  def name: String
  def description: Option[String]
}

case class IntrospectionScalarType(name: String, description: Option[String])
    extends IntrospectionType {
  val kind: introspection.TypeKind.Value = TypeKind.Scalar
}

case class IntrospectionObjectType(
    name: String,
    description: Option[String],
    fields: Seq[IntrospectionField],
    interfaces: Seq[IntrospectionNamedTypeRef])
    extends IntrospectionType {
  val kind = TypeKind.Object
  lazy val fieldsByName = fields.groupBy(_.name).map { case (k, v) => (k, v.head) }
}

case class IntrospectionInputObjectType(
    name: String,
    description: Option[String],
    inputFields: Seq[IntrospectionInputValue])
    extends IntrospectionType {
  val kind = TypeKind.InputObject
  lazy val inputFieldsByName = inputFields.groupBy(_.name).map { case (k, v) => (k, v.head) }
}

case class IntrospectionInterfaceType(
    name: String,
    description: Option[String],
    fields: Seq[IntrospectionField],
    interfaces: Seq[IntrospectionNamedTypeRef],
    possibleTypes: Seq[IntrospectionNamedTypeRef]
) extends IntrospectionType {

  val kind = TypeKind.Interface

}

object IntrospectionInterfaceType {
  def apply(
      name: String,
      description: Option[String],
      fields: Seq[IntrospectionField],
      possibleTypes: Seq[IntrospectionNamedTypeRef]): IntrospectionInterfaceType =
    IntrospectionInterfaceType(name, description, fields, Seq.empty, possibleTypes)
}

case class IntrospectionUnionType(
    name: String,
    description: Option[String],
    possibleTypes: Seq[IntrospectionNamedTypeRef])
    extends IntrospectionType {
  val kind = TypeKind.Union
}

case class IntrospectionEnumType(
    name: String,
    description: Option[String],
    enumValues: Seq[IntrospectionEnumValue])
    extends IntrospectionType {
  val kind = TypeKind.Enum
}

case class IntrospectionField(
    name: String,
    description: Option[String],
    args: Seq[IntrospectionInputValue],
    tpe: IntrospectionTypeRef,
    isDeprecated: Boolean,
    deprecationReason: Option[String]
) {
  lazy val argsByName = args.groupBy(_.name).map { case (k, v) => (k, v.head) }
}

case class IntrospectionEnumValue(
    name: String,
    description: Option[String],
    isDeprecated: Boolean,
    deprecationReason: Option[String])

case class IntrospectionInputValue(
    name: String,
    description: Option[String],
    tpe: IntrospectionTypeRef,
    defaultValue: Option[String],
    isDeprecated: Option[Boolean],
    deprecationReason: Option[String]
)

sealed trait IntrospectionTypeRef {
  def kind: TypeKind.Value
}

case class IntrospectionNamedTypeRef(kind: TypeKind.Value, name: String)
    extends IntrospectionTypeRef

case class IntrospectionListTypeRef(ofType: IntrospectionTypeRef) extends IntrospectionTypeRef {
  val kind = TypeKind.List
}

case class IntrospectionNonNullTypeRef(ofType: IntrospectionTypeRef) extends IntrospectionTypeRef {
  val kind = TypeKind.NonNull
}

case class IntrospectionDirective(
    name: String,
    description: Option[String],
    locations: Set[DirectiveLocation.Value],
    args: Seq[IntrospectionInputValue],
    repeatable: Boolean)
