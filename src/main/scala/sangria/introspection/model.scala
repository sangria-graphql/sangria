package sangria.introspection

case class IntrospectionSchema(
  queryType: IntrospectionNamedTypeRef,
  mutationType: Option[IntrospectionNamedTypeRef],
  subscriptionType: Option[IntrospectionNamedTypeRef],
  types: Seq[IntrospectionType],
  directives: Seq[IntrospectionDirective])

sealed trait IntrospectionType {
  def kind: String
  def name: String
  def description: Option[String]
}

case class IntrospectionScalarType(
    name: String,
    description: Option[String]) extends IntrospectionType {
  val kind = "SCALAR"
}

case class IntrospectionObjectType(
    name: String,
    description: Option[String],
    fields: Seq[IntrospectionField],
    interfaces: Seq[IntrospectionNamedTypeRef]) extends IntrospectionType {
  val kind = "OBJECT"
}

case class IntrospectionInputObjectType(
    name: String,
    description: Option[String],
    inputFields: Seq[IntrospectionInputValue]) extends IntrospectionType {
  val kind = "INPUT_OBJECT"
}

case class IntrospectionInterfaceType(
    name: String,
    description: Option[String],
    fields: Seq[IntrospectionField],
    possibleTypes: Seq[IntrospectionNamedTypeRef]) extends IntrospectionType {
  val kind = "INTERFACE"
}

case class IntrospectionUnionType(
    name: String,
    description: Option[String],
    possibleTypes: Seq[IntrospectionNamedTypeRef]) extends IntrospectionType {
  val kind = "UNION"
}

case class IntrospectionEnumType(
    name: String,
    description: Option[String],
    enumValues: Seq[IntrospectionEnumValue]) extends IntrospectionType {
  val kind = "ENUM"
}

case class IntrospectionField(
  name: String,
  description: Option[String],
  args: Seq[IntrospectionInputValue],
  tpe: IntrospectionTypeRef,
  isDeprecated: Boolean,
  deprecationReason: Option[String])

case class IntrospectionEnumValue(
  name: String,
  description: Option[String],
  isDeprecated: Boolean,
  deprecationReason: Option[String])

case class IntrospectionInputValue(
  name: String,
  description: Option[String],
  tpe: IntrospectionTypeRef,
  defaultValue: Option[String])

sealed trait IntrospectionTypeRef {
  def kind: String
}

case class IntrospectionNamedTypeRef(kind: String, name: String) extends IntrospectionTypeRef

case class IntrospectionListTypeRef(ofType: IntrospectionTypeRef) extends IntrospectionTypeRef {
  val kind = "LIST"
}

case class IntrospectionNonNullTypeRef(ofType: IntrospectionTypeRef) extends IntrospectionTypeRef {
  val kind = "NON_NULL"
}

case class IntrospectionDirective(
  name: String,
  description: Option[String],
  args: Seq[IntrospectionInputValue],
  onOperation: Boolean,
  onFragment: Boolean,
  onField: Boolean)
