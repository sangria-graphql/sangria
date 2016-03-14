package sangria.macros.derive

sealed trait DeriveEnumTypeConfig

case class EnumTypeName(name: String) extends DeriveEnumTypeConfig
case class EnumTypeDescription(description: String) extends DeriveEnumTypeConfig

case object UppercaseValues extends DeriveEnumTypeConfig

case class DocumentValue(value: String, description: String, deprecationReason: Option[String] = None) extends DeriveEnumTypeConfig
case class DeprecateValue(value: String, deprecationReason: String) extends DeriveEnumTypeConfig
case class RenameValue(value: String, graphqlName: String) extends DeriveEnumTypeConfig

case class IncludeValues(values: String*) extends DeriveEnumTypeConfig
case class ExcludeValues(fieldNames: String*) extends DeriveEnumTypeConfig
