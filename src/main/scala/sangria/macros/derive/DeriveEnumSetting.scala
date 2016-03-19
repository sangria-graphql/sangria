package sangria.macros.derive

sealed trait DeriveEnumSetting

case class EnumTypeName(name: String) extends DeriveEnumSetting
case class EnumTypeDescription(description: String) extends DeriveEnumSetting

case object UppercaseValues extends DeriveEnumSetting

case class DocumentValue(value: String, description: String, deprecationReason: Option[String] = None) extends DeriveEnumSetting
case class DeprecateValue(value: String, deprecationReason: String) extends DeriveEnumSetting
case class RenameValue(value: String, graphqlName: String) extends DeriveEnumSetting

case class IncludeValues(values: String*) extends DeriveEnumSetting
case class ExcludeValues(fieldNames: String*) extends DeriveEnumSetting
