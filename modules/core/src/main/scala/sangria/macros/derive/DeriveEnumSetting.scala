package sangria.macros.derive

sealed trait DeriveEnumSetting

case class EnumTypeName(name: String) extends DeriveEnumSetting
case class EnumTypeDescription(description: String) extends DeriveEnumSetting

@deprecated("Use more generic `TransformValueNames` instead. Equivalent: `TransformValueNames(StringUtil.camelCaseToUnderscore(_).toUpperCase)`", "1.4.1")
case object UppercaseValues extends DeriveEnumSetting

case class DocumentValue(value: String, description: String, deprecationReason: Option[String] = None) extends DeriveEnumSetting
case class DeprecateValue(value: String, deprecationReason: String) extends DeriveEnumSetting
case class RenameValue(value: String, graphqlName: String) extends DeriveEnumSetting

case class IncludeValues(values: String*) extends DeriveEnumSetting
case class ExcludeValues(fieldNames: String*) extends DeriveEnumSetting

case class TransformValueNames(transformer: String => String) extends DeriveEnumSetting
