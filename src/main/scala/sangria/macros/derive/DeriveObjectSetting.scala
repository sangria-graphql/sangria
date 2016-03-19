package sangria.macros.derive

import sangria.execution.FieldTag
import sangria.schema.{Args, PossibleInterface, Field}

sealed trait DeriveObjectSetting[Ctx, Val]

case class ObjectTypeName[Ctx, Val](name: String) extends DeriveObjectSetting[Ctx, Val]
case class ObjectTypeDescription[Ctx, Val](description: String) extends DeriveObjectSetting[Ctx, Val]
case class Interfaces[Ctx, Val](interfaces: PossibleInterface[Ctx, Val]*) extends DeriveObjectSetting[Ctx, Val]

case class DocumentField[Ctx, Val](fieldName: String, description: String, deprecationReason: Option[String] = None) extends DeriveObjectSetting[Ctx, Val]
case class DeprecateField[Ctx, Val](fieldName: String, deprecationReason: String) extends DeriveObjectSetting[Ctx, Val]
case class RenameField[Ctx, Val](fieldName: String, graphqlName: String) extends DeriveObjectSetting[Ctx, Val]
case class FieldTags[Ctx, Val](fieldName: String, tags: FieldTag*) extends DeriveObjectSetting[Ctx, Val]
case class FieldComplexity[Ctx, Val](fieldName: String, complexity: (Ctx, Args, Double) ⇒ Double) extends DeriveObjectSetting[Ctx, Val]

case class IncludeFields[Ctx, Val](fieldNames: String*) extends DeriveObjectSetting[Ctx, Val]
case class IncludeMethods[Ctx, Val](methodNames: String*) extends DeriveObjectSetting[Ctx, Val]
case class ExcludeFields[Ctx, Val](fieldNames: String*) extends DeriveObjectSetting[Ctx, Val]
case class ReplaceField[Ctx, Val](fieldName: String, field: Field[Ctx, Val]) extends DeriveObjectSetting[Ctx, Val]
case class AddFields[Ctx, Val](fields: Field[Ctx, Val]*) extends DeriveObjectSetting[Ctx, Val]
