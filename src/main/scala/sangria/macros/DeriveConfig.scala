package sangria.macros

import sangria.execution.FieldTag
import sangria.schema.{PossibleInterface, Field}

sealed trait DeriveConfig[Ctx, Val]

case class Name[Ctx, Val](name: String) extends DeriveConfig[Ctx, Val]
case class Description[Ctx, Val](description: String) extends DeriveConfig[Ctx, Val]
case class Interfaces[Ctx, Val](interfaces: PossibleInterface[Ctx, Val]*) extends DeriveConfig[Ctx, Val]

case class DocumentField[Ctx, Val](fieldName: String, description: String, deprecationReason: Option[String] = None) extends DeriveConfig[Ctx, Val]
case class DeprecateField[Ctx, Val](fieldName: String, deprecationReason: String) extends DeriveConfig[Ctx, Val]
case class RenameField[Ctx, Val](fieldName: String, graphqlName: String) extends DeriveConfig[Ctx, Val]
case class FieldTags[Ctx, Val](fieldName: String, tags: FieldTag*) extends DeriveConfig[Ctx, Val]

case class IncludeFields[Ctx, Val](fieldNames: String*) extends DeriveConfig[Ctx, Val]
case class ExcludeFields[Ctx, Val](fieldNames: String*) extends DeriveConfig[Ctx, Val]
case class AddFields[Ctx, Val](field: Field[Ctx, Val]*) extends DeriveConfig[Ctx, Val]
