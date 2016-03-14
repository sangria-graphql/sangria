package sangria.macros

import sangria.execution.FieldTag
import sangria.schema.{PossibleInterface, Field}

sealed trait DeriveObjectTypeConfig[Ctx, Val]

case class ObjectTypeName[Ctx, Val](name: String) extends DeriveObjectTypeConfig[Ctx, Val]
case class ObjectTypeDescription[Ctx, Val](description: String) extends DeriveObjectTypeConfig[Ctx, Val]
case class Interfaces[Ctx, Val](interfaces: PossibleInterface[Ctx, Val]*) extends DeriveObjectTypeConfig[Ctx, Val]

case class DocumentField[Ctx, Val](fieldName: String, description: String, deprecationReason: Option[String] = None) extends DeriveObjectTypeConfig[Ctx, Val]
case class DeprecateField[Ctx, Val](fieldName: String, deprecationReason: String) extends DeriveObjectTypeConfig[Ctx, Val]
case class RenameField[Ctx, Val](fieldName: String, graphqlName: String) extends DeriveObjectTypeConfig[Ctx, Val]
case class FieldTags[Ctx, Val](fieldName: String, tags: FieldTag*) extends DeriveObjectTypeConfig[Ctx, Val]

case class IncludeFields[Ctx, Val](fieldNames: String*) extends DeriveObjectTypeConfig[Ctx, Val]
case class ExcludeFields[Ctx, Val](fieldNames: String*) extends DeriveObjectTypeConfig[Ctx, Val]
case class OverrideField[Ctx, Val](fieldName: String, field: Field[Ctx, Val]) extends DeriveObjectTypeConfig[Ctx, Val]
case class AddFields[Ctx, Val](fields: Field[Ctx, Val]*) extends DeriveObjectTypeConfig[Ctx, Val]
