package sangria.federation

import sangria.ast._

object Query {

  val _entities =
    FieldDefinition(
      name = "_entities",
      fieldType = NotNullType(ListType(NamedType("_Entity"))),
      arguments = Vector(
        InputValueDefinition(
          name = "representations",
          valueType = NotNullType(ListType(NotNullType(NamedType("_Any")))),
          defaultValue = None)))

  def queryType(fields: FieldDefinition*) =
    ObjectTypeExtensionDefinition(
      name = "Query",
      interfaces = Vector.empty,
      fields = fields.toVector)
}
