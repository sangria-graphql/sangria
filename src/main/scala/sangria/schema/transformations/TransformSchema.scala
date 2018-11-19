package sangria.schema.transformations

import sangria.ast.{Document, ObjectTypeExtensionDefinition}
import sangria.schema.{AstSchemaMaterializer, Field, Schema}

object TransformSchema {
  type FieldFilter[Ctx] = List[Field[Ctx, Any]] => List[Field[Ctx, Any]]

  def filterFields[Ctx, Val](schema: Schema[Ctx, Val], filter: FieldFilter[Ctx]): Schema[Ctx, Val] = {
    AstSchemaMaterializer.extendSchema(
      schema,
      emptyExtensionDocument(schema.query.name),
      new FieldFilteredAstSchemaBuilder[Ctx](filter)
    )
  }

  private def emptyExtensionDocument(queryName: String): Document = {
    Document(Vector(
      ObjectTypeExtensionDefinition(queryName, Vector.empty, Vector.empty)
    ))
  }
}
