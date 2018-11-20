package sangria.schema.transformations

import sangria.ast.{Document, ObjectTypeExtensionDefinition}
import sangria.schema.{AstSchemaMaterializer, Field, Schema}

object TransformSchema {

  def filterFields[Ctx, Val](schema: Schema[Ctx, Val], filter: List[Field[Ctx, Any]] => List[Field[Ctx, Any]]): Schema[Ctx, Val] = {
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
