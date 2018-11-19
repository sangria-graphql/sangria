package sangria.schema.transformations

import sangria.ast
import sangria.schema.transformations.TransformSchema.FieldFilter
import sangria.schema.{AstSchemaMaterializer, DefaultAstSchemaBuilder, Field, InterfaceType, MatOrigin, ObjectType}


class FieldFilteredAstSchemaBuilder[Ctx](filter: FieldFilter[Ctx]) extends DefaultAstSchemaBuilder[Ctx] {
  override def extendObjectType(
                        origin: MatOrigin,
                        existing: ObjectType[Ctx, _],
                        extensions: List[ast.ObjectTypeExtensionDefinition],
                        fields: () ⇒ List[Field[Ctx, Any]],
                        interfaces: List[InterfaceType[Ctx, Any]],
                        mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any] = {
    super.extendObjectType(
      origin = origin,
      existing = existing,
      extensions = extensions,
      fields = () => filter(fields()),
      interfaces = interfaces,
      mat = mat
    )
  }
}

