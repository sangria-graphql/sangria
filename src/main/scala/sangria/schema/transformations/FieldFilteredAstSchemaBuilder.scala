package sangria.schema.transformations

import sangria.ast
import sangria.ast.InterfaceTypeExtensionDefinition
import sangria.schema.{AstSchemaMaterializer, DefaultAstSchemaBuilder, Field, InterfaceType, MatOrigin, ObjectType}


class FieldFilteredAstSchemaBuilder[Ctx](filter: List[Field[Ctx, Any]] => List[Field[Ctx, Any]]) extends DefaultAstSchemaBuilder[Ctx] {

  override def extendObjectType(
                        origin: MatOrigin,
                        existing: ObjectType[Ctx, _],
                        extensions: List[ast.ObjectTypeExtensionDefinition],
                        fields: () ⇒ List[Field[Ctx, Any]],
                        interfaces: List[InterfaceType[Ctx, Any]],
                        mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any] = {
    super.extendObjectType(origin, existing, extensions, applyFilter(fields), interfaces, mat)
  }

  override def extendInterfaceType(origin: MatOrigin,
                                   existing: InterfaceType[Ctx, _],
                                   extensions: List[InterfaceTypeExtensionDefinition],
                                   fields: () => List[Field[Ctx, Any]],
                                   mat: AstSchemaMaterializer[Ctx]): InterfaceType[Ctx, Any] = {
    super.extendInterfaceType(origin, existing, extensions, applyFilter(fields), mat)
  }

  def applyFilter(fieldsFn: () => List[Field[Ctx, Any]]): () => List[Field[Ctx, Any]] = {
    () => filter(fieldsFn())
  }
}

