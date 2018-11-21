package sangria.schema.extension

import sangria.ast
import sangria.schema.{Argument, AstSchemaMaterializer, Directive, Field, InputField, InputObjectType, InputType, InterfaceType, MatOrigin, Named, ObjectLikeType, ObjectType, OutputType, ScalarAlias, ScalarType, Schema, Type, UnionType}

trait AstSchemaExtender[Ctx] {

  def extendArgument(
                      origin: MatOrigin,
                      typeDefinition: Option[ObjectLikeType[Ctx, _]],
                      field: Field[Ctx, Any],
                      argument: Argument[Any],
                      argumentType: InputType[_],
                      mat: AstSchemaMaterializer[Ctx]): Argument[Any]

  def extendArgumentType(
                          origin: MatOrigin,
                          typeDefinition: Option[ObjectLikeType[Ctx, _]],
                          field: Field[Ctx, Any],
                          existing: Argument[Any],
                          mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def extendField(
                   origin: MatOrigin,
                   typeDefinition: Option[ObjectLikeType[Ctx, _]],
                   existing: Field[Ctx, Any],
                   fieldType: OutputType[_],
                   arguments: List[Argument[_]],
                   mat: AstSchemaMaterializer[Ctx]): Field[Ctx, Any]

  def extendFieldType(
                       origin: MatOrigin,
                       typeDefinition: Option[ObjectLikeType[Ctx, _]],
                       existing: Field[Ctx, Any],
                       mat: AstSchemaMaterializer[Ctx]): OutputType[Any]

  def extendInputField(
                        origin: MatOrigin,
                        typeDefinition: InputObjectType[_],
                        existing: InputField[Any],
                        fieldType: InputType[_],
                        mat: AstSchemaMaterializer[Ctx]): InputField[Any]

  def extendInputFieldType(
                            origin: MatOrigin,
                            typeDefinition: InputObjectType[_],
                            existing: InputField[Any],
                            mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def extendInterfaceType(
                           origin: MatOrigin,
                           existing: InterfaceType[Ctx, _],
                           extensions: List[ast.InterfaceTypeExtensionDefinition],
                           fields: () ⇒ List[Field[Ctx, Any]],
                           mat: AstSchemaMaterializer[Ctx]): InterfaceType[Ctx, Any]

  def extendObjectType(
                        origin: MatOrigin,
                        existing: ObjectType[Ctx, _],
                        extensions: List[ast.ObjectTypeExtensionDefinition],
                        fields: () ⇒ List[Field[Ctx, Any]],
                        interfaces: List[InterfaceType[Ctx, Any]],
                        mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any]

  def extendScalarAlias[T, ST](
                                origin: MatOrigin,
                                extensions: Vector[ast.ScalarTypeExtensionDefinition],
                                existing: ScalarAlias[T, ST],
                                aliasFor: ScalarType[ST],
                                mat: AstSchemaMaterializer[Ctx]): ScalarAlias[T, ST]

  def extendSchema[Val](
                         originalSchema: Schema[Ctx, Val],
                         extensions: List[ast.SchemaExtensionDefinition],
                         queryType: ObjectType[Ctx, Val],
                         mutationType: Option[ObjectType[Ctx, Val]],
                         subscriptionType: Option[ObjectType[Ctx, Val]],
                         additionalTypes: List[Type with Named],
                         directives: List[Directive],
                         mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Val]

  def extendUnionType(
                       origin: MatOrigin,
                       extensions: Vector[ast.UnionTypeExtensionDefinition],
                       existing: UnionType[Ctx],
                       types: List[ObjectType[Ctx, _]],
                       mat: AstSchemaMaterializer[Ctx]): UnionType[Ctx]
}
