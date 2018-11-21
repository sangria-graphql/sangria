package sangria.schema.extension

import sangria.ast
import sangria.schema.{Action, Argument, AstSchemaMaterializer, Context, Directive, Field, InputField, InputObjectType, InputType, InterfaceType, MatOrigin, Named, ObjectLikeType, ObjectType, OutputType, ScalarAlias, ScalarType, Schema, Type, UnionType}

import scala.reflect.ClassTag

class DefaultAstSchemaExtender[Ctx] extends AstSchemaExtender[Ctx] {

  override def extendArgument(
                               origin: MatOrigin,
                               typeDefinition: Option[ObjectLikeType[Ctx, _]],
                               field: Field[Ctx, Any],
                               argument: Argument[Any],
                               argumentType: InputType[_],
                               mat: AstSchemaMaterializer[Ctx]): Argument[Any] =
    argument.copy(argumentType = argumentType)


  override def extendArgumentType(
                                   origin: MatOrigin,
                                   typeDefinition: Option[ObjectLikeType[Ctx, _]],
                                   field: Field[Ctx, Any],
                                   existing: Argument[Any],
                                   mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputTypeFromExistingType(origin, existing.argumentType)

  override def extendField(
                            origin: MatOrigin,
                            typeDefinition: Option[ObjectLikeType[Ctx, _]],
                            existing: Field[Ctx, Any],
                            fieldType: OutputType[_],
                            arguments: List[Argument[_]],
                            mat: AstSchemaMaterializer[Ctx]): Field[Ctx, Any] =
    existing.copy(
      fieldType = fieldType,
      arguments = arguments,
      resolve = extendFieldResolver(origin, typeDefinition, existing, fieldType, mat),
      manualPossibleTypes = () ⇒ Nil)

  def extendFieldResolver(
                           origin: MatOrigin,
                           typeDefinition: Option[ObjectLikeType[Ctx, _]],
                           existing: Field[Ctx, Any],
                           fieldType: OutputType[_],
                           mat: AstSchemaMaterializer[Ctx]): Context[Ctx, Any] ⇒ Action[Ctx, _] = existing.resolve

  override def extendFieldType(
                                origin: MatOrigin,
                                typeDefinition: Option[ObjectLikeType[Ctx, _]],
                                existing: Field[Ctx, Any],
                                mat: AstSchemaMaterializer[Ctx]): OutputType[Any] =
    mat.getTypeFromExistingType(origin, existing.fieldType)

  override def extendInputField(
                                 origin: MatOrigin,
                                 typeDefinition: InputObjectType[_],
                                 existing: InputField[Any],
                                 fieldType: InputType[_],
                                 mat: AstSchemaMaterializer[Ctx]): InputField[Any] =
    existing.copy(fieldType = fieldType)

  override def extendInputFieldType(
                                     origin: MatOrigin,
                                     typeDefinition: InputObjectType[_],
                                     existing: InputField[Any],
                                     mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputTypeFromExistingType(origin, existing.fieldType)

  override def extendInterfaceType(
                                    origin: MatOrigin,
                                    existing: InterfaceType[Ctx, _],
                                    extensions: List[ast.InterfaceTypeExtensionDefinition],
                                    fields: () ⇒ List[Field[Ctx, Any]],
                                    mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(fieldsFn = fields, manualPossibleTypes = () ⇒ Nil, interfaces = Nil,
      astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
      astNodes = existing.astNodes ++ extensions)

  override def extendObjectType(
                                 origin: MatOrigin,
                                 existing: ObjectType[Ctx, _],
                                 extensions: List[ast.ObjectTypeExtensionDefinition],
                                 fields: () ⇒ List[Field[Ctx, Any]],
                                 interfaces: List[InterfaceType[Ctx, Any]],
                                 mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any] =
    extendedObjectTypeInstanceCheck(origin, existing, extensions) match {
      case Some(fn) ⇒
        existing.copy(
          fieldsFn = fields,
          interfaces = interfaces,
          astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
          astNodes = existing.astNodes ++ extensions,
          instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) ⇒ fn(value, clazz))(ClassTag(existing.valClass))
      case None ⇒
        existing.copy(
          fieldsFn = fields,
          interfaces = interfaces,
          astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
          astNodes = existing.astNodes ++ extensions,
          instanceCheck = existing.instanceCheck.asInstanceOf[(Any, Class[_], ObjectType[Ctx, _]) ⇒ Boolean])(ClassTag(existing.valClass))
    }

  def extendedObjectTypeInstanceCheck(origin: MatOrigin, tpe: ObjectType[Ctx, _], extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) ⇒ Boolean] =
    None

  override def extendScalarAlias[T, ST](
                                         origin: MatOrigin,
                                         extensions: Vector[ast.ScalarTypeExtensionDefinition],
                                         existing: ScalarAlias[T, ST],
                                         aliasFor: ScalarType[ST],
                                         mat: AstSchemaMaterializer[Ctx]): ScalarAlias[T, ST] =
    existing.copy(aliasFor = aliasFor)

  override def extendSchema[Val](
                                  originalSchema: Schema[Ctx, Val],
                                  extensions: List[ast.SchemaExtensionDefinition],
                                  queryType: ObjectType[Ctx, Val],
                                  mutationType: Option[ObjectType[Ctx, Val]],
                                  subscriptionType: Option[ObjectType[Ctx, Val]],
                                  additionalTypes: List[Type with Named],
                                  directives: List[Directive],
                                  mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Val] =
    Schema[Ctx, Val](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      directives = directives,
      description = originalSchema.description,
      validationRules = originalSchema.validationRules,
      astDirectives = originalSchema.astDirectives ++ extensions.flatMap(_.directives),
      astNodes = {
        val (docs, other) = originalSchema.astNodes.partition(_.isInstanceOf[ast.Document])
        val newDoc = ast.Document.merge(docs.asInstanceOf[Vector[ast.Document]] :+ mat.document)

        (newDoc +: other) ++ extensions
      })

  override def extendUnionType(
                                origin: MatOrigin,
                                extensions: Vector[ast.UnionTypeExtensionDefinition],
                                existing: UnionType[Ctx],
                                types: List[ObjectType[Ctx, _]],
                                mat: AstSchemaMaterializer[Ctx]): UnionType[Ctx] =
    existing.copy(types = types,
      astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
      astNodes = existing.astNodes ++ extensions)
}
