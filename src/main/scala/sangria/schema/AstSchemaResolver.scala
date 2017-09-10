package sangria.schema

import sangria.ast
import sangria.ast.{FieldDefinition, TypeSystemDefinition}
import sangria.marshalling.ResultMarshallerForType

sealed trait AstSchemaResolver[Ctx]

case class AdditionalTypes[Ctx](additionalTypes: List[MaterializedType]) extends AstSchemaResolver[Ctx]

object AdditionalTypes {
  def apply[Ctx](additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] =
    AdditionalTypes(additionalTypes.toList.map(MaterializedType(ExistingOrigin, _)))
}

case class AdditionalDirectives[Ctx](additionalDirectives: Seq[Directive]) extends AstSchemaResolver[Ctx]

case class DirectiveResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveContext[Ctx] ⇒ Action[Ctx, Any]) extends AstSchemaResolver[Ctx]

case class DirectiveInputTypeResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveInputTypeContext ⇒ InputType[Any]) extends AstSchemaResolver[Ctx]

case class DirectiveOutputTypeResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveOutputTypeContext ⇒ OutputType[Any]) extends AstSchemaResolver[Ctx]

case class DirectiveScalarResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveScalarContext ⇒ ScalarType[_]) extends AstSchemaResolver[Ctx]

case class ScalarResolver[Ctx](resolve: PartialFunction[ast.ScalarTypeDefinition, ScalarType[_]]) extends AstSchemaResolver[Ctx]

case class DynamicDirectiveResolver[Ctx, T](
  directiveName: String,
  resolve: DynamicDirectiveContext[Ctx, T] ⇒ Action[Ctx, Any])(implicit val marshaller: ResultMarshallerForType[T]) extends AstSchemaResolver[Ctx]

case class FieldResolver[Ctx](
  resolve: PartialFunction[(ast.TypeDefinition, ast.FieldDefinition), Context[Ctx, _] ⇒ Action[Ctx, Any]]) extends AstSchemaResolver[Ctx]

case class ExistingFieldResolver[Ctx](
  resolve: PartialFunction[(ObjectLikeType[Ctx, _], Field[Ctx, _]), Context[Ctx, _] ⇒ Action[Ctx, Any]]) extends AstSchemaResolver[Ctx]

case class AnyFieldResolver[Ctx](
  resolve: PartialFunction[MatOrigin, Context[Ctx, _] ⇒ Action[Ctx, Any]]) extends AstSchemaResolver[Ctx]

case class ConflictResolver[Ctx](resolve: (MatOrigin, Vector[MaterializedType]) ⇒ MaterializedType) extends AstSchemaResolver[Ctx]

case class GenericDirectiveResolver[T](
  directive: Directive,
  locations: Set[DirectiveLocation.Value] = Set.empty,
  resolve: GenericDirectiveContext ⇒ Option[T])

case class AstDirectiveInputTypeContext(
  directive: ast.Directive,
  contextDefinition: Either[(TypeSystemDefinition, Option[FieldDefinition]), ast.InputObjectTypeDefinition],
  definition: ast.InputValueDefinition,
  materializer: AstSchemaMaterializer[_],
  args: Args) extends WithArguments

case class AstDirectiveOutputTypeContext(
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  fieldDefinition: ast.FieldDefinition,
  extensions: Vector[ast.TypeExtensionDefinition],
  materializer: AstSchemaMaterializer[_],
  args: Args) extends WithArguments

case class AstDirectiveContext[Ctx](
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  fieldDefinition: ast.FieldDefinition,
  extensions: Vector[ast.TypeExtensionDefinition],
  ctx: Context[Ctx, _],
  lastValue: Option[Action[Ctx, Any]],
  args: Args) extends WithArguments

case class GenericDirectiveContext(
  directive: ast.Directive,
  astNode: ast.AstNode,
  args: Args) extends WithArguments

case class AstDirectiveScalarContext(
  directive: ast.Directive,
  definition: ast.ScalarTypeDefinition,
  args: Args) extends WithArguments

case class DynamicDirectiveContext[Ctx, In](
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  fieldDefinition: ast.FieldDefinition,
  extensions: Vector[ast.TypeExtensionDefinition],
  ctx: Context[Ctx, _],
  lastValue: Option[Action[Ctx, Any]],
  args: In)