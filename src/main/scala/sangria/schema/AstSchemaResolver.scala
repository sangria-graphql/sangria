package sangria.schema

import language.existentials

import sangria.ast
import sangria.ast.{FieldDefinition, TypeSystemDefinition}
import sangria.marshalling.ResultMarshallerForType

sealed trait AstSchemaResolver[Ctx]

case class AdditionalTypes[Ctx](additionalTypes: List[MaterializedType]) extends AstSchemaResolver[Ctx]

object AdditionalTypes {
  def apply[Ctx](schema: Schema[Ctx, _], additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] = {
    val origin = ExistingSchemaOrigin(schema)

    AdditionalTypes(additionalTypes.toList.map(MaterializedType(origin, _)))
  }

  def apply[Ctx](additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] =
    AdditionalTypes(additionalTypes.toList.map(MaterializedType(StandaloneOrigin, _)))
}

case class AdditionalDirectives[Ctx](additionalDirectives: Seq[Directive]) extends AstSchemaResolver[Ctx]

case class DirectiveResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveContext[Ctx] ⇒ Action[Ctx, Any],
  complexity: Option[ComplexityDirectiveContext[Ctx] ⇒ (Ctx, Args, Double) ⇒ Double] = None) extends AstSchemaResolver[Ctx]

case class DirectiveFieldProvider[Ctx](
  directive: Directive,
  resolve: DirectiveFieldProviderContext[Ctx] ⇒ List[MaterializedField[Ctx, _]]) extends AstSchemaResolver[Ctx]

case class DynamicDirectiveFieldProvider[Ctx, A](
  directiveName: String,
  resolve: DynamicDirectiveFieldProviderContext[Ctx, A] ⇒ List[MaterializedField[Ctx, _]])(implicit val marshaller: ResultMarshallerForType[A]) extends AstSchemaResolver[Ctx]

case class DirectiveInputTypeResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveInputTypeContext ⇒ InputType[Any]) extends AstSchemaResolver[Ctx]

case class DirectiveOutputTypeResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveOutputTypeContext ⇒ OutputType[Any]) extends AstSchemaResolver[Ctx]

case class DirectiveScalarResolver[Ctx](
  directive: Directive,
  resolve: AstDirectiveScalarContext ⇒ ScalarType[_]) extends AstSchemaResolver[Ctx]

case class ExistingScalarResolver[Ctx](
  resolve: PartialFunction[ExistingScalarContext[Ctx], ScalarType[Any]]) extends AstSchemaResolver[Ctx]

case class ExistingEnumResolver[Ctx](
  resolve: PartialFunction[ExistingEnumContext[Ctx], EnumType[Any]]) extends AstSchemaResolver[Ctx]

case class ScalarResolver[Ctx](resolve: PartialFunction[ast.ScalarTypeDefinition, ScalarType[_]]) extends AstSchemaResolver[Ctx]

case class DynamicDirectiveResolver[Ctx, T](
  directiveName: String,
  resolve: DynamicDirectiveContext[Ctx, T] ⇒ Action[Ctx, Any],
  complexity: Option[ComplexityDynamicDirectiveContext[Ctx, T] ⇒ (Ctx, Args, Double) ⇒ Double] = None)(implicit val marshaller: ResultMarshallerForType[T]) extends AstSchemaResolver[Ctx]

case class FieldResolver[Ctx](
  resolve: PartialFunction[(ast.TypeDefinition, ast.FieldDefinition), Context[Ctx, _] ⇒ Action[Ctx, Any]],
  complexity: PartialFunction[(ast.TypeDefinition, ast.FieldDefinition), (Ctx, Args, Double) ⇒ Double] = PartialFunction.empty) extends AstSchemaResolver[Ctx]

object FieldResolver {
  def map[Ctx](config: (String, Map[String, Context[Ctx, _] ⇒ Action[Ctx, Any]])*): FieldResolver[Ctx] = {
    val configMap = config.toMap

    FieldResolver {
      case (tpe, field) if configMap.contains(tpe.name) && configMap(tpe.name).contains(field.name) ⇒
        configMap(tpe.name)(field.name)
    }
  }
}

case class ExistingFieldResolver[Ctx](
  resolve: PartialFunction[(MatOrigin, Option[ObjectLikeType[Ctx, _]], Field[Ctx, _]), Context[Ctx, _] ⇒ Action[Ctx, Any]]) extends AstSchemaResolver[Ctx]

object ExistingFieldResolver {
  def map[Ctx](config: (String, Map[String, Context[Ctx, _] ⇒ Action[Ctx, Any]])*): ExistingFieldResolver[Ctx] = {
    val configMap = config.toMap

    ExistingFieldResolver {
      case (_, tpe, field) if tpe.isDefined && configMap.contains(tpe.get.name) && configMap(tpe.get.name).contains(field.name) ⇒
        configMap(tpe.get.name)(field.name)
    }
  }
}

case class AnyFieldResolver[Ctx](
  resolve: PartialFunction[MatOrigin, Context[Ctx, _] ⇒ Action[Ctx, Any]]) extends AstSchemaResolver[Ctx]

case class InstanceCheck[Ctx](
  fn: InstanceCheckContext[Ctx] ⇒ (Any, Class[_]) ⇒ Boolean) extends AstSchemaResolver[Ctx]

case class ExistingInstanceCheck[Ctx](
  fn: ExistingInstanceCheckContext[Ctx] ⇒ (Any, Class[_]) ⇒ Boolean) extends AstSchemaResolver[Ctx]

case class ConflictResolver[Ctx](resolve: (MatOrigin, Vector[MaterializedType]) ⇒ MaterializedType) extends AstSchemaResolver[Ctx]

sealed trait AstSchemaGenericResolver[T] {
  def locations: Set[DirectiveLocation.Value]
  def directiveName: String
}

case class GenericDirectiveResolver[T](
    directive: Directive,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    resolve: GenericDirectiveContext ⇒ Option[T]) extends AstSchemaGenericResolver[T] {
  def directiveName = directive.name
}

case class GenericDynamicDirectiveResolver[T, A](
  directiveName: String,
  locations: Set[DirectiveLocation.Value] = Set.empty,
  resolve: GenericDynamicDirectiveContext[A] ⇒ Option[T])(implicit val marshaller: ResultMarshallerForType[T]) extends AstSchemaGenericResolver[T]

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

case class DirectiveFieldProviderContext[Ctx](
  origin: MatOrigin,
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  extensions: Vector[ast.TypeExtensionDefinition],
  materializer: AstSchemaMaterializer[Ctx],
  args: Args) extends WithArguments

case class DynamicDirectiveFieldProviderContext[Ctx, A](
  origin: MatOrigin,
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  extensions: Vector[ast.TypeExtensionDefinition],
  materializer: AstSchemaMaterializer[Ctx],
  args: A)

case class GenericDirectiveContext(
  directive: ast.Directive,
  astNode: ast.AstNode,
  args: Args) extends WithArguments

case class GenericDynamicDirectiveContext[A](
  directive: ast.Directive,
  astNode: ast.AstNode,
  args: A)

case class AstDirectiveScalarContext(
  directive: ast.Directive,
  definition: ast.ScalarTypeDefinition,
  args: Args) extends WithArguments

case class ExistingScalarContext[Ctx](
  origin: MatOrigin,
  existing: ScalarType[Any],
  mat: AstSchemaMaterializer[Ctx])

case class ExistingEnumContext[Ctx](
  origin: MatOrigin,
  existing: EnumType[Any],
  mat: AstSchemaMaterializer[Ctx])

case class DynamicDirectiveContext[Ctx, In](
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  fieldDefinition: ast.FieldDefinition,
  extensions: Vector[ast.TypeExtensionDefinition],
  ctx: Context[Ctx, _],
  lastValue: Option[Action[Ctx, Any]],
  args: In)

case class ComplexityDynamicDirectiveContext[Ctx, In](
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  fieldDefinition: ast.FieldDefinition,
  args: In)

case class ComplexityDirectiveContext[Ctx](
  directive: ast.Directive,
  typeDefinition: ast.TypeDefinition,
  fieldDefinition: ast.FieldDefinition,
  args: Args)

case class InstanceCheckContext[Ctx](
  origin: MatOrigin,
  definition: ast.ObjectTypeDefinition,
  extensions: List[ast.TypeExtensionDefinition])

case class ExistingInstanceCheckContext[Ctx](
  origin: MatOrigin,
  tpe: ObjectType[Ctx, _],
  extensions: List[ast.TypeExtensionDefinition])