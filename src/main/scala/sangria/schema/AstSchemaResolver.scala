package sangria.schema

import sangria.ast
import sangria.ast.{FieldDefinition, TypeSystemDefinition}
import sangria.marshalling.ResultMarshallerForType

import scala.reflect.{ClassTag, classTag}

sealed trait AstSchemaResolver[Ctx]

case class AdditionalTypes[Ctx](additionalTypes: List[MaterializedType]) extends AstSchemaResolver[Ctx]

object AdditionalTypes {
  def apply[Ctx](additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] =
    AdditionalTypes(additionalTypes.toList.map(MaterializedType(ExistingOrigin, _)))
}

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

case class GenericDirectiveResolver[Ctx, T : ClassTag](
    directive: Directive,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    resolve: GenericDirectiveContext ⇒ GenericDirectiveValue[T]) extends AstSchemaResolver[Ctx] {
  def ofClass = classTag[T].runtimeClass
}

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

case class GenericDirectiveValue[T](value: Option[T], types: Vector[MaterializedType])

object GenericDirectiveValue {
  val empty: GenericDirectiveValue[Any] = GenericDirectiveValue(None, Vector.empty)

  def apply[T](value: T): GenericDirectiveValue[T] =
    GenericDirectiveValue(Some(value), Vector.empty)

  def apply[T](value: T, origin: MatOrigin, types: (Type with Named)*): GenericDirectiveValue[T] =
    GenericDirectiveValue(Some(value), types.toVector.map(MaterializedType(origin, _)))

  def apply(origin: MatOrigin, types: (Type with Named)*): GenericDirectiveValue[Nothing] =
    GenericDirectiveValue(None, types.toVector.map(MaterializedType(origin, _)))
}

trait MatOrigin {
  def description: String

  override def toString = description
}

abstract class BaseMatOrigin(val description: String) extends MatOrigin

case object SDLOrigin extends BaseMatOrigin("SDL")
case object ExistingOrigin extends BaseMatOrigin("existing schema")

sealed trait MaterializedType {
  def origin: MatOrigin
  def name: String
  def rename(newName: String): MaterializedType
}

object MaterializedType {
  def apply(origin: MatOrigin, tpe: ast.TypeDefinition): MaterializedType = MaterializedTypeAst(origin, tpe)
  def apply(origin: MatOrigin, tpe: Type with Named): MaterializedType = MaterializedTypeInst(origin, tpe)
}

case class MaterializedTypeAst(origin: MatOrigin, tpe: ast.TypeDefinition) extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
}

case class MaterializedTypeInst(origin: MatOrigin, tpe: Type with Named) extends MaterializedType {
  def name = tpe.name
  def rename(newName: String) = copy(tpe = tpe.rename(newName))
}