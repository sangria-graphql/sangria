package sangria.schema

import sangria.ast
import sangria.marshalling.{InputUnmarshaller, ResultMarshallerForType}
import sangria.schema.AstSchemaBuilder.TypeName

sealed trait AstSchemaResolver[Ctx]

case class AdditionalTypes[Ctx](additionalTypes: List[MaterializedType])
    extends AstSchemaResolver[Ctx]

object AdditionalTypes {
  def apply[Ctx, F[_]](
      schema: Schema[Ctx, _, F],
      additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] = {
    val origin = ExistingSchemaOrigin(schema)

    AdditionalTypes(additionalTypes.toList.map(MaterializedType(origin, _)))
  }

  def apply[Ctx](additionalTypes: (Type with Named)*): AdditionalTypes[Ctx] =
    AdditionalTypes(additionalTypes.toList.map(MaterializedType(StandaloneOrigin, _)))
}

case class AdditionalDirectives[Ctx](additionalDirectives: Seq[Directive])
    extends AstSchemaResolver[Ctx]

@deprecated("Please migrate to new string-based description format", "1.4.0")
class LegacyCommentDescriptionsResolver[Ctx] extends AstSchemaResolver[Ctx]

object LegacyCommentDescriptionsResolver {
  @deprecated("Please migrate to new string-based description format", "1.4.0")
  def apply[Ctx]() = new LegacyCommentDescriptionsResolver[Ctx]
}

case class DirectiveResolver[Ctx, F[_]](
    directive: Directive,
    resolve: AstDirectiveContext[Ctx, F] => Action[Ctx, Any, F],
    complexity: Option[ComplexityDirectiveContext[Ctx, F] => (Ctx, Args, Double) => Double] = None)
    extends AstSchemaResolver[Ctx]

case class DirectiveFieldProvider[Ctx, F[_]](
    directive: Directive,
    resolve: DirectiveFieldProviderContext[Ctx, F] => List[MaterializedField[Ctx, _]])
    extends AstSchemaResolver[Ctx]

case class DynamicDirectiveFieldProvider[Ctx, A, F[_]](
    directiveName: String,
    resolve: DynamicDirectiveFieldProviderContext[Ctx, A, F] => List[MaterializedField[Ctx, _]])(
    implicit val marshaller: ResultMarshallerForType[A])
    extends AstSchemaResolver[Ctx]

case class DirectiveInputTypeResolver[Ctx, F[_]](
    directive: Directive,
    resolve: AstDirectiveInputTypeContext[Ctx, F] => InputType[Any])
    extends AstSchemaResolver[Ctx]

case class DirectiveOutputTypeResolver[Ctx, F[_]](
    directive: Directive,
    resolve: AstDirectiveOutputTypeContext[Ctx, F] => OutputType[Any])
    extends AstSchemaResolver[Ctx]

case class InputTypeResolver[Ctx, F[_]](
    resolve: PartialFunction[AstInputTypeContext[Ctx, F], InputType[Any]])
    extends AstSchemaResolver[Ctx]

case class OutputTypeResolver[Ctx, F[_]](
    resolve: PartialFunction[AstOutputTypeContext[Ctx, F], OutputType[Any]])
    extends AstSchemaResolver[Ctx]

case class DirectiveScalarResolver[Ctx](
    directive: Directive,
    resolve: AstDirectiveScalarContext => ScalarType[_])
    extends AstSchemaResolver[Ctx]

case class SimpleEnumValueResolver[Ctx](
    resolve: PartialFunction[
      (Either[ast.EnumTypeDefinition, EnumType[_]], ast.EnumValueDefinition),
      String])
    extends AstSchemaResolver[Ctx]

case class ExistingScalarResolver[Ctx, F[_]](
    resolve: PartialFunction[ExistingScalarContext[Ctx, F], ScalarType[Any]])
    extends AstSchemaResolver[Ctx]

case class ExistingEnumResolver[Ctx, F[_]](
    resolve: PartialFunction[ExistingEnumContext[Ctx, F], EnumType[Any]])
    extends AstSchemaResolver[Ctx]

case class ScalarResolver[Ctx](resolve: PartialFunction[ast.ScalarTypeDefinition, ScalarType[_]])
    extends AstSchemaResolver[Ctx]

case class DynamicDirectiveResolver[Ctx, T, F[_]](
    directiveName: String,
    resolve: DynamicDirectiveContext[Ctx, T, F] => Action[Ctx, Any, F],
    complexity: Option[
      ComplexityDynamicDirectiveContext[Ctx, T, F] => (Ctx, Args, Double) => Double] = None)(
    implicit val marshaller: ResultMarshallerForType[T])
    extends AstSchemaResolver[Ctx]

case class FieldResolver[Ctx, F[_]](
    resolve: PartialFunction[
      (Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]], ast.FieldDefinition),
      Context[Ctx, _, F] => Action[Ctx, Any, F]],
    complexity: PartialFunction[
      (Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]], ast.FieldDefinition),
      (Ctx, Args, Double) => Double] = PartialFunction.empty)
    extends AstSchemaResolver[Ctx]

object FieldResolver {
  def map[Ctx, F[_]](config: (String, Map[String, Context[Ctx, _, F] => Action[Ctx, Any, F]])*)
      : FieldResolver[Ctx, F] = {
    val configMap = config.toMap

    FieldResolver {
      case (TypeName(name), field)
          if configMap.contains(name) && configMap(name).contains(field.name) =>
        configMap(name)(field.name)
    }
  }

  def defaultInput[Ctx, In: InputUnmarshaller, F[_]] =
    ResolverBasedAstSchemaBuilder.defaultInputResolver[Ctx, In, F]
}

case class ExistingFieldResolver[Ctx, F[_]](
    resolve: PartialFunction[
      (MatOrigin, Option[ObjectLikeType[Ctx, _, F]], Field[Ctx, _, F]),
      Context[Ctx, _, F] => Action[Ctx, Any, F]])
    extends AstSchemaResolver[Ctx]

object ExistingFieldResolver {
  def map[Ctx, F[_]](config: (String, Map[String, Context[Ctx, _, F] => Action[Ctx, Any, F]])*)
      : ExistingFieldResolver[Ctx, F] = {
    val configMap = config.toMap

    ExistingFieldResolver {
      case (_, tpe, field)
          if tpe.isDefined && configMap.contains(tpe.get.name) && configMap(tpe.get.name).contains(
            field.name) =>
        configMap(tpe.get.name)(field.name)
    }
  }

  def defaultInput[Ctx, In: InputUnmarshaller, F[_]] =
    ResolverBasedAstSchemaBuilder.defaultExistingInputResolver[Ctx, In, F]
}

case class AnyFieldResolver[Ctx, F[_]](
    resolve: PartialFunction[MatOrigin, Context[Ctx, _, F] => Action[Ctx, Any, F]])
    extends AstSchemaResolver[Ctx]

object AnyFieldResolver {
  def defaultInput[Ctx, In: InputUnmarshaller, F[_]] =
    ResolverBasedAstSchemaBuilder.defaultAnyInputResolver[Ctx, In, F]
}

case class InstanceCheck[Ctx](fn: InstanceCheckContext[Ctx] => (Any, Class[_]) => Boolean)
    extends AstSchemaResolver[Ctx]

object InstanceCheck {
  def simple[Ctx](fn: Any => String): InstanceCheck[Ctx] =
    InstanceCheck(c => (value, _) => fn(value) == c.definition.name)

  def field[Ctx, T: InputUnmarshaller]: InstanceCheck[Ctx] =
    field[Ctx, T]("type")

  def field[Ctx, T: InputUnmarshaller](fieldName: String): InstanceCheck[Ctx] = {
    val iu = implicitly[InputUnmarshaller[T]]

    InstanceCheck(c =>
      (value, _) => {
        val node = value.asInstanceOf[T]

        if (!iu.isMapNode(node)) false
        else
          iu.getMapValue(node, fieldName) match {
            case Some(v) => iu.isScalarNode(v) && iu.getScalaScalarValue(v) == c.definition.name
            case None => false
          }
      })
  }
}

case class ExistingInstanceCheck[Ctx, F[_]](
    fn: ExistingInstanceCheckContext[Ctx, F] => (Any, Class[_]) => Boolean)
    extends AstSchemaResolver[Ctx]

case class ConflictResolver[Ctx](resolve: (MatOrigin, Vector[MaterializedType]) => MaterializedType)
    extends AstSchemaResolver[Ctx]

sealed trait AstSchemaGenericResolver[T] {
  def locations: Set[DirectiveLocation.Value]
  def directiveName: String
}

case class GenericDirectiveResolver[T](
    directive: Directive,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    resolve: GenericDirectiveContext => Option[T])
    extends AstSchemaGenericResolver[T] {
  def directiveName = directive.name
}

trait WithTypeLookup[Ctx, F[_]] {
  def origin: MatOrigin
  def materializer: AstSchemaMaterializer[Ctx, F]

  def objectType(typeName: String): ObjectType[Ctx, Any, F] =
    materializer.getObjectType(origin, ast.NamedType(typeName))

  def scalarType(typeName: String): ScalarType[Any] =
    materializer.getScalarType(origin, ast.NamedType(typeName))

  def interfaceType(typeName: String): InterfaceType[Ctx, Any, F] =
    materializer.getInterfaceType(origin, ast.NamedType(typeName))

  def inputType(typeName: String): InputType[_] =
    materializer.getInputType(origin, ast.NamedType(typeName), optional = false)

  def inputType(tpe: ast.Type, replacementNamedType: InputType[_]): InputType[_] =
    materializer.getInputType(origin, tpe, Some(replacementNamedType))

  def outputType(typeName: String): OutputType[_] =
    materializer.getOutputType(origin, ast.NamedType(typeName), optional = false)

  def outputType(tpe: ast.Type, replacementNamedType: OutputType[_]): OutputType[_] =
    materializer.getOutputType(origin, tpe, Some(replacementNamedType))
}

case class GenericDynamicDirectiveResolver[T, A](
    directiveName: String,
    locations: Set[DirectiveLocation.Value] = Set.empty,
    resolve: GenericDynamicDirectiveContext[A] => Option[T])(implicit
    val marshaller: ResultMarshallerForType[T])
    extends AstSchemaGenericResolver[T]

case class AstDirectiveInputTypeContext[Ctx, F[_]](
    origin: MatOrigin,
    directive: ast.Directive,
    schemaDefinition: Option[Type with Named],
    astDefinition: Option[ast.TypeSystemDefinition],
    astField: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    materializer: AstSchemaMaterializer[Ctx, F],
    args: Args)
    extends WithArguments
    with WithTypeLookup[Ctx, F]

case class AstDirectiveOutputTypeContext[Ctx, F[_]](
    origin: MatOrigin,
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx, F],
    args: Args)
    extends WithArguments
    with WithTypeLookup[Ctx, F]

case class AstInputTypeContext[Ctx, F[_]](
    origin: MatOrigin,
    schemaDefinition: Option[Type with Named],
    astDefinition: Option[ast.TypeSystemDefinition],
    astField: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    materializer: AstSchemaMaterializer[Ctx, F])
    extends WithTypeLookup[Ctx, F]

case class AstOutputTypeContext[Ctx, F[_]](
    origin: MatOrigin,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx, F])
    extends WithTypeLookup[Ctx, F]

case class AstDirectiveContext[Ctx, F[_]](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    ctx: Context[Ctx, _, F],
    lastValue: Option[Action[Ctx, Any, F]],
    args: Args)
    extends WithArguments

case class DirectiveFieldProviderContext[Ctx, F[_]](
    origin: MatOrigin,
    directive: ast.Directive,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx, F],
    args: Args)
    extends WithArguments
    with WithTypeLookup[Ctx, F]

case class DynamicDirectiveFieldProviderContext[Ctx, A, F[_]](
    origin: MatOrigin,
    directive: ast.Directive,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    materializer: AstSchemaMaterializer[Ctx, F],
    args: A)
    extends WithTypeLookup[Ctx, F]

case class GenericDirectiveContext(directive: ast.Directive, astNode: ast.AstNode, args: Args)
    extends WithArguments

case class GenericDynamicDirectiveContext[A](
    directive: ast.Directive,
    astNode: ast.AstNode,
    args: A)

case class AstDirectiveScalarContext(
    directive: ast.Directive,
    definition: ast.ScalarTypeDefinition,
    args: Args)
    extends WithArguments

case class ExistingScalarContext[Ctx, F[_]](
    origin: MatOrigin,
    extensions: Vector[ast.ScalarTypeExtensionDefinition],
    existing: ScalarType[Any],
    materializer: AstSchemaMaterializer[Ctx, F])
    extends WithTypeLookup[Ctx, F]

case class ExistingEnumContext[Ctx, F[_]](
    origin: MatOrigin,
    extensions: Vector[ast.EnumTypeExtensionDefinition],
    existing: EnumType[Any],
    materializer: AstSchemaMaterializer[Ctx, F])
    extends WithTypeLookup[Ctx, F]

case class DynamicDirectiveContext[Ctx, In, F[_]](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
    fieldDefinition: ast.FieldDefinition,
    extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
    ctx: Context[Ctx, _, F],
    lastValue: Option[Action[Ctx, Any, F]],
    args: In)

case class ComplexityDynamicDirectiveContext[Ctx, In, F[_]](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
    fieldDefinition: ast.FieldDefinition,
    args: In)

case class ComplexityDirectiveContext[Ctx, F[_]](
    directive: ast.Directive,
    typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
    fieldDefinition: ast.FieldDefinition,
    args: Args)

case class InstanceCheckContext[Ctx](
    origin: MatOrigin,
    definition: ast.ObjectTypeDefinition,
    extensions: List[ast.ObjectTypeExtensionDefinition])

case class ExistingInstanceCheckContext[Ctx, F[_]](
    origin: MatOrigin,
    tpe: ObjectType[Ctx, _, F],
    extensions: List[ast.ObjectTypeExtensionDefinition])
