package sangria.schema

import language.postfixOps

import sangria.ast
import sangria.marshalling.{InputUnmarshaller, ResultMarshallerForType, ToInput}
import sangria.renderer.SchemaRenderer
import sangria.validation.{QueryValidator, UnknownDirectiveViolation, Violation}

import scala.util.control.NonFatal

class ResolverBasedAstSchemaBuilder[Ctx](val resolvers: Seq[AstSchemaResolver[Ctx]]) extends DefaultAstSchemaBuilder[Ctx] {
  protected lazy val directiveResolvers = resolvers collect {case dr: DirectiveResolver[Ctx] ⇒ dr}
  protected lazy val directiveScalarResolvers = resolvers collect {case dr: DirectiveScalarResolver[Ctx] ⇒ dr}
  protected lazy val directiveInpResolvers = resolvers collect {case dr: DirectiveInputTypeResolver[Ctx] ⇒ dr}
  protected lazy val directiveOutResolvers = resolvers collect {case dr: DirectiveOutputTypeResolver[Ctx] ⇒ dr}

  protected lazy val dynamicDirectiveNames = resolvers collect {case dr: DynamicDirectiveResolver[Ctx, _] ⇒ dr.directiveName} toSet
  protected lazy val directives =
    directiveResolvers.map(_.directive) ++
      directiveScalarResolvers.map(_.directive) ++
      directiveInpResolvers.map(_.directive) ++
      directiveOutResolvers.map(_.directive)

  protected lazy val stubQueryType = ObjectType("Query", fields[Unit, Unit](Field("stub", StringType, resolve = _ ⇒ "stub")))
  protected lazy val validationSchema = Schema(stubQueryType, directives = directives.toList ++ BuiltinDirectives)

  override val additionalTypes: List[Type with Named] = resolvers.flatMap {
    case AdditionalTypes(at) ⇒ at
    case _ ⇒ Nil
  }.toList

  def validateSchema(schema: ast.Document): Vector[Violation] =
    allowKnownDynamicDirectives(QueryValidator.default.validateQuery(validationSchema, schema))

  protected def allowKnownDynamicDirectives(violations: Vector[Violation]) =
    violations.filterNot {
      case UnknownDirectiveViolation(name, _, _) if dynamicDirectiveNames.contains(name) ⇒ true
      case _ ⇒ false
    }

  protected def findResolver(directive: ast.Directive): Option[(ast.Directive, AstSchemaResolver[Ctx])] = resolvers.collectFirst {
    case r @ DirectiveResolver(d, _) if d.name == directive.name ⇒ directive → r
    case r @ DynamicDirectiveResolver(directive.name, _) ⇒ directive → r
  }

  protected def findResolver(typeDefinition: ast.TypeDefinition, definition: ast.FieldDefinition): Option[FieldResolver[Ctx]] = {
    val arg = typeDefinition → definition

    resolvers.collectFirst {
      case r @ FieldResolver(fn) if fn.isDefinedAt(arg) ⇒ r
    }
  }

  protected def createArgs(directive: Directive, astDirective: ast.Directive): Args = {
    import sangria.marshalling.queryAst._

    Args(
      directive.arguments,
      ast.ObjectValue(astDirective.arguments.map(arg ⇒ ast.ObjectField(arg.name, arg.value))): ast.Value)
  }

  protected def createDynamicArgs[T : ResultMarshallerForType](astDirective: ast.Directive): T = {
    import sangria.marshalling.queryAst._
    import sangria.marshalling.MarshallingUtil._

    val value: ast.Value = ast.ObjectValue(astDirective.arguments.map(arg ⇒ ast.ObjectField(arg.name, arg.value)))

    value.convertMarshaled[T]
  }

  override def resolveField(typeDefinition: ast.TypeDefinition, extensions: Vector[ast.TypeExtensionDefinition], definition: ast.FieldDefinition) = {
    val dResolvers = definition.directives flatMap (findResolver(_))

    // TODO: create args outside of resolver
    if (dResolvers.nonEmpty)
      c ⇒ {
        val resultAction =
          dResolvers.foldLeft(None: Option[Action[Ctx, Any]]) {
            case (acc, (d, DirectiveResolver(sd, fn))) ⇒
              Some(fn(AstDirectiveContext[Ctx](d, typeDefinition, definition, extensions, c, acc, createArgs(sd, d))))

            case (acc, (d, ddc @ DynamicDirectiveResolver(_, fn))) ⇒
              implicit val marshaller = ddc.marshaller

              Some(fn(DynamicDirectiveContext[Ctx, Any](d, typeDefinition, definition, extensions, c, acc, createDynamicArgs(d))))

            case (acc, _) ⇒
              acc
          }

        resultAction getOrElse (throw SchemaMaterializationException(s"Resolver for '${typeDefinition.name}.${definition.name}' haven't returned any action!"))
      }
    else
      findResolver(typeDefinition, definition) match {
        case Some(fResolver) ⇒
          fResolver.resolve(typeDefinition, definition)
        case None ⇒
          super.resolveField(typeDefinition, extensions, definition)
      }
  }

  override def buildFieldType(
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.TypeExtensionDefinition],
    definition: ast.FieldDefinition,
    arguments: List[Argument[_]],
    mat: AstSchemaMaterializer[Ctx]
  ) = {
    val tpe =
      resolvers.collectFirst {
        case DirectiveOutputTypeResolver(d, fn) if definition.directives.exists(_.name == d.name) ⇒
          val astDirective = definition.directives.find(_.name == d.name).get

          fn(AstDirectiveOutputTypeContext(astDirective, typeDefinition, definition, extensions, mat, createArgs(d, astDirective)))
      }

    tpe getOrElse super.buildFieldType(typeDefinition, extensions, definition, arguments, mat)
  }

  private def buildInputTypeResolver(
      contextDefinition: Either[(ast.TypeSystemDefinition, Option[ast.FieldDefinition]), ast.InputObjectTypeDefinition],
      definition: ast.InputValueDefinition,
      mat: AstSchemaMaterializer[Ctx]) =
    resolvers.collectFirst {
      case DirectiveInputTypeResolver(d, fn) if definition.directives.exists(_.name == d.name) ⇒
        val astDirective = definition.directives.find(_.name == d.name).get

        fn(AstDirectiveInputTypeContext(astDirective, contextDefinition, definition, mat, createArgs(d, astDirective)))
    }

  override def buildArgumentType(
      typeDefinition: ast.TypeSystemDefinition,
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[Tuple2[_, ToInput[_, _]]],
      mat: AstSchemaMaterializer[Ctx]) =
    buildInputTypeResolver(Left(typeDefinition → fieldDefinition), definition, mat) getOrElse
      super.buildArgumentType(typeDefinition, fieldDefinition, definition, defaultValue, mat)

  override def buildInputFieldType(
      typeDefinition: ast.InputObjectTypeDefinition,
      definition: ast.InputValueDefinition,
      defaultValue: Option[Tuple2[_, ToInput[_, _]]],
      mat: AstSchemaMaterializer[Ctx]) =
    buildInputTypeResolver(Right(typeDefinition), definition, mat) getOrElse
      super.buildInputFieldType(typeDefinition, definition, defaultValue, mat)

  override def buildScalarType(definition: ast.ScalarTypeDefinition, mat: AstSchemaMaterializer[Ctx]) = {
    val scalar =
      resolvers.collectFirst {
        case DirectiveScalarResolver(d, fn) if definition.directives.exists(_.name == d.name) ⇒
          val astDirective = definition.directives.find(_.name == d.name).get

          fn(AstDirectiveScalarContext(astDirective, definition, createArgs(d, astDirective)))

        case ScalarResolver(fn) if fn.isDefinedAt(definition) ⇒
          fn(definition)
      }

    scalar match {
      case Some(s) ⇒ Some(s.asInstanceOf[ScalarType[Any]])
      case _ ⇒ super.buildScalarType(definition, mat)
    }
  }
}

object ResolverBasedAstSchemaBuilder {
  def apply[Ctx](resolvers: AstSchemaResolver[Ctx]*) = new ResolverBasedAstSchemaBuilder[Ctx](resolvers)

  private def extractScalar[In](t: ScalarType[_], value: In)(implicit iu: InputUnmarshaller[In]) = {
    iu.getScalaScalarValue(value)
  }

  private def extractEnum[In](t: EnumType[_], value: In)(implicit iu: InputUnmarshaller[In]) = {
    iu.getScalarValue(value)
  }

  def extractValue[In](tpe: OutputType[_], value: Option[In])(implicit iu: InputUnmarshaller[In]): Any = tpe match {
    case OptionType(ofType) ⇒ Option(extractValue(ofType, value))
    case _ if value.isEmpty || !iu.isDefined(value.get) ⇒ null
    case ListType(ofType) ⇒ iu.getListValue(value.get) map (v ⇒ extractValue(ofType, Some(v)))
    case t: ScalarAlias[_, _] ⇒ extractValue(t.aliasFor, value)
    case t: ScalarType[_] ⇒ extractScalar(t, value.get)
    case t: EnumType[_] ⇒ extractEnum(t, value.get)
    case _: CompositeType[_] ⇒ value.get
    case t ⇒ throw new SchemaMaterializationException(s"Extractor for a type '${SchemaRenderer.renderTypeName(t)}' is not supported yet.")
  }

  def extractFieldValue[In](parentType: CompositeType[_], field: Field[_, _], value: In)(implicit iu: InputUnmarshaller[In]): Any = {
    try
      if (!iu.isMapNode(value))
        throw SchemaMaterializationException(s"Can't extract value for a field '${parentType.name}.${field.name}': not a map-like value.")
      else
        extractValue(field.fieldType, iu.getMapValue(value, field.name))
    catch {
      case e: SchemaMaterializationException ⇒
        throw e

      case NonFatal(e) ⇒
        throw SchemaMaterializationException(s"Can't extract value for a field '${parentType.name}.${field.name}'.", e)
    }
  }

  def defaultInputResolver[Ctx, In : InputUnmarshaller] = FieldResolver[Ctx] {
    case (tpeDef, fieldDef) ⇒
      c ⇒ extractFieldValue(c.parentType, c.field, c.value.asInstanceOf[In])
  }
}
