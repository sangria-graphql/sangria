package sangria.schema

import language.postfixOps
import sangria.ast
import sangria.ast.AstVisitor
import sangria.execution.MaterializedSchemaValidationError
import sangria.marshalling.{InputUnmarshaller, ResultMarshallerForType, ToInput}
import sangria.renderer.SchemaRenderer
import sangria.validation.{QueryValidator, UnknownDirectiveViolation, ValidatorStack, Violation}
import sangria.validation.rules.KnownDirectives
import sangria.visitor.VisitorCommand

import scala.collection.immutable.VectorBuilder
import scala.reflect.{ClassTag, classTag}
import scala.util.Try
import scala.util.control.NonFatal

class ResolverBasedAstSchemaBuilder[Ctx](val resolvers: Seq[AstSchemaResolver[Ctx]]) extends DefaultAstSchemaBuilder[Ctx] {
  protected lazy val directiveResolvers = resolvers collect {case dr: DirectiveResolver[Ctx] ⇒ dr}
  protected lazy val directiveScalarResolvers = resolvers collect {case dr: DirectiveScalarResolver[Ctx] ⇒ dr}
  protected lazy val directiveInpResolvers = resolvers collect {case dr: DirectiveInputTypeResolver[Ctx] ⇒ dr}
  protected lazy val directiveOutResolvers = resolvers collect {case dr: DirectiveOutputTypeResolver[Ctx] ⇒ dr}
  protected lazy val additionalDirectives = resolvers flatMap {
    case AdditionalDirectives(ad) ⇒ ad
    case _ ⇒ Nil
  }

  protected lazy val dynamicDirectiveNames = resolvers collect {case dr: DynamicDirectiveResolver[Ctx, _] ⇒ dr.directiveName} toSet
  protected lazy val directives =
    directiveResolvers.map(_.directive) ++
      directiveScalarResolvers.map(_.directive) ++
      directiveInpResolvers.map(_.directive) ++
      directiveOutResolvers.map(_.directive) ++
      additionalDirectives

  protected lazy val stubQueryType = ObjectType("Query", fields[Unit, Unit](Field("stub", StringType, resolve = _ ⇒ "stub")))
  protected lazy val validationSchema = Schema(stubQueryType, directives = directives.toList ++ BuiltinDirectives)

  override lazy val additionalTypes: List[MaterializedType] = resolvers.flatMap {
    case AdditionalTypes(at) ⇒ at
    case _ ⇒ Nil
  }.toList

  def validateSchema(schema: ast.Document): Vector[Violation] =
    allowKnownDynamicDirectives(QueryValidator.default.validateQuery(validationSchema, schema))

  def validateSchemaWithException(schema: ast.Document): ResolverBasedAstSchemaBuilder[Ctx] = {
    val violations = validateSchema(schema)

    if (violations.nonEmpty) throw MaterializedSchemaValidationError(violations)
    else this
  }

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

  protected def findExistingResolver(typeDefinition: ObjectLikeType[Ctx, _], field: Field[Ctx, _]): Option[ExistingFieldResolver[Ctx]] = {
    val arg = typeDefinition → field

    resolvers.collectFirst {
      case r @ ExistingFieldResolver(fn) if fn.isDefinedAt(arg) ⇒ r
    }
  }

  protected def findAnyResolver(origin: MatOrigin): Option[AnyFieldResolver[Ctx]] =
    resolvers.collectFirst {
      case r @ AnyFieldResolver(fn) if fn.isDefinedAt(origin) ⇒ r
    }

  protected def createDynamicArgs[T : ResultMarshallerForType](astDirective: ast.Directive): T = {
    import sangria.marshalling.queryAst._
    import sangria.marshalling.ImprovedMarshallingUtil._

    val value: ast.Value = ast.ObjectValue(astDirective.arguments.map(arg ⇒ ast.ObjectField(arg.name, arg.value)))

    value.convertMarshaled[T]
  }

  override def resolveField(origin: MatOrigin, typeDefinition: ast.TypeDefinition, extensions: Vector[ast.TypeExtensionDefinition], definition: ast.FieldDefinition, mat: AstSchemaMaterializer[Ctx]) = {
    val dResolvers = definition.directives flatMap (findResolver(_))
    
    if (dResolvers.nonEmpty)
      c ⇒ {
        val resultAction =
          dResolvers.foldLeft(None: Option[Action[Ctx, Any]]) {
            case (acc, (d, DirectiveResolver(sd, fn))) ⇒
              Some(fn(AstDirectiveContext[Ctx](d, typeDefinition, definition, extensions, c, acc, Args(sd, d))))

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
          findAnyResolver(origin) match {
            case Some(fResolver) ⇒
              fResolver.resolve(origin)
            case None ⇒
              super.resolveField(origin, typeDefinition, extensions, definition, mat)
          }
      }
  }

  override def extendFieldResolver(origin: MatOrigin, typeDefinition: ObjectLikeType[Ctx, _], existing: Field[Ctx, Any], fieldType: OutputType[_], mat: AstSchemaMaterializer[Ctx]) =
    findExistingResolver(typeDefinition, existing) match {
      case Some(fResolver) ⇒
        fResolver.resolve(typeDefinition, existing)
      case None ⇒
        findAnyResolver(origin) match {
          case Some(fResolver) ⇒
            fResolver.resolve(origin)
          case None ⇒
            super.extendFieldResolver(origin, typeDefinition, existing, fieldType, mat)
        }
    }


  override def buildFieldType(
    origin: MatOrigin,
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

          fn(AstDirectiveOutputTypeContext(astDirective, typeDefinition, definition, extensions, mat, Args(d, astDirective)))
      }

    tpe getOrElse super.buildFieldType(origin, typeDefinition, extensions, definition, arguments, mat)
  }

  private def buildInputTypeResolver(
      contextDefinition: Either[(ast.TypeSystemDefinition, Option[ast.FieldDefinition]), ast.InputObjectTypeDefinition],
      definition: ast.InputValueDefinition,
      mat: AstSchemaMaterializer[Ctx]) =
    resolvers.collectFirst {
      case DirectiveInputTypeResolver(d, fn) if definition.directives.exists(_.name == d.name) ⇒
        val astDirective = definition.directives.find(_.name == d.name).get

        fn(AstDirectiveInputTypeContext(astDirective, contextDefinition, definition, mat, Args(d, astDirective)))
    }

  override def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: ast.TypeSystemDefinition,
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[Tuple2[_, ToInput[_, _]]],
      mat: AstSchemaMaterializer[Ctx]) =
    buildInputTypeResolver(Left(typeDefinition → fieldDefinition), definition, mat) getOrElse
      super.buildArgumentType(origin, typeDefinition, fieldDefinition, definition, defaultValue, mat)

  override def buildInputFieldType(
      origin: MatOrigin,
      typeDefinition: ast.InputObjectTypeDefinition,
      definition: ast.InputValueDefinition,
      defaultValue: Option[Tuple2[_, ToInput[_, _]]],
      mat: AstSchemaMaterializer[Ctx]) =
    buildInputTypeResolver(Right(typeDefinition), definition, mat) getOrElse
      super.buildInputFieldType(origin, typeDefinition, definition, defaultValue, mat)

  override def buildScalarType(origin: MatOrigin, definition: ast.ScalarTypeDefinition, mat: AstSchemaMaterializer[Ctx]) = {
    val scalar =
      resolvers.collectFirst {
        case DirectiveScalarResolver(d, fn) if definition.directives.exists(_.name == d.name) ⇒
          val astDirective = definition.directives.find(_.name == d.name).get

          fn(AstDirectiveScalarContext(astDirective, definition, Args(d, astDirective)))

        case ScalarResolver(fn) if fn.isDefinedAt(definition) ⇒
          fn(definition)
      }

    scalar match {
      case Some(s) ⇒ Some(s.asInstanceOf[ScalarType[Any]])
      case _ ⇒ super.buildScalarType(origin, definition, mat)
    }
  }

  override def resolveNameConflict(fromOrigin: MatOrigin, types: Vector[MaterializedType]) =
    resolvers.collectFirst {case r: ConflictResolver[Ctx] ⇒ r.resolve(fromOrigin, types)} getOrElse
      super.resolveNameConflict(fromOrigin, types)
}

object ResolverBasedAstSchemaBuilder {
  def apply[Ctx](resolvers: AstSchemaResolver[Ctx]*) = new ResolverBasedAstSchemaBuilder[Ctx](resolvers)

  private def invalidType[In](expected: String, got: In)(implicit iu: InputUnmarshaller[In]) =
    throw InputMaterializationException(s"Expected $expected value, but got: " + iu.render(got))

  private def extractScalar[In](t: ScalarType[_], value: In)(implicit iu: InputUnmarshaller[In]) = {
    iu.getScalaScalarValue(value)
  }

  private def extractEnum[In](t: EnumType[_], value: In)(implicit iu: InputUnmarshaller[In]) = {
    val coerced = iu.getScalarValue(value)

    t match {
      case BooleanType ⇒
        coerced match  {
          case v: Boolean ⇒ v
          case v: String ⇒ Try(v.toBoolean).fold(invalidType("Boolean", value), identity)
          case _ ⇒ invalidType("Boolean", value)
        }
      case StringType ⇒
        coerced.toString
      case IDType ⇒
        coerced match  {
          case s: String ⇒ s
          case _ ⇒ invalidType("ID", value)
        }
      case IntType ⇒
        coerced match  {
          case v: Int ⇒ v
          case i: Long if i.isValidInt ⇒ i.toInt
          case v: BigInt if v.isValidInt ⇒ v
          case d: Double if d.isValidInt ⇒ d.intValue
          case d: BigDecimal if d.isValidInt ⇒ d.intValue
          case v: String ⇒ Try(v.toInt).fold(invalidType("Int", value), identity)
          case _ ⇒ invalidType("Int", value)
        }
      case IntType ⇒
        coerced match  {
          case i: Int ⇒ i: Long
          case i: Long ⇒ i
          case i: BigInt if !i.isValidLong ⇒ invalidType("Long", value)
          case i: BigInt ⇒ i.longValue
          case d: Double if d.isWhole ⇒ d.toLong
          case d: BigDecimal if d.isValidLong ⇒ d.longValue()
          case v: String ⇒ Try(v.toLong).fold(invalidType("Long", value), identity)
          case _ ⇒ invalidType("Long", value)
        }
      case BigIntType ⇒
        coerced match {
          case i: Int ⇒ BigInt(i)
          case i: Long ⇒ BigInt(i)
          case i: BigInt ⇒ i
          case d: Double if d.isWhole ⇒ BigInt(d.toLong)
          case d: BigDecimal if d.isWhole ⇒ d.toBigInt
          case v: String ⇒ Try(BigInt(v)).fold(invalidType("BigInt", value), identity)
          case _ ⇒ invalidType("BigInt", value)
        }
      case BigDecimalType ⇒
        coerced match {
          case i: Int ⇒ BigDecimal(i)
          case i: Long ⇒ BigDecimal(i)
          case i: BigInt ⇒ BigDecimal(i)
          case d: Double ⇒ BigDecimal(d)
          case d: BigDecimal ⇒ d
          case v: String ⇒ Try(BigDecimal(v)).fold(invalidType("BigDecimal", value), identity)
          case _ ⇒ invalidType("BigDecimal", value)
        }
      case FloatType ⇒
        coerced match {
          case i: Int ⇒ i.toDouble
          case i: Long ⇒ i.toDouble
          case i: BigInt if !i.isValidDouble ⇒ invalidType("Float", value)
          case i: BigInt ⇒ i.doubleValue()
          case d: Double ⇒ d
          case d: BigDecimal if !d.isDecimalDouble ⇒ invalidType("Float", value)
          case d: BigDecimal ⇒ d.doubleValue()
          case v: String ⇒ Try(v.toDouble).fold(invalidType("Float", value), identity)
          case _ ⇒ invalidType("Float", value)
        }
      case _ ⇒ coerced
    }
  }

  def extractValue[In](tpe: OutputType[_], value: Option[In])(implicit iu: InputUnmarshaller[In]): Any = tpe match {
    case OptionType(ofType) ⇒ Option(extractValue(ofType, value))
    case _ if value.isEmpty || !iu.isDefined(value.get) ⇒ null
    case ListType(ofType) ⇒ iu.getListValue(value.get) map (v ⇒ extractValue(ofType, Some(v)))
    case t: ScalarAlias[_, _] ⇒ extractValue(t.aliasFor, value)
    case t: ScalarType[_] ⇒ extractScalar(t, value.get)
    case t: EnumType[_] ⇒ extractEnum(t, value.get)
    case _: CompositeType[_] ⇒
      val objValue = value.get

      if (iu.isMapNode(objValue)) objValue
      else invalidType("Object", objValue)
    case t ⇒ throw SchemaMaterializationException(s"Extractor for a type '${SchemaRenderer.renderTypeName(t)}' is not supported yet.")
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

  def defaultInputResolver[Ctx, In : InputUnmarshaller] =
    FieldResolver[Ctx] {
      case (_, _) ⇒ c ⇒ extractFieldValue(c.parentType, c.field, c.value.asInstanceOf[In])
    }

  def defaultExistingInputResolver[Ctx, In : InputUnmarshaller] =
    ExistingFieldResolver[Ctx] {
      case (_, _) ⇒ c ⇒ extractFieldValue(c.parentType, c.field, c.value.asInstanceOf[In])
    }

  def defaultAnyInputResolver[Ctx, In : InputUnmarshaller] =
    AnyFieldResolver[Ctx] {
      case origin if origin != ExistingOrigin ⇒ c ⇒ extractFieldValue(c.parentType, c.field, c.value.asInstanceOf[In])
    }

  def collectGeneric[T](schema: ast.Document, resolvers: GenericDirectiveResolver[T]*): Vector[T] = {
    val result = new VectorBuilder[T]
    val resolversByName = resolvers.groupBy(_.directive.name)
    val stack = ValidatorStack.empty[ast.AstNode]

    AstVisitor.visit(schema, AstVisitor(
      onEnter = {
        case node: ast.WithDirectives ⇒
          stack.push(node)

          result ++=
              node.directives.flatMap(astDir ⇒
                findByLocation(stack, node, resolversByName.getOrElse(astDir.name, Nil))
                  .flatMap(d ⇒ d.resolve(GenericDirectiveContext(astDir, node, Args(d.directive, astDir)))))

          VisitorCommand.Continue

        case node ⇒
          stack.push(node)
          VisitorCommand.Continue
      },
      onLeave = {
        case _ ⇒
          stack.pop()
          VisitorCommand.Continue
      }
    ))

    result.result()
  }

  private def findByLocation[T](visitorStack: ValidatorStack[ast.AstNode], node: ast.AstNode, directives: Seq[GenericDirectiveResolver[T]]) =
    directives.filter(d ⇒ d.locations.isEmpty || KnownDirectives.getLocation(node, visitorStack.head(1)).fold(false)(l ⇒ d.locations contains l._1))
}
