package sangria.validation

import sangria.ast
import sangria.ast.{AstVisitor, AstVisitorCommand}
import sangria.ast.AstVisitorCommand._
import sangria.parser.SourceMapper
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.validation.rules._

import scala.collection.mutable.{ListBuffer, Map => MutableMap, Set => MutableSet}
import scala.reflect.{ClassTag, classTag}

trait QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): Vector[Violation]
}

object QueryValidator {
  val allRules: List[ValidationRule] = List(
    new ValuesOfCorrectType,
    new VariablesDefaultValueAllowed,
    new ExecutableDefinitions,
    new FieldsOnCorrectType,
    new FragmentsOnCompositeType,
    new KnownArgumentNames,
    new KnownDirectives,
    new KnownFragmentNames,
    new KnownTypeNames,
    new LoneAnonymousOperation,
    new NoFragmentCycles,
    new NoUndefinedVariables,
    new NoUnusedFragments,
    new NoUnusedVariables,
    new OverlappingFieldsCanBeMerged,
    new PossibleFragmentSpreads,
    new ProvidedNonNullArguments,
    new ScalarLeafs,
    new UniqueArgumentNames,
    new UniqueDirectivesPerLocation,
    new UniqueFragmentNames,
    new UniqueInputFieldNames,
    new UniqueOperationNames,
    new UniqueVariableNames,
    new VariablesAreInputTypes,
    new VariablesInAllowedPosition,
    new InputDocumentNonConflictingVariableInference,
    new SingleFieldSubscriptions
  )

  def ruleBased(rules: List[ValidationRule]) = new RuleBasedQueryValidator(rules)

  val empty = new QueryValidator {
    def validateQuery(schema: Schema[_, _], queryAst: ast.Document): Vector[Violation] = Vector.empty
  }

  val default: RuleBasedQueryValidator = ruleBased(allRules)
}

class RuleBasedQueryValidator(rules: List[ValidationRule]) extends QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): Vector[Violation] = {
    val ctx = new ValidationContext(schema, queryAst, queryAst.sourceMapper, new TypeInfo(schema))

    validateUsingRules(queryAst, ctx, rules map (_ visitor ctx), topLevel = true)

    ctx.violations
  }

  def validateInputDocument(schema: Schema[_, _], doc: ast.InputDocument, inputTypeName: String): Vector[Violation] =
    schema.getInputType(ast.NamedType(inputTypeName)) match {
      case Some(it) ⇒ validateInputDocument(schema, doc, it)
      case None ⇒ throw new IllegalStateException(s"Can't find input type '$inputTypeName' in the schema. Known input types are: ${schema.inputTypes.keys.toVector.sorted mkString ", "}.")
    }

  def validateInputDocument(schema: Schema[_, _], doc: ast.InputDocument, inputType: InputType[_]): Vector[Violation] = {
    val typeInfo = new TypeInfo(schema, Some(inputType))

    val ctx = new ValidationContext(schema, ast.Document.emptyStub, doc.sourceMapper, typeInfo)

    validateUsingRules(doc, ctx, rules map (_ visitor ctx), topLevel = true)

    ctx.violations
  }

  def validateUsingRules(queryAst: ast.AstNode, ctx: ValidationContext, visitors: List[ValidationRule#AstValidatingVisitor], topLevel: Boolean): Unit = AstVisitor.visitAstRecursive(
    doc = queryAst,
    onEnter = node ⇒ {
      ctx.typeInfo.enter(node)

      visitors foreach { visitor ⇒
        if (ctx.validVisitor(visitor) && visitor.onEnter.isDefinedAt(node)) {
          handleResult(ctx, node, visitor, visitor.onEnter(node))
        }
      }

      Continue
    },
    onLeave = node ⇒ {
      visitors foreach { visitor ⇒
        if (visitor.onLeave.isDefinedAt(node) && ctx.validVisitor(visitor)) {
          handleResult(ctx, node, visitor, visitor.onLeave(node))
        }

        if (ctx.skips.get(visitor).exists(_ eq node))
          ctx.skips.remove(visitor)
      }

      ctx.typeInfo.leave(node)
      Continue
    }
  )

  def handleResult(ctx: ValidationContext, node: ast.AstNode, visitor: ValidationRule#AstValidatingVisitor, visitRes: Either[Vector[Violation], AstVisitorCommand.Value]) =
    visitRes match {
      case Left(violation) ⇒
        ctx.addViolations(violation)
      case AstVisitorCommand.RightSkip ⇒
        ctx.skips(visitor) = node
      case Right(Break) ⇒
        ctx.ignoredVisitors += visitor
      case _ ⇒ // do nothing
    }

  def withoutValidation[T : ClassTag] = {
    val cls = classTag[T].runtimeClass
    val newRules = rules.filterNot(r ⇒ cls.isAssignableFrom(r.getClass))

    new RuleBasedQueryValidator(newRules)
  }
}

class ValidationContext(val schema: Schema[_, _], val doc: ast.Document, val sourceMapper: Option[SourceMapper], val typeInfo: TypeInfo) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val errors = ListBuffer[Violation]()

  val documentAnalyzer = SchemaBasedDocumentAnalyzer(schema, doc)

  val ignoredVisitors = MutableSet[ValidationRule#AstValidatingVisitor]()
  val skips = MutableMap[ValidationRule#AstValidatingVisitor, ast.AstNode]()

  def validVisitor(visitor: ValidationRule#AstValidatingVisitor) =
    !ignoredVisitors.contains(visitor) && !skips.contains(visitor)

  def addViolation(v: Violation) = errors += v
  def addViolations(vs: Vector[Violation]) = errors ++= vs

  def violations = errors.toVector
}

object ValidationContext {
  @deprecated("The validations are now implemented as a part of `ValuesOfCorrectType` validation.", "1.4.0")
  def isValidLiteralValue(tpe: InputType[_], value: ast.Value, sourceMapper: Option[SourceMapper]): Vector[Violation] = (tpe, value) match {
    case (_, _: ast.VariableValue) ⇒ Vector.empty
    case (OptionInputType(ofType), _: ast.NullValue) ⇒ Vector.empty
    case (OptionInputType(ofType), v) ⇒
      isValidLiteralValue(ofType, v, sourceMapper)
    case (ListInputType(ofType), ast.ListValue(values, _, pos)) ⇒
      values.zipWithIndex.flatMap {
        case (elem, idx) ⇒ isValidLiteralValue(ofType, elem, sourceMapper) map (ListValueViolation(idx, _, sourceMapper, pos.toList))
      }
    case (ListInputType(ofType), v) ⇒
      isValidLiteralValue(ofType, v, sourceMapper) map (ListValueViolation(0, _, sourceMapper, v.location.toList))
    case (io: InputObjectType[_], ast.ObjectValue(fields, _, pos)) ⇒
      val unknownFields = fields.collect {
        case f if !io.fieldsByName.contains(f.name) ⇒
          UnknownInputObjectFieldViolation(SchemaRenderer.renderTypeName(io, true), f.name, sourceMapper, f.location.toList)
      }

      val fieldViolations =
        io.fields.toVector.flatMap { field ⇒
          val astField = fields.find(_.name == field.name)

          (astField, field.fieldType) match {
            case (None, _: OptionInputType[_]) ⇒
              Vector.empty
            case (None, t) ⇒
              Vector(NotNullInputObjectFieldMissingViolation(io.name, field.name, SchemaRenderer.renderTypeName(t), sourceMapper, pos.toList))
            case (Some(af), _) ⇒
              isValidLiteralValue(field.fieldType, af.value, sourceMapper) map (MapValueViolation(field.name, _, sourceMapper, af.location.toList))
          }
        }

      unknownFields ++ fieldViolations
    case (io: InputObjectType[_], v) ⇒
      Vector(InputObjectIsOfWrongTypeMissingViolation(SchemaRenderer.renderTypeName(io, true), sourceMapper, v.location.toList))
    case (s: ScalarType[_], v) ⇒
      s.coerceInput(v) match {
        case Left(violation) ⇒ Vector(violation)
        case _ ⇒ Vector.empty
      }
    case (s: ScalarAlias[_, _], v) ⇒
      s.aliasFor.coerceInput(v) match {
        case Left(violation) ⇒ Vector(violation)
        case Right(v) ⇒ s.fromScalar(v) match {
          case Left(violation) ⇒ Vector(violation)
          case _ ⇒ Vector.empty
        }
      }
    case (enum: EnumType[_], v) ⇒
      enum.coerceInput(v) match {
        case Left(violation) ⇒ Vector(violation)
        case _ ⇒ Vector.empty
      }
  }
}