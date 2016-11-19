package sangria.validation

import sangria.ast
import sangria.ast.{AstVisitorCommand, AstVisitor, FragmentDefinition}
import sangria.ast.AstVisitorCommand._
import sangria.parser.SourceMapper
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.introspection.{SchemaMetaField, TypeMetaField, TypeNameMetaField}
import sangria.validation.rules._
import scala.collection.concurrent.TrieMap
import scala.collection.mutable.{Set ⇒ MutableSet, Map ⇒ MutableMap, ListBuffer}

trait QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): Vector[Violation]
}

object QueryValidator {
  val allRules: List[ValidationRule] = List(
    new ArgumentsOfCorrectType,
    new DefaultValuesOfCorrectType,
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
    new VariablesInAllowedPosition
  )

  val empty = new QueryValidator {
    def validateQuery(schema: Schema[_, _], queryAst: ast.Document): Vector[Violation] = Vector.empty
  }

  val default = new RuleBasedQueryValidator(allRules)
}

class RuleBasedQueryValidator(rules: List[ValidationRule]) extends QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): Vector[Violation] = {
    val ctx = new ValidationContext(schema, queryAst, new TypeInfo(schema))

    validateUsingRules(queryAst, ctx, rules map (_ visitor ctx), true)

    ctx.violations
  }

  def validateUsingRules(queryAst: ast.AstNode, ctx: ValidationContext, visitors: List[ValidationRule#AstValidatingVisitor], topLevel: Boolean): Unit = AstVisitor.visitAst(
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
}

class ValidationContext(val schema: Schema[_, _], val doc: ast.Document, val typeInfo: TypeInfo) {
  // Using mutable data-structures and mutability to minimize validation footprint

  import ValidationContext.VariableUsage

  private val errors = ListBuffer[Violation]()

  val ignoredVisitors = MutableSet[ValidationRule#AstValidatingVisitor]()
  val skips = MutableMap[ValidationRule#AstValidatingVisitor, ast.AstNode]()

  lazy val fragments = doc.definitions
    .collect{case frDef: FragmentDefinition ⇒ frDef}
    .groupBy(_.name)
    .mapValues(_.head)

  private val fragmentSpreadsCache = TrieMap[Int, List[ast.FragmentSpread]]()
  private val recursivelyReferencedFragmentsCache = TrieMap[Int, List[ast.FragmentDefinition]]()
  private val variableUsages = TrieMap[Int, List[VariableUsage]]()
  private val recursiveVariableUsages = TrieMap[Int, List[VariableUsage]]()

  def getFragmentSpreads(astNode: ast.SelectionContainer) =
    fragmentSpreadsCache.getOrElseUpdate(astNode.cacheKeyHash, {
      val spreads = ListBuffer[ast.FragmentSpread]()
      val setsToVisit = ValidatorStack.empty[List[ast.Selection]]

      setsToVisit.push(astNode.selections)

      while (setsToVisit.nonEmpty) {
        val set = setsToVisit.pop()

        set.foreach {
          case fs: ast.FragmentSpread ⇒
            spreads += fs
          case cont: ast.SelectionContainer ⇒
            setsToVisit push cont.selections
        }
      }

      spreads.toList
    })

  def getRecursivelyReferencedFragments(operation: ast.OperationDefinition) =
    recursivelyReferencedFragmentsCache.getOrElseUpdate(operation.cacheKeyHash, {
      val frags = ListBuffer[ast.FragmentDefinition]()
      val collectedNames = MutableSet[String]()
      val nodesToVisit = ValidatorStack.empty[ast.SelectionContainer]

      nodesToVisit.push(operation)

      while (nodesToVisit.nonEmpty) {
        val node = nodesToVisit.pop()
        val spreads = getFragmentSpreads(node)

        spreads.foreach { spread ⇒
          val fragName = spread.name

          if (!collectedNames.contains(fragName)) {
            collectedNames += fragName

            fragments.get(fragName) match {
              case Some(frag) ⇒
                frags += frag
                nodesToVisit.push(frag)
              case None ⇒ // do nothing
            }
          }
        }
      }

      frags.toList
    })

  def getVariableUsages(astNode: ast.SelectionContainer) =
    variableUsages.getOrElseUpdate(astNode.cacheKeyHash, {
      val usages = ListBuffer[VariableUsage]()
      val typeInfo = new TypeInfo(schema)

      AstVisitor.visitAst(
        doc = astNode,
        onEnter = node ⇒ {
          typeInfo.enter(node)

          node match {
            case _: ast.VariableDefinition ⇒
              Skip
            case vv: ast.VariableValue ⇒
              usages += VariableUsage(vv, typeInfo.inputType)
              Continue
            case _ ⇒
              Continue
          }
        },
        onLeave = node ⇒ {
          typeInfo.leave(node)
          Continue
        }
      )

      usages.toList
    })

  def getRecursiveVariableUsages(operation: ast.OperationDefinition) =
    recursiveVariableUsages.getOrElseUpdate(operation.cacheKeyHash,
      getRecursivelyReferencedFragments(operation).foldLeft(getVariableUsages(operation)) {
        case (acc, fragment) ⇒ acc ++ getVariableUsages(fragment)
      })

  def validVisitor(visitor: ValidationRule#AstValidatingVisitor) =
    !ignoredVisitors.contains(visitor) && !skips.contains(visitor)

  def sourceMapper = doc.sourceMapper

  def addViolation(v: Violation) = errors += v
  def addViolations(vs: Vector[Violation]) = errors ++= vs

  def violations = errors.toVector
}

object ValidationContext {
  case class VariableUsage(node: ast.VariableValue, tpe: Option[InputType[_]])

  def isValidLiteralValue(tpe: InputType[_], value: ast.Value, sourceMapper: Option[SourceMapper]): Vector[Violation] = (tpe, value) match {
    case (_, _: ast.VariableValue) ⇒ Vector.empty
    case (OptionInputType(ofType), _: ast.NullValue) ⇒ Vector.empty
    case (OptionInputType(ofType), v) ⇒
      isValidLiteralValue(ofType, v, sourceMapper)
    case (ListInputType(ofType), ast.ListValue(values, _, pos)) ⇒
      values.zipWithIndex.toVector.flatMap {
        case (elem, idx) ⇒ isValidLiteralValue(ofType, elem, sourceMapper) map (ListValueViolation(idx, _, sourceMapper, pos.toList))
      }
    case (ListInputType(ofType), v) ⇒
      isValidLiteralValue(ofType, v, sourceMapper) map (ListValueViolation(0, _, sourceMapper, v.position.toList))
    case (io: InputObjectType[_], ast.ObjectValue(fields, _, pos)) ⇒
      val unknownFields = fields.toVector.collect {
        case f if !io.fieldsByName.contains(f.name) ⇒
          UnknownInputObjectFieldViolation(SchemaRenderer.renderTypeName(io, true), f.name, sourceMapper, f.position.toList)
      }

      if (unknownFields.nonEmpty) unknownFields
      else {
        io.fields.toVector.flatMap { field ⇒
          val astField = fields.find(_.name == field.name)

          (astField, field.fieldType) match {
            case (None, _: OptionInputType[_]) ⇒
              Vector.empty
            case (None, t) ⇒
              Vector(NotNullInputObjectFieldMissingViolation(SchemaRenderer.renderTypeName(t, true), field.name, sourceMapper, pos.toList))
            case (Some(af), _) ⇒
              isValidLiteralValue(field.fieldType, af.value, sourceMapper) map (MapValueViolation(field.name, _, sourceMapper, af.position.toList))
          }
        }
      }
    case (io: InputObjectType[_], v) ⇒
      Vector(InputObjectIsOfWrongTypeMissingViolation(SchemaRenderer.renderTypeName(io, true), sourceMapper, v.position.toList))
    case (s: ScalarType[_], v) ⇒
      s.coerceInput(v) match {
        case Left(violation) ⇒ Vector(violation)
        case _ ⇒ Vector.empty
      }
    case (enum: EnumType[_], v) ⇒
      enum.coerceInput(v) match {
        case Left(violation) ⇒ Vector(violation)
        case _ ⇒ Vector.empty
      }
  }
}

class TypeInfo(schema: Schema[_, _]) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val typeStack: ValidatorStack[Option[Type]] = ValidatorStack.empty
  private val parentTypeStack: ValidatorStack[Option[CompositeType[_]]] = ValidatorStack.empty
  private val inputTypeStack: ValidatorStack[Option[InputType[_]]] = ValidatorStack.empty
  private val fieldDefStack: ValidatorStack[Option[Field[_, _]]] = ValidatorStack.empty
  private val ancestorStack: ValidatorStack[ast.AstNode] = ValidatorStack.empty

  var directive: Option[Directive] = None
  var argument: Option[Argument[_]] = None

  def tpe = typeStack.headOption.flatten
  def previousParentType = parentTypeStack.headOption(1).flatten
  def parentType = parentTypeStack.headOption.flatten
  def inputType = inputTypeStack.headOption.flatten
  def fieldDef = fieldDefStack.headOption.flatten
  def ancestors: Seq[ast.AstNode] = ancestorStack.toSeq

  def enter(node: ast.AstNode) = {
    ancestorStack push node

    node match {
      case f: ast.Field ⇒
        val parent = parentType
        val fieldDef = parent flatMap (getFieldDef(_, f))
        val fieldType = fieldDef map (_.fieldType)

        fieldDefStack push fieldDef
        typeStack push fieldType

        pushParent()
      case ast.Directive(name, _, _, _) ⇒
        directive = schema.directivesByName get name
      case ast.OperationDefinition(ast.OperationType.Query, _, _, _, _, _, _, _) ⇒
        typeStack push Some(schema.query)
        pushParent()
      case ast.OperationDefinition(ast.OperationType.Mutation, _, _, _, _, _, _, _) ⇒
        typeStack push schema.mutation
        pushParent()
      case ast.OperationDefinition(ast.OperationType.Subscription, _, _, _, _, _, _, _) ⇒
        typeStack push schema.subscription
        pushParent()
      case fd: ast.FragmentDefinition ⇒
        typeStack.push(schema.allTypes get fd.typeCondition.name)
        pushParent()
      case ifd: ast.InlineFragment ⇒
        typeStack.push(ifd.typeCondition.fold(tpe)(schema.allTypes get _.name))
        pushParent()
      case vd: ast.VariableDefinition ⇒
        inputTypeStack push schema.getInputType(vd.tpe)
      case a: ast.Argument ⇒
        argument = directive orElse fieldDef flatMap { withArgs ⇒
          withArgs.arguments find (_.name == a.name)
        }
        inputTypeStack push argument.map(_.inputValueType)
      case ast.ListValue(values, _, _) ⇒
        inputType match {
          case Some(it) ⇒ getNotNullType(it) match {
            case it: ListInputType[_] ⇒ inputTypeStack push Some(it.ofType)
            case _ ⇒ inputTypeStack push None
          }
          case None ⇒ inputTypeStack push None
        }
      case ast.ObjectField(name, value, _, _) ⇒
        val fieldType = inputType flatMap (it ⇒ getNamedType(it) match {
          case obj: InputObjectType[_] ⇒ obj.fieldsByName.get(name) map (_.inputValueType)
          case _ ⇒ None
        })

        inputTypeStack push fieldType
      case _ ⇒ // ignore
    }
  }

  def pushParent(): Unit = {
    tpe match {
      case Some(some) ⇒ getNamedType(some) match {
        case comp: CompositeType[_] ⇒ parentTypeStack push Some(comp)
        case _ ⇒ parentTypeStack push None
      }
      case _ ⇒ parentTypeStack push None
    }
  }

  def leave(node: ast.AstNode) = {
    node match {
      case f: ast.Field ⇒
        fieldDefStack.pop()
        typeStack.pop()
        parentTypeStack.pop()
      case ast.Directive(name, _, _, _) ⇒
        directive = None
      case ast.OperationDefinition(ast.OperationType.Query, _, _, _, _, _, _, _) ⇒
        typeStack.pop()
        parentTypeStack.pop()
      case ast.OperationDefinition(ast.OperationType.Mutation, _, _, _, _, _, _, _) ⇒
        typeStack.pop()
        parentTypeStack.pop()
      case ast.OperationDefinition(ast.OperationType.Subscription, _, _, _, _, _, _, _) ⇒
        typeStack.pop()
        parentTypeStack.pop()
      case fd: ast.FragmentDefinition ⇒
        typeStack.pop()
        parentTypeStack.pop()
      case fd: ast.InlineFragment ⇒
        typeStack.pop()
        parentTypeStack.pop()
      case vd: ast.VariableDefinition ⇒
        inputTypeStack.pop()
      case a: ast.Argument ⇒
        argument = None
        inputTypeStack.pop()
      case ast.ListValue(values, _, _) ⇒
        inputTypeStack.pop()
      case ast.ObjectField(name, value, _, _) ⇒
        inputTypeStack.pop()
      case _ ⇒ // ignore
    }

    ancestorStack.pop()
  }

  def getNamedType(it: Type): Type with Named = it match {
    case OptionInputType(ofType) ⇒ getNamedType(ofType)
    case OptionType(ofType) ⇒ getNamedType(ofType)
    case ListInputType(ofType) ⇒ getNamedType(ofType)
    case ListType(ofType) ⇒ getNamedType(ofType)
    case n: Named ⇒ n
    case t ⇒ throw new IllegalStateException("Expected named type, but got: " + t)
  }

  def getNotNullType(it: InputType[_]): InputType[_] = it match {
    case OptionInputType(ofType) ⇒ ofType
    case n ⇒ n
  }

  def getFieldDef(parent: CompositeType[_], astField: ast.Field): Option[Field[_, _]] = {
    if (astField.name == SchemaMetaField.name && schema.query.name == parent.name)
      Some(SchemaMetaField)
    else if (astField.name == TypeMetaField.name && schema.query.name == parent.name)
      Some(TypeMetaField)
    else if (astField.name == TypeNameMetaField.name)
      Some(TypeNameMetaField)
    else parent match {
      case o: ObjectLikeType[_, _] ⇒ o.getField(schema, astField.name).headOption
      case _ ⇒ None
    }
  }
}

class ValidatorStack[T] {
  private val stack: ListBuffer[T] = ListBuffer.empty
    
  def push(element: T): Unit = stack.prepend(element)
  def pop(): T = stack.remove(0)
  def headOption = stack.headOption
  def headOption(toDrop: Int) = stack.drop(toDrop).headOption
  def nonEmpty = stack.nonEmpty
  def toSeq: Seq[T] = stack
}

object ValidatorStack {
  def empty[T] = new ValidatorStack[T]
}