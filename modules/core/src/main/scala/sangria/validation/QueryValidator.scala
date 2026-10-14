package sangria.validation

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.ast.{AstVisitor, AstVisitorCommand, SourceMapper}
import sangria.execution
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.validation.rules._

import scala.collection.mutable.{ListBuffer, Map => MutableMap, Set => MutableSet}
import scala.reflect.{ClassTag, classTag}

trait QueryValidator {
  def validateQuery(
      schema: Schema[_, _],
      queryAst: ast.Document,
      variableValues: Map[String, execution.VariableValue],
      errorsLimit: Option[Int]): Vector[Violation]
}

object QueryValidator {
  val allRules: List[ValidationRule] = List(
    new ValuesOfCorrectType,
    new ExecutableDefinitions,
    new FieldsOnCorrectType,
    new FragmentsOnCompositeTypes,
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
    new ProvidedRequiredArguments,
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
    new SingleFieldSubscriptions,
    new ExactlyOneOfFieldGiven
  )

  def ruleBased(rules: List[ValidationRule]): RuleBasedQueryValidator =
    new RuleBasedQueryValidator(rules)

  val empty: QueryValidator = new QueryValidator {
    def validateQuery(
        schema: Schema[_, _],
        queryAst: ast.Document,
        variableValues: Map[String, execution.VariableValue],
        errorsLimit: Option[Int]): Vector[Violation] = Vector.empty
  }

  val default: RuleBasedQueryValidator = ruleBased(allRules)
}

object RuleBasedQueryValidator {

  // a distinct instance, compared by reference, that tells a visitor did not match the node
  private[validation] val notMatchedResult: Either[Vector[Violation], AstVisitorCommand.Value] =
    Right(AstVisitorCommand.Continue)

  private[validation] val notMatched
      : ast.AstNode => Either[Vector[Violation], AstVisitorCommand.Value] =
    _ => notMatchedResult

  /** What the dispatch of a set of visitors depends on: their classes and whether their `onEnter`
    * and `onLeave` are the empty function. A class can have both kinds of visitors, for example
    * when a rule only validates under some condition, so the classes alone are not enough.
    */
  private final class VisitorsKey(val classes: Array[Class[_]], val empty: Array[Boolean]) {
    override val hashCode: Int =
      31 * java.util.Arrays.hashCode(classes.asInstanceOf[Array[AnyRef]]) +
        java.util.Arrays.hashCode(empty)

    override def equals(obj: Any): Boolean = obj match {
      case other: VisitorsKey =>
        java.util.Arrays.equals(
          classes.asInstanceOf[Array[AnyRef]],
          other.classes.asInstanceOf[Array[AnyRef]]) && java.util.Arrays.equals(empty, other.empty)
      case _ => false
    }
  }

  // The dispatch only depends on the kinds of nodes the visitors handle. For the visitors of a class
  // they are the same, except that a visitor can have no `onEnter`/`onLeave` (see `VisitorsKey`);
  // see `AstValidatingVisitor.onEnterNodes`. So the dispatch is shared by the requests.
  // shortcut: one entry per distinct set of rules, never evicted; fine unless rule sets are
  // generated dynamically
  private val dispatches =
    new java.util.concurrent.ConcurrentHashMap[VisitorsKey, (Dispatch, Dispatch)]

  private val noDispatch = (new Dispatch(Array.empty), new Dispatch(Array.empty)) // by reference

  private[validation] def dispatchFor(
      visitors: Array[ValidationRule#AstValidatingVisitor]): (Dispatch, Dispatch) = {
    val empty = new Array[Boolean](visitors.length * 2)
    var i = 0
    while (i < visitors.length) {
      empty(2 * i) = visitors(i).onEnter eq PartialFunction.empty
      empty(2 * i + 1) = visitors(i).onLeave eq PartialFunction.empty
      i += 1
    }
    val key = new VisitorsKey(visitors.map(_.getClass: Class[_]), empty)
    val cached = dispatches.getOrDefault(key, noDispatch)
    if (cached ne noDispatch) cached
    else {
      val computed =
        (new Dispatch(visitors.map(_.onEnterNodes)), new Dispatch(visitors.map(_.onLeaveNodes)))
      dispatches.putIfAbsent(key, computed)
      computed
    }
  }

  /** Finds, for a node, the visitors that can match it. `nodes` are the declared node classes of
    * each visitor, `None` meaning any node. The result is computed once per class of node.
    */
  private[validation] final class Dispatch(nodes: Array[Option[Vector[Class[_]]]]) {
    private val cache = new java.util.concurrent.ConcurrentHashMap[Class[_], Array[Int]]
    private val notComputed = new Array[Int](0) // compared by reference

    // With so few visitors, looking up the class of a node costs more than matching the visitors
    private val all: Array[Int] =
      if (nodes.length <= 2) nodes.indices.toArray else notComputed

    def visitorsFor(node: ast.AstNode): Array[Int] =
      if (all ne notComputed) all else visitorsForClass(node.getClass)

    private def visitorsForClass(cls: Class[_]): Array[Int] = {
      val cached = cache.getOrDefault(cls, notComputed)
      if (cached ne notComputed) cached
      else {
        val result =
          nodes.indices.filter(i => nodes(i).forall(_.exists(_.isAssignableFrom(cls)))).toArray
        cache.put(cls, result)
        result
      }
    }
  }
}

class RuleBasedQueryValidator(rules: List[ValidationRule]) extends QueryValidator {
  def validateQuery(
      schema: Schema[_, _],
      queryAst: ast.Document,
      variables: Map[String, execution.VariableValue],
      errorsLimit: Option[Int]
  ): Vector[Violation] = {
    val ctx = new ValidationContext(
      schema,
      queryAst,
      queryAst.sourceMapper,
      new TypeInfo(schema),
      variables,
      errorsLimit)

    validateUsingRules(queryAst, ctx, rules.map(_.visitor(ctx)), topLevel = true)

    ctx.violations
  }

  def validateInputDocument(
      schema: Schema[_, _],
      doc: ast.InputDocument,
      inputTypeName: String,
      variables: Map[String, execution.VariableValue]
  ): Vector[Violation] =
    schema.getInputType(ast.NamedType(inputTypeName)) match {
      case Some(it) => validateInputDocument(schema, doc, it, variables)
      case None =>
        throw new IllegalStateException(
          s"Can't find input type '$inputTypeName' in the schema. Known input types are: ${schema.inputTypes.keys.toVector.sorted
              .mkString(", ")}.")
    }

  def validateInputDocument(
      schema: Schema[_, _],
      doc: ast.InputDocument,
      inputType: InputType[_],
      variables: Map[String, execution.VariableValue]
  ): Vector[Violation] = {
    val typeInfo = new TypeInfo(schema, Some(inputType))

    val ctx = ValidationContext(
      schema,
      ast.Document.emptyStub,
      doc.sourceMapper,
      typeInfo,
      variables
    )

    validateUsingRules(doc, ctx, rules.map(_.visitor(ctx)), topLevel = true)

    ctx.violations
  }

  def validateUsingRules(
      queryAst: ast.AstNode,
      ctx: ValidationContext,
      visitors: List[ValidationRule#AstValidatingVisitor],
      topLevel: Boolean): Unit = {
    val visitorsArray = visitors.toArray
    val (enterDispatch, leaveDispatch) = RuleBasedQueryValidator.dispatchFor(visitorsArray)

    // Which visitors are ignored (they broke out) or skipping a node, by index of the visitor.
    // Arrays are much cheaper than the sets and maps of the context, which hash the visitors.
    val ignored = new Array[Boolean](visitorsArray.length)
    val skipping = new Array[Boolean](visitorsArray.length)
    val skippedNode = new Array[ast.AstNode](visitorsArray.length) // only read while skipping
    var skippingCount = 0

    def handle(
        i: Int,
        node: ast.AstNode,
        result: Either[Vector[Violation], AstVisitorCommand.Value]): Unit =
      result match {
        case Left(violations) => ctx.addViolations(violations)
        case AstVisitorCommand.RightSkip =>
          if (!skipping(i)) skippingCount += 1
          skipping(i) = true
          skippedNode(i) = node
        case Right(Break) => ignored(i) = true
        case _ => // do nothing
      }

    AstVisitor.visitAstRecursive(
      doc = queryAst,
      onEnter = node => {
        ctx.typeInfo.enter(node)

        // only the visitors that can match this kind of node
        val applicable = enterDispatch.visitorsFor(node)
        var j = 0
        while (j < applicable.length) {
          val i = applicable(j)
          if (!ignored(i) && !skipping(i)) {
            // applyOrElse matches the node only once, unlike isDefinedAt followed by apply
            val result =
              visitorsArray(i).onEnter.applyOrElse(node, RuleBasedQueryValidator.notMatched)
            if (result ne RuleBasedQueryValidator.notMatchedResult) handle(i, node, result)
          }
          j += 1
        }

        Continue
      },
      onLeave = node => {
        val applicable = leaveDispatch.visitorsFor(node)
        var j = 0
        while (j < applicable.length) {
          val i = applicable(j)
          if (!ignored(i) && !skipping(i)) {
            val result =
              visitorsArray(i).onLeave.applyOrElse(node, RuleBasedQueryValidator.notMatched)
            if (result ne RuleBasedQueryValidator.notMatchedResult) handle(i, node, result)
          }
          j += 1
        }

        // a visitor that skipped this node is active again once the node is left
        if (skippingCount > 0) {
          var i = 0
          while (i < visitorsArray.length) {
            if (skipping(i) && (skippedNode(i) eq node)) {
              skipping(i) = false
              skippingCount -= 1
            }
            i += 1
          }
        }

        ctx.typeInfo.leave(node)
        Continue
      }
    )
  }

  def handleResult(
      ctx: ValidationContext,
      node: ast.AstNode,
      visitor: ValidationRule#AstValidatingVisitor,
      visitRes: Either[Vector[Violation], AstVisitorCommand.Value]) =
    visitRes match {
      case Left(violation) =>
        ctx.addViolations(violation)
      case AstVisitorCommand.RightSkip =>
        ctx.skips(visitor) = node
      case Right(Break) =>
        ctx.ignoredVisitors += visitor
      case _ => // do nothing
    }

  def withoutValidation[T: ClassTag] = {
    val cls = classTag[T].runtimeClass
    val newRules = rules.filterNot(r => cls.isAssignableFrom(r.getClass))

    new RuleBasedQueryValidator(newRules)
  }
}

class ValidationContext(
    val schema: Schema[_, _],
    val doc: ast.Document,
    val sourceMapper: Option[SourceMapper],
    val typeInfo: TypeInfo,
    val variables: Map[String, execution.VariableValue],
    errorsLimit: Option[Int]) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val errors = ListBuffer[Violation]()

  val documentAnalyzer = SchemaBasedDocumentAnalyzer(schema, doc)

  val ignoredVisitors = MutableSet[ValidationRule#AstValidatingVisitor]()
  val skips = MutableMap[ValidationRule#AstValidatingVisitor, ast.AstNode]()

  def validVisitor(visitor: ValidationRule#AstValidatingVisitor) =
    (ignoredVisitors.isEmpty || !ignoredVisitors.contains(visitor)) &&
      (skips.isEmpty || !skips.contains(visitor))

  def addViolation(v: Violation) = errorsLimit.fold(errors += v) { limit =>
    if (errors.length >= limit) errors
    else errors += v
  }
  def addViolations(vs: Vector[Violation]) = errorsLimit.fold(errors ++= vs) { limit =>
    vs.foreach(addViolation)
    errors
  }

  def violations = errors.toVector
}

object ValidationContext {
  def apply(
      schema: Schema[_, _],
      doc: ast.Document,
      sourceMapper: Option[SourceMapper],
      typeInfo: TypeInfo,
      variables: Map[String, execution.VariableValue]
  ): ValidationContext =
    new ValidationContext(schema, doc, sourceMapper, typeInfo, variables, None)

  @deprecated(
    "The validations are now implemented as a part of `ValuesOfCorrectType` validation.",
    "1.4.0")
  def isValidLiteralValue(
      tpe: InputType[_],
      value: ast.Value,
      sourceMapper: Option[SourceMapper]): Vector[Violation] = (tpe, value) match {
    case (_, _: ast.VariableValue) => Vector.empty
    case (OptionInputType(ofType), _: ast.NullValue) => Vector.empty
    case (OptionInputType(ofType), v) =>
      isValidLiteralValue(ofType, v, sourceMapper)
    case (ListInputType(ofType), ast.ListValue(values, _, pos)) =>
      values.zipWithIndex.flatMap { case (elem, idx) =>
        isValidLiteralValue(ofType, elem, sourceMapper).map(
          ListValueViolation(idx, _, sourceMapper, pos.toList))
      }
    case (ListInputType(ofType), v) =>
      isValidLiteralValue(ofType, v, sourceMapper).map(
        ListValueViolation(0, _, sourceMapper, v.location.toList))
    case (io: InputObjectType[_], ast.ObjectValue(fields, _, pos)) =>
      val unknownFields = fields.collect {
        case f if !io.fieldsByName.contains(f.name) =>
          UnknownInputObjectFieldViolation(
            SchemaRenderer.renderTypeName(io, true),
            f.name,
            sourceMapper,
            f.location.toList)
      }

      val fieldViolations =
        io.fields.toVector.flatMap { field =>
          val astField = fields.find(_.name == field.name)

          (astField, field.fieldType) match {
            case (None, _: OptionInputType[_]) =>
              Vector.empty
            case (None, t) =>
              Vector(
                NotNullInputObjectFieldMissingViolation(
                  io.name,
                  field.name,
                  SchemaRenderer.renderTypeName(t),
                  sourceMapper,
                  pos.toList))
            case (Some(af), _) =>
              isValidLiteralValue(field.fieldType, af.value, sourceMapper).map(
                MapValueViolation(field.name, _, sourceMapper, af.location.toList))
          }
        }

      unknownFields ++ fieldViolations
    case (io: InputObjectType[_], v) =>
      Vector(
        InputObjectIsOfWrongTypeMissingViolation(
          SchemaRenderer.renderTypeName(io, topLevel = true),
          sourceMapper,
          v.location.toList))
    case (s: ScalarType[_], v) =>
      s.coerceInput(v) match {
        case Left(violation) => Vector(violation)
        case _ => Vector.empty
      }
    case (s: ScalarAlias[_, _], v) =>
      s.aliasFor.coerceInput(v) match {
        case Left(violation) => Vector(violation)
        case Right(v) =>
          s.fromScalar(v) match {
            case Left(violation) => Vector(violation)
            case _ => Vector.empty
          }
      }
    case (enumT: EnumType[_], v) =>
      enumT.coerceInput(v) match {
        case Left(violation) => Vector(violation)
        case _ => Vector.empty
      }
  }
}
