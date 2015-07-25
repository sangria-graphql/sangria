package sangria.validation

import sangria.ast
import sangria.ast.{AstVisitor, FragmentDefinition}
import sangria.ast.AstVisitorCommand._
import sangria.schema._
import sangria.introspection.{SchemaMetaField, TypeMetaField, TypeNameMetaField}
import scala.collection.mutable.{Stack => MutableStack, ListBuffer}

trait QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): List[Violation]
}

object QueryValidator {
  val allRules: List[ValidationRule] = Nil

  val empty = new QueryValidator {
    def validateQuery(schema: Schema[_, _], queryAst: ast.Document): List[Violation] = Nil
  }

  val default = new RuleBasedQueryValidator(allRules)
}

class RuleBasedQueryValidator(rules: List[ValidationRule]) extends QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: ast.Document): List[Violation] = {
    val ctx = new ValidationContext(schema, queryAst, new TypeInfo(schema))

    validateUsingRules(queryAst, ctx, rules map (_ visitor ctx))

    ctx.violations
  }

  def validateUsingRules(queryAst: ast.Document, ctx: ValidationContext, visitors: List[ValidationRule#AstValidatingVisitor]) = AstVisitor.visitAst(
    doc = queryAst,
    onEnter = node => {
      ctx.typeInfo.enter(node)
      visitors foreach { visitor =>
        if (visitor.onEnter.isDefinedAt(node)) {
          visitor.onEnter(node) match {
            case Left(violation) =>
              ctx.addViolation(violation)
            case _ => // todo
          }
        }
      }

      Continue
    },
    onLeave = node => {
      visitors foreach { visitor =>
        if (visitor.onLeave.isDefinedAt(node)) {
          visitor.onLeave(node) match {
            case Left(violation) =>
              ctx.addViolation(violation)
            case _ => // todo
          }
        }
      }

      ctx.typeInfo.leave(node)
      Continue
    }
  )

}

class ValidationContext(val schema: Schema[_, _], val doc: ast.Document, val typeInfo: TypeInfo) {
  private val errors = ListBuffer[Violation]()

  lazy val fragments = doc.definitions
    .collect{case frDef: FragmentDefinition => frDef}
    .groupBy(_.name)
    .mapValues(_.head)

  def sourceMapper = doc.sourceMapper

  def addViolation(v: Violation) = errors += v

  def violations = errors.toList
}

object ValidationContext {
  def isValidLiteralValue(tpe: InputType[_], value: ast.Value): Boolean = (tpe, value) match {
    case (_, _: ast.VariableValue) => true
    case (OptionInputType(ofType), v) =>
      isValidLiteralValue(ofType, v)
    case (ListInputType(ofType), ast.ArrayValue(values, _)) =>
      values.forall(isValidLiteralValue(ofType, _))
    case (ListInputType(ofType), v) =>
      isValidLiteralValue(ofType, v)
    case (io: InputObjectType[_], ast.ObjectValue(fields, _)) =>
      fields.forall(f => io.fieldsByName contains f.name) && {
        io.fields.forall { field =>
          val astField = fields.find(_.name == field.name)

          (astField, field.fieldType) match {
            case (None, _: OptionInputType[_]) => true
            case (None, _) => false
            case (Some(af), _) => isValidLiteralValue(field.fieldType, af.value)
          }
        }
      }
    case (io: InputObjectType[_], _) => false
    case (s: ScalarType[_], v) =>
      s.coerceInput(v).isRight
    case (enum: EnumType[_], v) =>
      enum.coerceInput(v).isRight

  }
}

class TypeInfo(schema: Schema[_, _]) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val typeStack: MutableStack[Option[Type]] = MutableStack()
  private val parentTypeStack: MutableStack[Option[CompositeType[_]]] = MutableStack()
  private val inputTypeStack: MutableStack[Option[InputType[_]]] = MutableStack()
  private val fieldDefStack: MutableStack[Option[Field[_, _]]] = MutableStack()
  private val ancestorStack: MutableStack[ast.AstNode] = MutableStack()

  var directive: Option[Directive] = None
  var argument: Option[Argument[_]] = None

  def tpe = typeStack.headOption.flatten
  def previousParentType = parentTypeStack.drop(1).headOption.flatten
  def parentType = parentTypeStack.headOption.flatten
  def inputType = inputTypeStack.headOption.flatten
  def fieldDef = fieldDefStack.headOption.flatten
  def ancestors: Seq[ast.AstNode] = ancestorStack

  def enter(node: ast.AstNode) = {
    ancestorStack push node

    node match {
      case f: ast.Field =>
        val parent = parentType
        val fieldDef = parent flatMap (getFieldDef(_, f))
        val fieldType = fieldDef map (_.fieldType)

        fieldDefStack push fieldDef
        typeStack push fieldType

        pushParent()
      case ast.Directive(name, _, _) =>
        directive = schema.directivesByName get name
      case ast.OperationDefinition(ast.OperationType.Query, _, _, _, _, _) =>
        typeStack push Some(schema.query)
        pushParent()
      case ast.OperationDefinition(ast.OperationType.Mutation, _, _, _, _, _) =>
        typeStack push schema.mutation
        pushParent()
      case fd: ast.FragmentDefinition =>
        typeStack.push(schema.allTypes get fd.typeCondition.name)
        pushParent()
      case ifd: ast.InlineFragment =>
        typeStack.push(schema.allTypes get ifd.typeCondition.name)
        pushParent()
      case vd: ast.VariableDefinition =>
        inputTypeStack push schema.getInputType(vd.tpe)
      case a: ast.Argument =>
        argument = directive orElse fieldDef flatMap { withArgs =>
          withArgs.arguments find (_.name == a.name)
        }
        inputTypeStack push argument.map(_.inputValueType)
      case ast.ArrayValue(values, _) =>
        inputType match {
          case Some(it) => getNotNullType(it) match {
            case it: ListInputType[_] => inputTypeStack push Some(it.ofType)
            case _ => inputTypeStack push None
          }
          case None => inputTypeStack push None
        }
      case ast.ObjectField(name, value, _) =>
        val fieldType = inputType flatMap (it => getNamedType(it) match {
          case obj: InputObjectType[_] => obj.fieldsByName.get(name) map (_.inputValueType)
          case _ => None
        })

        inputTypeStack push fieldType
      case _ => // ignore
    }
  }

  def pushParent(): Unit = {
    tpe match {
      case Some(some) => getNamedType(some) match {
        case comp: CompositeType[_] => parentTypeStack push Some(comp)
        case _ => parentTypeStack push None
      }
      case _ => parentTypeStack push None
    }
  }

  def leave(node: ast.AstNode) = {
    node match {
      case f: ast.Field =>
        fieldDefStack.pop()
        typeStack.pop()
        parentTypeStack.pop()
      case ast.Directive(name, _, _) =>
        directive = None
      case ast.OperationDefinition(ast.OperationType.Query, _, _, _, _, _) =>
        typeStack.pop()
        parentTypeStack.pop()
      case ast.OperationDefinition(ast.OperationType.Mutation, _, _, _, _, _) =>
        typeStack.pop()
        parentTypeStack.pop()
      case fd: ast.FragmentDefinition =>
        typeStack.pop()
        parentTypeStack.pop()
      case fd: ast.InlineFragment =>
        typeStack.pop()
        parentTypeStack.pop()
      case vd: ast.VariableDefinition =>
        inputTypeStack.pop()
      case a: ast.Argument =>
        argument = None
        inputTypeStack.pop()
      case ast.ArrayValue(values, _) =>
        inputTypeStack.pop()
      case ast.ObjectField(name, value, _) =>
        inputTypeStack.pop()
      case _ => // ignore
    }

    ancestorStack.pop()
  }

  def getNamedType(it: Type): Type with Named = it match {
    case OptionInputType(ofType) => getNamedType(ofType)
    case OptionType(ofType) => getNamedType(ofType)
    case ListInputType(ofType) => getNamedType(ofType)
    case ListType(ofType) => getNamedType(ofType)
    case n: Named => n
    case t => throw new IllegalStateException("Expected named type, but got: " + t)
  }

  def getNotNullType(it: InputType[_]): InputType[_] = it match {
    case OptionInputType(ofType) => ofType
    case n => n
  }

  def getFieldDef(parent: CompositeType[_], astField: ast.Field): Option[Field[_, _]] = {
    if (astField.name == SchemaMetaField.name && schema.query.name == parent.name)
      Some(SchemaMetaField)
    else if (astField.name == TypeMetaField.name && schema.query.name == parent.name)
      Some(TypeMetaField)
    else if (astField.name == TypeNameMetaField.name)
      Some(TypeNameMetaField)
    else parent match {
      case o: ObjectLikeType[_, _] => o.getField(schema, astField.name)
      case _ => None
    }
  }
}