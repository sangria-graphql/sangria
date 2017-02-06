package sangria.validation

import sangria.ast
import sangria.introspection.{SchemaMetaField, TypeMetaField, TypeNameMetaField}
import sangria.schema._
import scala.collection.mutable.{ListBuffer, Set ⇒ MutableSet, Map ⇒ MutableMap}

class TypeInfo(schema: Schema[_, _]) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val typeStack: ValidatorStack[Option[Type]] = ValidatorStack.empty
  private val parentTypeStack: ValidatorStack[Option[CompositeType[_]]] = ValidatorStack.empty
  private val inputTypeStack: ValidatorStack[Option[InputType[_]]] = ValidatorStack.empty
  private val fieldDefStack: ValidatorStack[Option[Field[_, _]]] = ValidatorStack.empty
  private val ancestorStack: ValidatorStack[ast.AstNode] = ValidatorStack.empty

  var directive: Option[Directive] = None
  var enumValue: Option[EnumValue[_]] = None
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
        val fieldType = inputType flatMap (it ⇒ it.namedType match {
          case obj: InputObjectType[_] ⇒ obj.fieldsByName.get(name) map (_.inputValueType)
          case _ ⇒ None
        })

        inputTypeStack push fieldType
      case ast.EnumValue(name, _, _) ⇒
        enumValue = inputType match {
          case Some(it) ⇒ it.namedType match {
            case enum: EnumType[_] ⇒ enum.byName.get(name)
            case _ ⇒ None
          }
          case None ⇒ None
        }
      case _ ⇒ // ignore
    }
  }

  def pushParent(): Unit = {
    tpe match {
      case Some(some) ⇒ some.namedType match {
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
      case ast.ListValue(_, _, _) ⇒
        inputTypeStack.pop()
      case ast.ObjectField(_, _, _, _) ⇒
        inputTypeStack.pop()
      case ast.EnumValue(_, _, _) ⇒
        enumValue = None
      case _ ⇒ // ignore
    }

    ancestorStack.pop()
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
