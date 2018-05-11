package sangria.validation

import sangria.ast
import sangria.introspection.{SchemaMetaField, TypeMetaField, TypeNameMetaField}
import sangria.marshalling.ToInput
import sangria.schema._

class TypeInfo(schema: Schema[_, _], initialType: Option[Type] = None) {
  // Using mutable data-structures and mutability to minimize validation footprint

  private val typeStack: ValidatorStack[Option[Type]] = ValidatorStack.empty
  private val parentTypeStack: ValidatorStack[Option[CompositeType[_]]] = ValidatorStack.empty
  private val inputTypeStack: ValidatorStack[Option[InputType[_]]] = ValidatorStack.empty
  private val fieldDefStack: ValidatorStack[Option[Field[_, _]]] = ValidatorStack.empty
  private val ancestorStack: ValidatorStack[ast.AstNode] = ValidatorStack.empty
  private val documentStack: ValidatorStack[ast.Document] = ValidatorStack.empty
  private val defaultValueStack: ValidatorStack[Option[(_, ToInput[_, _])]] = ValidatorStack.empty

  initialType.foreach(forcePushType)

  var directive: Option[Directive] = None
  var enumValue: Option[EnumValue[_]] = None
  var argument: Option[Argument[_]] = None

  def tpe = typeStack.headOption.flatten
  def previousParentType = parentTypeStack.headOption(1).flatten
  def parentType = parentTypeStack.headOption.flatten
  def inputType = inputTypeStack.headOption.flatten
  def parentInputType = inputTypeStack.headOption(1).flatten
  def fieldDef = fieldDefStack.headOption.flatten
  def ancestors: Seq[ast.AstNode] = ancestorStack.toSeq
  def document = documentStack.headOption
  def defaultValue = defaultValueStack.headOption.flatten

  def forcePushType(tpe: Type): Unit = {
    tpe match {
      case t: InputType[_] ⇒ inputTypeStack.push(Some(t))
      case _ ⇒ // do nothing
    }

    tpe match {
      case t: CompositeType[_] ⇒ parentTypeStack.push(Some(t))
      case _ ⇒ // do nothing
    }

    tpe match {
      case t: OutputType[_] ⇒ typeStack.push(Some(t))
      case _ ⇒ // do nothing
    }
  }

  def withInputType(inputType: InputType[_]) = {
    inputTypeStack push Some(inputType)

    this
  }

  def enter(node: ast.AstNode) = {
    ancestorStack push node

    node match {
      case document: ast.Document ⇒
        documentStack push document
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
      case fs: ast.FragmentSpread ⇒
        val fragment = document.flatMap(_.fragments.get(fs.name))
        typeStack.push(fragment.flatMap(fd ⇒ schema.allTypes get fd.typeCondition.name))
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

        defaultValueStack push argument.flatMap(_.defaultValue)
        inputTypeStack push argument.map(_.inputValueType)
      case ast.ListValue(values, _, _) ⇒
        // List positions never have a default value.
        defaultValueStack push None

        inputType match {
          case Some(it) ⇒ it.nonOptionalType match {
            case it: ListInputType[_] ⇒ inputTypeStack push Some(it.ofType)
            case _ ⇒ inputTypeStack push None
          }
          case None ⇒ inputTypeStack push None
        }
      case ast.ObjectField(name, value, _, _) ⇒
        val (fieldType, defaultValue) = inputType match {
          case Some(it) if it.namedType.isInstanceOf[InputObjectType[_]] ⇒
            it.namedType match {
              case obj: InputObjectType[_] ⇒
                val field = obj.fieldsByName.get(name)

                field.map(_.inputValueType) → field.flatMap(_.defaultValue)
              case _ ⇒ None → None
            }

          case _ ⇒ None → None
        }

        defaultValueStack push defaultValue
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
      case document: ast.Document ⇒
        documentStack.pop()
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
      case fs: ast.FragmentSpread ⇒
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
        defaultValueStack.pop()
        inputTypeStack.pop()
      case ast.ListValue(_, _, _) ⇒
        defaultValueStack.pop()
        inputTypeStack.pop()
      case ast.ObjectField(_, _, _, _) ⇒
        defaultValueStack.pop()
        inputTypeStack.pop()
      case ast.EnumValue(_, _, _) ⇒
        enumValue = None
      case _ ⇒ // ignore
    }

    ancestorStack.pop()
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
