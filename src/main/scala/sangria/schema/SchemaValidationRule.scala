package sangria.schema

import sangria.ast._

import language.higherKinds
import sangria.execution._
import sangria.introspection
import sangria.marshalling.{CoercedScalaResultMarshaller, ToInput}
import sangria.parser.SourceMapper
import sangria.renderer.SchemaRenderer
import sangria.streaming.SubscriptionStream
import sangria.validation._

import scala.collection.immutable.VectorBuilder

trait SchemaValidationRule {
  def validate[Ctx, Val](schema: Schema[Ctx, Val]): List[Violation]
}

object SchemaValidationRule {
  val empty: List[SchemaValidationRule] = Nil

  val defaultFullSchemaTraversalValidationRule = new FullSchemaTraversalValidationRule(
    EnumValueReservedNameValidator,
    ContainerMembersValidator,
    ValidNamesValidator,
    IntrospectionNamesValidator)

  val default: List[SchemaValidationRule] = List(
    DefaultValuesValidationRule,
    InterfaceImplementationValidationRule,
    InterfaceMustHaveImplementationValidationRule,
    SubscriptionFieldsValidationRule,
    defaultFullSchemaTraversalValidationRule)

  def validate[Ctx, Val](schema: Schema[Ctx, Val], validationRules: List[SchemaValidationRule]): List[Violation] =
    validationRules flatMap (_.validate(schema))

  def validateWithException[Ctx, Val](schema: Schema[Ctx, Val], validationRules: List[SchemaValidationRule]): Schema[Ctx, Val] = {
    val validationErrors = validate(schema, validationRules)

    if (validationErrors.nonEmpty) throw SchemaValidationException(validationErrors.toVector)
    else schema
  }
}

object DefaultValuesValidationRule extends SchemaValidationRule {
  def validate[Ctx, Val](schema: Schema[Ctx, Val]) = {
    val coercionHelper = ValueCoercionHelper.default

    def validate(prefix: ⇒ String, path: List[String], tpe: InputType[_])(defaultValue: (_, ToInput[_, _])) = {
      val (default, toInput) = defaultValue.asInstanceOf[(Any, ToInput[Any, Any])]
      val (inputValue, iu) = toInput.toInput(default)

      coercionHelper.coerceInputValue(tpe, path, inputValue, None, None, CoercedScalaResultMarshaller.default, CoercedScalaResultMarshaller.default, false, prefix)(iu) match {
        case Left(violations) ⇒ violations
        case Right(violations) ⇒ Nil
      }
    }

    val inputTypeViolations = schema.inputTypes.values.toList flatMap {
      case it: InputObjectType[_] ⇒
        it.fields flatMap (f ⇒
          f.defaultValue map validate(s"Invalid default value of field '${f.name}' in input type '${it.name}'. ", it.name :: f.name :: Nil, f.inputValueType) getOrElse Nil)
      case _ ⇒ Nil
    }

    val outputTypeViolations = schema.outputTypes.values.toList flatMap {
      case ot: ObjectLikeType[_, _] ⇒
        ot.fields flatMap (f ⇒
          f.arguments flatMap (a ⇒
            a.defaultValue map validate(s"Invalid default value of argument '${a.name}' in field '${f.name}' defined in output type '${ot.name}'. ", ot.name :: f.name :: ("[" + a.name + "]") :: Nil, a.inputValueType) getOrElse Nil))
      case _ ⇒ Nil
    }

    inputTypeViolations ++ outputTypeViolations
  }
}

object InterfaceImplementationValidationRule extends SchemaValidationRule {
  private def validateObjectType[Ctx, Val](schema: Schema[Ctx, Val], objTpe: ObjectType[_, _], intTpe: InterfaceType[_, _]): Vector[Violation] = {
    val objFields: Map[String, Vector[Field[_, _]]] = objTpe.ownFields.groupBy(_.name)

    intTpe.ownFields.flatMap { intField ⇒
      objFields.get(intField.name) match {
        case None ⇒
          // we allow object type to inherit fields from the interfaces
          // without explicitly defining them, but only when it is not
          // defined though SDL.
          Vector.empty

        case Some(objField) if !TypeComparators.isSubType(schema, objField.head.fieldType, intField.fieldType) ⇒
          Vector(InvalidImplementationFieldTypeViolation(
            intTpe.name,
            objTpe.name,
            intField.name,
            SchemaRenderer.renderTypeName(intField.fieldType),
            SchemaRenderer.renderTypeName(objField.head.fieldType),
            SchemaElementValidator.sourceMapper(schema),
            SchemaElementValidator.location(objField.head) ++ SchemaElementValidator.location(intField)))

        case Some(objField) ⇒
          val intArgViolations = intField.arguments.flatMap { iarg ⇒
            objField.head.arguments.find(_.name == iarg.name) match {
              case None ⇒
                Vector(MissingImplementationFieldArgumentViolation(intTpe.name, objTpe.name, intField.name, iarg.name,
                  SchemaElementValidator.sourceMapper(schema),
                  SchemaElementValidator.location(iarg) ++ SchemaElementValidator.location(objField.head)))

              case Some(oarg) if !TypeComparators.isEqualType(iarg.argumentType, oarg.argumentType) ⇒
                Vector(InvalidImplementationFieldArgumentTypeViolation(
                  intTpe.name,
                  objTpe.name,
                  intField.name,
                  iarg.name,
                  SchemaRenderer.renderTypeName(iarg.argumentType),
                  SchemaRenderer.renderTypeName(oarg.argumentType),
                  SchemaElementValidator.sourceMapper(schema),
                  SchemaElementValidator.location(iarg) ++ SchemaElementValidator.location(oarg)))

              case _ ⇒ Nil
            }
          }

          val objArgViolations = objField.head.arguments
            .filterNot(oa ⇒ intField.arguments.exists(_.name == oa.name))
            .flatMap {
              case oarg if !oarg.argumentType.isInstanceOf[OptionInputType[_]] ⇒
                Vector(ImplementationExtraFieldArgumentNotOptionalViolation(
                  intTpe.name,
                  objTpe.name,
                  intField.name,
                  oarg.name,
                  SchemaRenderer.renderTypeName(oarg.argumentType),
                  SchemaElementValidator.sourceMapper(schema),
                  SchemaElementValidator.location(oarg) ++ SchemaElementValidator.location(intField)))
              case _ ⇒ Nil
            }

          intArgViolations ++ objArgViolations
      }
    }
  }

  def validate[Ctx, Val](schema: Schema[Ctx, Val]) =
    schema.possibleTypes.toList.flatMap {
      case (intName, objTypes) ⇒
        schema.outputTypes(intName) match {
          case intTpe: InterfaceType[_, _] ⇒ objTypes.flatMap(validateObjectType(schema, _, intTpe))
          case _ ⇒ Nil
        }
    }
}

object InterfaceMustHaveImplementationValidationRule extends SchemaValidationRule {
  def validate[Ctx, Val](schema: Schema[Ctx, Val]) =
    schema.typeList
      .collect {
        case interface: InterfaceType[_, _] ⇒ interface
      }
      .filter(interface ⇒
        schema.possibleTypes.get(interface.name) match {
          case Some(list) if list.nonEmpty ⇒ false
          case _ ⇒ true
        }
      )
      .map(interface ⇒ NoInterfaceImplementationViolation(
        interface.name,
        SchemaElementValidator.sourceMapper(schema),
        SchemaElementValidator.location(interface)))
      .toList
}

object SubscriptionFieldsValidationRule extends SchemaValidationRule {
  def validate[Ctx, Val](schema: Schema[Ctx, Val]) = {
    val subsName = schema.subscription.map(_.name)

    def subscriptionTag(tag: FieldTag) = tag match {
      case SubscriptionField(_) ⇒ true
      case _ ⇒ false
    }

    val otherViolations = schema.typeList.flatMap {
      case obj: ObjectLikeType[_, _] if subsName.isDefined && subsName.get != obj.name ⇒
        obj.uniqueFields.filter(_.tags exists subscriptionTag).map(f ⇒
          InvalidSubscriptionFieldViolation(obj.name, f.name))

      case _ ⇒ Nil
    }

    val subsViolations = schema.subscription.fold(List.empty[Violation]) { subsType ⇒
      val fields = subsType.uniqueFields
      val nonSubscription = fields.filter(f ⇒ !f.tags.exists(subscriptionTag))

      if (nonSubscription.size == fields.size) {
        Nil
      } else if (nonSubscription.isEmpty) {
        if (fields.isEmpty) Nil
        else {
          val first = fields.head.tags.collectFirst{case SubscriptionField(s) ⇒ s}.get

          val differentFields = fields.tail.filter(f ⇒ f.tags.collectFirst{case SubscriptionField(s) if !first.supported(s.asInstanceOf[SubscriptionStream[({type T[X]})#T]]) ⇒ s}.nonEmpty)

          if (differentFields.nonEmpty)
            List(NotAllSubscriptionFieldsHaveSameStreamViolation(subsType.name, differentFields.map(_.name)))
          else Nil
        }
      } else
        List(NotAllSubscriptionFieldsViolation(subsType.name, nonSubscription.map(_.name)))
    }

    subsViolations ++ otherViolations
  }
}

object IntrospectionNamesValidator extends SchemaElementValidator {
  private val explanation = "The name is reserved for GraphQL introspection API."
  private val allowedTypeNames = introspection.IntrospectionTypesByName.keySet

  private def isInvalidTypeName(name: String): Boolean =
    name.startsWith("__") && !allowedTypeNames.contains(name)

  private def isInvalidName(name: String): Boolean =
    name.startsWith("__")

  override def validateUnionType(schema: Schema[_, _], tpe: UnionType[_]) =
    validateType(schema, tpe, "Union")

  override def validateEnumType(schema: Schema[_, _], tpe: EnumType[_]) =
    validateType(schema, tpe, "Enum")

  override def validateInputObjectType(schema: Schema[_, _], tpe: InputObjectType[_]) =
    validateType(schema, tpe, "Input")

  override def validateObjectType(schema: Schema[_, _], tpe: ObjectType[_, _]) =
    validateType(schema, tpe, "Object")

  override def validateInterfaceType(schema: Schema[_, _], tpe: InterfaceType[_, _]) =
    validateType(schema, tpe, "Interface")

  override def validateScalarType(schema: Schema[_, _], tpe: ScalarType[_]) =
    validateType(schema, tpe, "Scalar")

  def validateType(schema: Schema[_, _], tpe: Type with Named with HasAstInfo, kind: String) =
    if(isInvalidTypeName(tpe.name)) Vector(InvalidTypeNameViolation(kind, tpe.name, explanation, sourceMapper(schema), location(tpe)))
    else Vector.empty

  override def validateEnumValue(schema: Schema[_, _], tpe: EnumType[_], value: EnumValue[_]) =
    if(isInvalidName(value.name)) Vector(InvalidEnumValueNameViolation(tpe.name, value.name, explanation, sourceMapper(schema), location(value)))
    else Vector.empty

  override def validateField(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _]) =
    if(isInvalidName(field.name)) Vector(InvalidFieldNameViolation(tpe.name, field.name, explanation, sourceMapper(schema), location(field)))
    else Vector.empty

  override def validateFieldArgument(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _], argument: Argument[_]) =
    if(isInvalidName(argument.name)) Vector(InvalidFieldArgumentNameViolation(tpe.name, field.name, argument.name, explanation, sourceMapper(schema), location(argument)))
    else Vector.empty

  override def validateInputField(schema: Schema[_, _], tpe: InputObjectType[_], field: InputField[_]) =
    if(isInvalidName(field.name)) Vector(InvalidInputFieldNameViolation(tpe.name, field.name, explanation, sourceMapper(schema), location(field)))
    else Vector.empty

  override def validateDirective(schema: Schema[_, _], tpe: Directive) =
    if(isInvalidName(tpe.name)) Vector(InvalidDirectiveNameViolation(tpe.name, explanation, None, Nil))
    else Vector.empty

  override def validateDirectiveArgument(schema: Schema[_, _], tpe: Directive, argument: Argument[_]) =
    if(isInvalidName(argument.name)) Vector(InvalidDirectiveArgumentNameViolation(tpe.name, argument.name, explanation, sourceMapper(schema), location(argument)))
    else Vector.empty
}

object ValidNamesValidator extends SchemaElementValidator {
  private val explanation = s"Valid names must satisfy following regex: /${Named.NameRegexp}/."

  override def validateUnionType(schema: Schema[_, _], tpe: UnionType[_]) =
    validateType(schema, tpe, "Union")

  override def validateEnumType(schema: Schema[_, _], tpe: EnumType[_]) =
    validateType(schema, tpe, "Enum")

  override def validateInputObjectType(schema: Schema[_, _], tpe: InputObjectType[_]) =
    validateType(schema, tpe, "Input")

  override def validateObjectType(schema: Schema[_, _], tpe: ObjectType[_, _]) =
    validateType(schema, tpe, "Object")

  override def validateInterfaceType(schema: Schema[_, _], tpe: InterfaceType[_, _]) =
    validateType(schema, tpe, "Interface")

  override def validateScalarType(schema: Schema[_, _], tpe: ScalarType[_]) =
    validateType(schema, tpe, "Scalar")

  def validateType(schema: Schema[_, _], tpe: Type with Named with HasAstInfo, kind: String) =
    if(!Named.isValidName(tpe.name)) Vector(InvalidTypeNameViolation(kind, tpe.name, explanation, sourceMapper(schema), location(tpe)))
    else Vector.empty

  override def validateEnumValue(schema: Schema[_, _], tpe: EnumType[_], value: EnumValue[_]) =
    if(!Named.isValidName(value.name)) Vector(InvalidEnumValueNameViolation(tpe.name, value.name, explanation, sourceMapper(schema), location(value)))
    else Vector.empty

  override def validateField(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _]) =
    if(!Named.isValidName(field.name)) Vector(InvalidFieldNameViolation(tpe.name, field.name, explanation, sourceMapper(schema), location(field)))
    else Vector.empty

  override def validateFieldArgument(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _], argument: Argument[_]) =
    if(!Named.isValidName(argument.name)) Vector(InvalidFieldArgumentNameViolation(tpe.name, field.name, argument.name, explanation, sourceMapper(schema), location(argument)))
    else Vector.empty

  override def validateInputField(schema: Schema[_, _], tpe: InputObjectType[_], field: InputField[_]) =
    if(!Named.isValidName(field.name)) Vector(InvalidInputFieldNameViolation(tpe.name, field.name, explanation, sourceMapper(schema), location(field)))
    else Vector.empty

  override def validateDirective(schema: Schema[_, _], tpe: Directive) =
    if(!Named.isValidName(tpe.name)) Vector(InvalidDirectiveNameViolation(tpe.name, explanation, None, Nil))
    else Vector.empty

  override def validateDirectiveArgument(schema: Schema[_, _], tpe: Directive, argument: Argument[_]) =
    if(!Named.isValidName(argument.name)) Vector(InvalidDirectiveArgumentNameViolation(tpe.name, argument.name, explanation, sourceMapper(schema), location(argument)))
    else Vector.empty
}

object ContainerMembersValidator extends SchemaElementValidator {
  override def validateUnionType(schema: Schema[_, _], tpe: UnionType[_]) = {
    val emptyErrors =
      if (tpe.types.isEmpty)
        Vector(EmptyUnionMembersViolation(tpe.name, sourceMapper(schema), location(tpe)))
      else Vector.empty

    val nonUnique =
      tpe.types.groupBy(_.name).toVector.collect {
        case (memberName, dup) if dup.size > 1 ⇒
          val astMembers = tpe.astNodes.collect {
            case astUnion: UnionTypeDefinition ⇒ astUnion.types
            case astUnion: UnionTypeExtensionDefinition ⇒ astUnion.interfaces_or_types
          }
          val locations = astMembers.flatten.filter(_.name == memberName).flatMap(_.location).toList

          NonUniqueUnionMembersViolation(tpe.name, memberName, sourceMapper(schema), locations)
      }

    emptyErrors ++ nonUnique
  }

  override def validateEnumType(schema: Schema[_, _], tpe: EnumType[_]) = {
    val emptyErrors =
      if (tpe.values.isEmpty)
        Vector(EmptyEnumValuesMembersViolation(tpe.name, sourceMapper(schema), location(tpe)))
      else Vector.empty

    val nonUnique =
      tpe.values.groupBy(_.name).toVector.collect {
        case (valueName, dup) if dup.size > 1 ⇒
          NonUniqueEnumValuesViolation(tpe.name, valueName, sourceMapper(schema), dup.flatMap(location))
      }

    emptyErrors ++ nonUnique
  }

  override def validateInputObjectType(schema: Schema[_, _], tpe: InputObjectType[_]) = {
    val emptyErrors =
      if (tpe.fields.isEmpty)
        Vector(EmptyInputFieldsViolation(tpe.name, sourceMapper(schema), location(tpe)))
      else Vector.empty

    val nonUnique =
      tpe.fields.groupBy(_.name).toVector.collect {
        case (fieldName, dup) if dup.size > 1 ⇒
          NonUniqueInputFieldsViolation(tpe.name, fieldName, sourceMapper(schema), dup.flatMap(location))
      }

    emptyErrors ++ nonUnique
  }

  override def validateObjectType(schema: Schema[_, _], tpe: ObjectType[_, _]) = {
    val generalErrors = validateObjectLikeType(schema, tpe, "Object")

    val nonUnique =
      tpe.interfaces.groupBy(_.name).toVector.collect {
        case (intName, dup) if dup.size > 1 ⇒
          val astMembers = tpe.astNodes.collect {
            case astUnion: ObjectTypeDefinition ⇒ astUnion.interfaces
            case astUnion: ObjectTypeExtensionDefinition ⇒ astUnion.interfaces
          }
          val locations = astMembers.flatten.filter(_.name == intName).flatMap(_.location).toList

          NonUniqueInterfacesViolation(tpe.name, intName, sourceMapper(schema), locations)
      }

    generalErrors ++ nonUnique
  }

  override def validateInterfaceType(schema: Schema[_, _], tpe: InterfaceType[_, _]) =
    validateObjectLikeType(schema, tpe, "Interface")

  def validateObjectLikeType(schema: Schema[_, _], tpe: ObjectLikeType[_, _], kind: String): Vector[Violation] = {
    val emptyErrors =
      if (tpe.uniqueFields.isEmpty)
        Vector(EmptyFieldsViolation(kind, tpe.name, sourceMapper(schema), location(tpe)))
      else Vector.empty

    val nonUnique =
      tpe.ownFields.groupBy(_.name).toVector.collect {
        case (fieldName, dup) if dup.size > 1 ⇒
          NonUniqueFieldsViolation(kind, tpe.name, fieldName, sourceMapper(schema), dup.flatMap(location).toList)
      }

    emptyErrors ++ nonUnique
  }

  override def validateField(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _]) =
    field.arguments.groupBy(_.name).toVector.collect {
      case (argName, dup) if dup.size > 1 ⇒
        NonUniqueFieldArgumentsViolation(tpe.name, field.name, argName, sourceMapper(schema), dup.flatMap(location))
    }

  override def validateDirective(schema: Schema[_, _], tpe: Directive) =
    tpe.arguments.groupBy(_.name).toVector.collect {
      case (argName, dup) if dup.size > 1 ⇒
        NonUniqueDirectiveArgumentsViolation(tpe.name, argName, sourceMapper(schema), dup.flatMap(location))
    }
}

object EnumValueReservedNameValidator extends SchemaElementValidator {
  private val reservedNames = Set("true", "false", "null")

  override def validateEnumValue(schema: Schema[_, _], tpe: EnumType[_], value: EnumValue[_]) =
    if (reservedNames.contains(value.name))
      Vector(ReservedEnumValueNameViolation(tpe.name, value.name, sourceMapper(schema), location(value)))
    else Vector.empty
}

trait SchemaElementValidator {
  def validateUnionType(schema: Schema[_, _], tpe: UnionType[_]): Vector[Violation] = Vector.empty

  def validateEnumType(schema: Schema[_, _], tpe: EnumType[_]): Vector[Violation] = Vector.empty
  def validateEnumValue(schema: Schema[_, _], tpe: EnumType[_], value: EnumValue[_]): Vector[Violation] = Vector.empty

  def validateObjectType(schema: Schema[_, _], tpe: ObjectType[_, _]): Vector[Violation] = Vector.empty
  def validateInterfaceType(schema: Schema[_, _], tpe: InterfaceType[_, _]): Vector[Violation] = Vector.empty
  def validateField(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _]): Vector[Violation] = Vector.empty
  def validateFieldArgument(schema: Schema[_, _], tpe: ObjectLikeType[_, _], field: Field[_, _], argument: Argument[_]): Vector[Violation] = Vector.empty

  def validateInputObjectType(schema: Schema[_, _], tpe: InputObjectType[_]): Vector[Violation] = Vector.empty
  def validateInputField(schema: Schema[_, _], tpe: InputObjectType[_], field: InputField[_]): Vector[Violation] = Vector.empty

  def validateDirective(schema: Schema[_, _], tpe: Directive): Vector[Violation] = Vector.empty
  def validateDirectiveArgument(schema: Schema[_, _], tpe: Directive, argument: Argument[_]): Vector[Violation] = Vector.empty

  def validateScalarType(schema: Schema[_, _], tpe: ScalarType[_]): Vector[Violation] = Vector.empty

  def sourceMapper(schema: Schema[_, _]): Option[SourceMapper] = SchemaElementValidator.sourceMapper(schema)
  def location(elem: HasAstInfo): List[AstLocation] = SchemaElementValidator.location(elem)
}

object SchemaElementValidator {
  def sourceMapper(schema: Schema[_, _]): Option[SourceMapper] =
    schema.astNodes.collectFirst{case doc: Document ⇒ doc.sourceMapper}.flatten

  def location(elem: HasAstInfo): List[AstLocation] =
    elem.astNodes.flatMap(_.location).toList
}

class FullSchemaTraversalValidationRule(validators: SchemaElementValidator*) extends SchemaValidationRule {
  private val reservedNames = Set("true", "false", "null")

  def validate[Ctx, Val](schema: Schema[Ctx, Val]) = {
    val violations = new VectorBuilder[Violation]

    def add(vs: Vector[Violation]): Unit =
      if (vs.nonEmpty) violations ++= vs

    def validate(fn: SchemaElementValidator ⇒ Vector[Violation]) =
      validators.foreach(v ⇒ add(fn(v)))

    schema.typeList.foreach {
      case tpe: EnumType[_] ⇒
        validate(_.validateEnumType(schema, tpe))
        tpe.values.foreach(v ⇒ validate(_.validateEnumValue(schema, tpe, v)))
      case tpe: ScalarType[_] ⇒
        validate(_.validateScalarType(schema, tpe))
      case tpe: UnionType[_] ⇒
        validate(_.validateUnionType(schema, tpe))
      case tpe: InputObjectType[_] ⇒
        validate(_.validateInputObjectType(schema, tpe))
        tpe.fields.foreach(f ⇒ validate(_.validateInputField(schema, tpe, f)))
      case tpe: ObjectType[_, _] ⇒
        validate(_.validateObjectType(schema, tpe))
        tpe.fields.foreach { f ⇒
          validate(_.validateField(schema, tpe, f))

          f.arguments.foreach(a ⇒ validate(_.validateFieldArgument(schema, tpe, f, a)))
        }
      case tpe: InterfaceType[_, _] ⇒
        validate(_.validateInterfaceType(schema, tpe))
        tpe.fields.foreach { f ⇒
          validate(_.validateField(schema, tpe, f))

          f.arguments.foreach(a ⇒ validate(_.validateFieldArgument(schema, tpe, f, a)))
        }
      case _ ⇒ // everything is fine
    }

    schema.directives.foreach { d ⇒
      validate(_.validateDirective(schema, d))

      d.arguments.foreach(a ⇒ validate(_.validateDirectiveArgument(schema, d, a)))
    }

    violations.result().toList
  }

  def validName(name: String): Boolean = !reservedNames.contains(name)
}

case class SchemaValidationException(violations: Vector[Violation], eh: ExceptionHandler = ExceptionHandler.empty) extends ExecutionError(
  s"Schema does not pass validation. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError
