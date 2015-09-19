package sangria.execution

import org.parboiled2.Position
import sangria.ast
import sangria.parser.SourceMapper
import sangria.renderer.{SchemaRenderer, QueryRenderer}
import sangria.schema._
import sangria.validation._

import scala.util.{Success, Failure, Try}

class ValueCollector[Ctx, Input](schema: Schema[_, _], inputVars: Input, sourceMapper: Option[SourceMapper], deprecationTracker: DeprecationTracker, userContext: Ctx)(implicit um: InputUnmarshaller[Input]) {
  def getVariableValues(definitions: List[ast.VariableDefinition]): Try[Map[String, Any]] =
    if (!um.isMapNode(inputVars))
      Failure(new ExecutionError(s"Variables should be a map-like object, like JSON object. Got: ${um.render(inputVars)}"))
    else {
      val res = definitions.foldLeft(List[(String, Either[List[Violation], Any])]()) {
        case (acc, varDef) =>
          val value = schema.getInputType(varDef.tpe)
            .map(getVariableValue(varDef, _, um.getRootMapValue(inputVars, varDef.name)))
            .getOrElse(Left(UnknownVariableTypeViolation(varDef.name, QueryRenderer.render(varDef.tpe), sourceMapper, varDef.position.toList) :: Nil))

          value match {
            case Right(Some(v)) => acc :+ (varDef.name, Right(v))
            case Right(None) => acc
            case l: Left[_, _] => acc :+ (varDef.name, l)
          }
      }

      val (errors, values) = res.partition(_._2.isLeft)

      if (errors.nonEmpty) Failure(VariableCoercionError(errors.collect{case (name, Left(errors)) => errors}.flatten))
      else Success(Map(values.collect {case (name, Right(v)) => name -> v}: _*))
    }

  def getArgumentValues(argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, Any]): Try[Map[String, Any]] = {
    val astArgMap = argumentAsts groupBy (_.name) mapValues (_.head)

    val res = argumentDefs.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
      case (acc, argDef) =>
        val argPath = argDef.name :: Nil
        val astValue = astArgMap get argDef.name map (_.value)

        resolveMapValue(argDef.argumentType, argPath, argDef.defaultValue, argDef.name, acc,
          astValue map (coerceAstValue(argDef.argumentType, argPath, _, variables)) getOrElse Right(None), allowErrorsOnDefault = true)
    }

    val errors = res.collect{case (_, Left(errors)) => errors}.toList.flatten

    if (errors.nonEmpty) Failure(AttributeCoercionError(errors))
    else Success(res mapValues (_.right.get))
  }

  def getVariableValue(definition: ast.VariableDefinition, tpe: InputType[_], input: Option[Input]): Either[List[Violation], Option[Any]] =
    if (isValidValue(tpe, input)) {
      val fieldPath = s"$$${definition.name}" :: Nil

      if (input.isEmpty || !um.isDefined(input.get))
        definition.defaultValue map (coerceAstValue(tpe, fieldPath, _, Map.empty)) getOrElse Right(None)
      else coerceInputValue(tpe, fieldPath, input.get)
    } else Left(VarTypeMismatchViolation(definition.name, QueryRenderer.render(definition.tpe), input map um.render, sourceMapper, definition.position.toList) :: Nil)

  def isValidValue(tpe: InputType[_], input: Option[Input]): Boolean = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) if um.isDefined(value) => isValidValue(ofType, Some(value))
    case (OptionInputType(_), _) => true
    case (ListInputType(ofType), Some(values)) if um.isArrayNode(values) =>
      um.getListValue(values).forall(v => isValidValue(ofType, v match {
        case opt: Option[Input @unchecked] => opt
        case other => Option(other)
      }))
    case (ListInputType(ofType), Some(value)) if um.isDefined(value) =>
      isValidValue(ofType, value match {
        case opt: Option[Input @unchecked] => opt
        case other => Option(other)
      })
    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) =>
      objTpe.fields.forall(f => isValidValue(f.fieldType, um.getMapValue(valueMap, f.name))) &&
        um.getMapKeys(valueMap).forall(objTpe.fieldsByName contains _)
    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) => scalar.coerceUserInput(um.getScalarValue(value)).isRight
    case (enum: EnumType[_], Some(value)) if um.isScalarNode(value) =>
      enum.coerceUserInput(um.getScalarValue(value)).isRight
    case _ => false
  }

  def resolveListValue(ofType: InputType[_], fieldPath: List[String], value: Either[List[Violation], Option[Any]], pos: Option[Position] = None) = value match {
    case r @ Right(None) if ofType.isInstanceOf[OptionInputType[_]] => r
    case Right(Some(v)) => Right(v)
    case Right(None) => Left(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos.toList) :: Nil)
    case l @ Left(_) => l
  }

  def resolveMapValue(ofType: InputType[_], fieldPath: List[String], default: Option[Any], fieldName: String, acc: Map[String, Either[List[Violation], Any]], value: Either[List[Violation], Option[Any]], pos: Option[Position] = None, allowErrorsOnDefault: Boolean = false) =
    value match {
      case Right(None) if default.isDefined => acc.updated(fieldName, Right(default.get))
      case r @ Right(None) if ofType.isInstanceOf[OptionInputType[_]] => acc
      case Right(Some(v)) => acc.updated(fieldName, Right(v))
      case Right(None) => acc.updated(fieldName, Left(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos.toList) :: Nil))
      case l @ Left(_) if allowErrorsOnDefault && default.isDefined => acc.updated(fieldName, Right(default.get))
      case l @ Left(_) => acc.updated(fieldName, l)
    }

  def coerceInputValue(tpe: InputType[_], fieldPath: List[String], input: Input): Either[List[Violation], Option[Any]] = (tpe, input) match {
    case (OptionInputType(ofType), value) => coerceInputValue(ofType, fieldPath, value)
    case (ListInputType(ofType), values) if um.isArrayNode(values) =>
      val res = um.getListValue(values).map {
        case defined if um.isDefined(defined) => resolveListValue(ofType, fieldPath, coerceInputValue(ofType, fieldPath, defined))
        case _ => resolveListValue(ofType, fieldPath, Right(None))
      }

      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(errors) => errors}.toList.flatten)
      else Right(Some(successes.collect {case Right(v) => v}))
    case (ListInputType(ofType), value) =>
      resolveListValue(ofType, fieldPath, coerceInputValue(ofType, fieldPath, value)) match {
        case Right(v) => Right(Some(Seq(v)))
        case l @ Left(violations) => Left(violations)
      }
    case (objTpe: InputObjectType[_], valueMap) if um.isMapNode(valueMap) =>
      val res = objTpe.fields.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
        case (acc, field) => um.getMapValue(valueMap, field.name) match {
          case Some(defined) if um.isDefined(defined) =>
            resolveMapValue(field.fieldType, fieldPath, field.defaultValue, field.name, acc,
              coerceInputValue(field.fieldType, fieldPath :+ field.name, defined))
          case _ => resolveMapValue(field.fieldType, fieldPath, field.defaultValue, field.name, acc, Right(None))
        }
      }

      val errors = res.collect{case (_, Left(errors)) => errors}.toList.flatten

      if (errors.nonEmpty) Left(errors)
      else Right(Some(res mapValues (_.right.get)))
    case (scalar: ScalarType[_], value) if um.isScalarNode(value) =>
      scalar.coerceUserInput(um.getScalarValue(value))
          .fold(violation => Left(FieldCoercionViolation(fieldPath, violation, None, Nil) :: Nil), v => Right(Some(v)))
    case (enum: EnumType[_], value) if um.isScalarNode(value) =>
      enum.coerceUserInput(um.getScalarValue(value))
          .fold(violation => Left(FieldCoercionViolation(fieldPath, violation, None, Nil) :: Nil), {
        case (v, deprecated) =>
          if (deprecated) deprecationTracker.deprecatedEnumValueUsed(enum, v, userContext)

          Right(Some(v))
      })
  }

  def coerceAstValue(tpe: InputType[_], fieldPath: List[String], input: ast.Value, variables: Map[String, Any]): Either[List[Violation], Option[Any]] = (tpe, input) match {
    // Note: we're not doing any checking that this variable is correct. We're
    // assuming that this query has been validated and the variable usage here
    // is of the correct type.
    case (_, ast.VariableValue(name, _)) => Right(variables get name)
    case (OptionInputType(ofType), value) => coerceAstValue(ofType, fieldPath, value, variables)
    case (ListInputType(ofType), ast.ListValue(values, _)) =>
      val res = values.map {v => resolveListValue(ofType, fieldPath, coerceAstValue(ofType, fieldPath, v, variables), v.position)}
      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(errors) => errors}.toList.flatten)
      else Right(Some(successes.collect {case Right(v) => v}))
    case (ListInputType(ofType), value) =>
      resolveListValue(ofType, fieldPath, coerceAstValue(ofType, fieldPath, value, variables), value.position) match {
        case Right(v) => Right(Some(Seq(v)))
        case l @ Left(violations) => Left(violations)
      }
    case (objTpe: InputObjectType[_], ast.ObjectValue(fieldList, objPos)) =>
      val astFields = fieldList groupBy (_.name) mapValues (_.head)
      val res = objTpe.fields.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
        case (acc, field) => astFields get field.name match {
          case Some(defined) =>
            resolveMapValue(field.fieldType, fieldPath, field.defaultValue, field.name, acc,
              coerceAstValue(field.fieldType, fieldPath :+ field.name, defined.value, variables), defined.value.position)
          case _ => resolveMapValue(field.fieldType, fieldPath, field.defaultValue, field.name, acc, Right(None), objPos)
        }
      }

      val errors = res.collect{case (_, Left(errors)) => errors}.toList.flatten

      if (errors.nonEmpty) Left(errors)
      else Right(Some(res mapValues (_.right.get)))
    case (objTpe: InputObjectType[_], value) =>
      Left(InputObjectTypeMismatchViolation(fieldPath, SchemaRenderer.renderTypeName(objTpe), QueryRenderer.render(value), sourceMapper, value.position.toList) :: Nil)
    case (scalar: ScalarType[_], value) =>
      scalar.coerceInput(value)
          .fold(violation => Left(FieldCoercionViolation(fieldPath, violation, sourceMapper, value.position.toList) :: Nil), v => Right(Some(v)))
    case (enum: EnumType[_], value) =>
      enum.coerceInput(value)
          .fold(violation => Left(FieldCoercionViolation(fieldPath, violation, sourceMapper, value.position.toList) :: Nil), {
        case (v, deprecated) =>
          if (deprecated) deprecationTracker.deprecatedEnumValueUsed(enum, v, userContext)

          Right(Some(v))
      })
  }
}
