package sangria.execution

import org.parboiled2.Position
import sangria.ast
import sangria.integration.{InputUnmarshaller, ToInput}
import sangria.parser.SourceMapper
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema._
import sangria.validation.{InputObjectTypeMismatchViolation, FieldCoercionViolation, NullValueForNotNullTypeViolation, Violation}

class ValueCoercionHelper[Ctx](sourceMapper: Option[SourceMapper] = None, deprecationTracker: DeprecationTracker = DeprecationTracker.empty, userContext: Option[Ctx] = None) {
  def resolveListValue(ofType: InputType[_], fieldPath: List[String], value: Either[List[Violation], Option[Any]], pos: Option[Position] = None) = value match {
    case r @ Right(None) if ofType.isInstanceOf[OptionInputType[_]] ⇒ r
    case Right(Some(v)) ⇒ Right(v)
    case Right(None) ⇒ Left(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos.toList) :: Nil)
    case l @ Left(_) ⇒ l
  }

  def resolveMapValue(ofType: InputType[_], fieldPath: List[String], default: Option[(_, ToInput[_, _])], fieldName: String, acc: Map[String, Either[List[Violation], Any]], value: Either[List[Violation], Option[Any]], pos: Option[Position] = None, allowErrorsOnDefault: Boolean = false) = {
    def getDefault = {
      val Some((defaultValue, toInput)) = default.asInstanceOf[Option[(Any, ToInput[Any, Any])]]
      val (defaultInput, inputUnmarshaller) = toInput.toInput(defaultValue)

      coerceInputValue(ofType, fieldPath, defaultInput)(inputUnmarshaller).right.map(_.get)
    }

    value match {
      case Right(None) if default.isDefined ⇒ acc.updated(fieldName, getDefault)
      case r @ Right(None) if ofType.isInstanceOf[OptionInputType[_]] ⇒ acc
      case Right(Some(v)) ⇒ acc.updated(fieldName, Right(v))
      case Right(None) ⇒ acc.updated(fieldName, Left(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos.toList) :: Nil))
      case l @ Left(_) if allowErrorsOnDefault && default.isDefined ⇒ acc.updated(fieldName, getDefault)
      case l @ Left(_) ⇒ acc.updated(fieldName, l)
    }
  }

  def coerceInputValue[In](tpe: InputType[_], fieldPath: List[String], input: In)(implicit iu: InputUnmarshaller[In]): Either[List[Violation], Option[Any]] = (tpe, input) match {
    case (OptionInputType(ofType), value) ⇒ coerceInputValue(ofType, fieldPath, value)
    case (ListInputType(ofType), values) if iu.isArrayNode(values) ⇒
      val res = iu.getListValue(values).map {
        case defined if iu.isDefined(defined) ⇒ resolveListValue(ofType, fieldPath, coerceInputValue(ofType, fieldPath, defined))
        case _ ⇒ resolveListValue(ofType, fieldPath, Right(None))
      }

      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(errors) ⇒ errors}.toList.flatten)
      else Right(Some(successes.collect {case Right(v) ⇒ v}))
    case (ListInputType(ofType), value) ⇒
      resolveListValue(ofType, fieldPath, coerceInputValue(ofType, fieldPath, value)) match {
        case Right(v) ⇒ Right(Some(Seq(v)))
        case l @ Left(violations) ⇒ Left(violations)
      }
    case (objTpe: InputObjectType[_], valueMap) if iu.isMapNode(valueMap) ⇒
      val res = objTpe.fields.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
        case (acc, field) ⇒ iu.getMapValue(valueMap, field.name) match {
          case Some(defined) if iu.isDefined(defined) ⇒
            resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, acc,
              coerceInputValue(field.fieldType, fieldPath :+ field.name, defined))
          case _ ⇒ resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, acc, Right(None))
        }
      }

      val errors = res.collect{case (_, Left(errors)) ⇒ errors}.toList.flatten

      if (errors.nonEmpty) Left(errors)
      else Right(Some(res mapValues (_.right.get)))
    case (scalar: ScalarType[_], value) if iu.isScalarNode(value) ⇒
      scalar.coerceUserInput(iu.getScalarValue(value))
          .fold(violation ⇒ Left(FieldCoercionViolation(fieldPath, violation, None, Nil) :: Nil), v ⇒ Right(Some(v)))
    case (enum: EnumType[_], value) if iu.isScalarNode(value) ⇒
      enum.coerceUserInput(iu.getScalarValue(value))
          .fold(violation ⇒ Left(FieldCoercionViolation(fieldPath, violation, None, Nil) :: Nil), {
        case (v, deprecated) ⇒
          if (deprecated && userContext.isDefined) deprecationTracker.deprecatedEnumValueUsed(enum, v, userContext.get)

          Right(Some(v))
      })
  }

  def isValidValue[In](tpe: InputType[_], input: Option[In])(implicit um: InputUnmarshaller[In]): Boolean = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) if um.isDefined(value) ⇒ isValidValue(ofType, Some(value))
    case (OptionInputType(_), _) ⇒ true
    case (ListInputType(ofType), Some(values)) if um.isArrayNode(values) ⇒
      um.getListValue(values).forall(v ⇒ isValidValue(ofType, v match {
        case opt: Option[In @unchecked] ⇒ opt
        case other ⇒ Option(other)
      }))
    case (ListInputType(ofType), Some(value)) if um.isDefined(value) ⇒
      isValidValue(ofType, value match {
        case opt: Option[In @unchecked] ⇒ opt
        case other ⇒ Option(other)
      })
    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) ⇒
      objTpe.fields.forall(f ⇒ isValidValue(f.fieldType, um.getMapValue(valueMap, f.name))) &&
        um.getMapKeys(valueMap).forall(objTpe.fieldsByName contains _)
    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) ⇒ scalar.coerceUserInput(um.getScalarValue(value)).isRight
    case (enum: EnumType[_], Some(value)) if um.isScalarNode(value) ⇒
      enum.coerceUserInput(um.getScalarValue(value)).isRight
    case _ ⇒ false
  }

  def coerceAstValue(tpe: InputType[_], fieldPath: List[String], input: ast.Value, variables: Map[String, Any]): Either[List[Violation], Option[Any]] = (tpe, input) match {
    case (_, ast.VariableValue(name, _)) ⇒ Right(variables get name)
    case (OptionInputType(ofType), value) ⇒ coerceAstValue(ofType, fieldPath, value, variables)
    case (ListInputType(ofType), ast.ListValue(values, _)) ⇒
      val res = values.map {v ⇒ resolveListValue(ofType, fieldPath, coerceAstValue(ofType, fieldPath, v, variables), v.position)}
      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(errors) ⇒ errors}.toList.flatten)
      else Right(Some(successes.collect {case Right(v) ⇒ v}))
    case (ListInputType(ofType), value) ⇒
      resolveListValue(ofType, fieldPath, coerceAstValue(ofType, fieldPath, value, variables), value.position) match {
        case Right(v) ⇒ Right(Some(Seq(v)))
        case l @ Left(violations) ⇒ Left(violations)
      }
    case (objTpe: InputObjectType[_], ast.ObjectValue(fieldList, objPos)) ⇒
      val astFields = fieldList groupBy (_.name) mapValues (_.head)
      val res = objTpe.fields.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
        case (acc, field) ⇒ astFields get field.name match {
          case Some(defined) ⇒
            resolveMapValue(field.fieldType, fieldPath, field.defaultValue, field.name, acc,
              coerceAstValue(field.fieldType, fieldPath :+ field.name, defined.value, variables), defined.value.position)
          case _ ⇒ resolveMapValue(field.fieldType, fieldPath, field.defaultValue, field.name, acc, Right(None), objPos)
        }
      }

      val errors = res.collect{case (_, Left(errors)) ⇒ errors}.toList.flatten

      if (errors.nonEmpty) Left(errors)
      else Right(Some(res mapValues (_.right.get)))
    case (objTpe: InputObjectType[_], value) ⇒
      Left(InputObjectTypeMismatchViolation(fieldPath, SchemaRenderer.renderTypeName(objTpe), QueryRenderer.render(value), sourceMapper, value.position.toList) :: Nil)
    case (scalar: ScalarType[_], value) ⇒
      scalar.coerceInput(value)
          .fold(violation ⇒ Left(FieldCoercionViolation(fieldPath, violation, sourceMapper, value.position.toList) :: Nil), v ⇒ Right(Some(v)))
    case (enum: EnumType[_], value) ⇒
      enum.coerceInput(value)
          .fold(violation ⇒ Left(FieldCoercionViolation(fieldPath, violation, sourceMapper, value.position.toList) :: Nil), {
        case (v, deprecated) ⇒
          if (deprecated && userContext.isDefined) deprecationTracker.deprecatedEnumValueUsed(enum, v, userContext.get)

          Right(Some(v))
      })
  }
}

object ValueCoercionHelper {
  lazy val default = new ValueCoercionHelper[Unit]
}
