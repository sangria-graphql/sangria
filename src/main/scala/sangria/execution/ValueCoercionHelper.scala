package sangria.execution

import org.parboiled2.Position
import sangria.ast
import sangria.marshalling.{InputUnmarshaller, ToInput}
import sangria.parser.SourceMapper
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.validation._

class ValueCoercionHelper[Ctx](sourceMapper: Option[SourceMapper] = None, deprecationTracker: DeprecationTracker = DeprecationTracker.empty, userContext: Option[Ctx] = None) {
  def resolveListValue(ofType: InputType[_], fieldPath: List[String], value: Either[Vector[Violation], Option[Any]], pos: List[Position] = Nil) = value match {
    case r @ Right(None) if ofType.isInstanceOf[OptionInputType[_]] ⇒ r
    case Right(Some(v)) ⇒ Right(v)
    case Right(None) ⇒ Left(Vector(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)))
    case l @ Left(_) ⇒ l
  }

  def resolveMapValue(ofType: InputType[_], fieldPath: List[String], default: Option[(_, ToInput[_, _])], fieldName: String, acc: Map[String, Either[Vector[Violation], Any]], value: Either[Vector[Violation], Option[Any]], pos: List[Position] = Nil, allowErrorsOnDefault: Boolean = false) = {
    def getDefault = {
      val Some((defaultValue, toInput)) = default.asInstanceOf[Option[(Any, ToInput[Any, Any])]]
      val (defaultInput, inputUnmarshaller) = toInput.toInput(defaultValue)

      coerceInputValue(ofType, fieldPath, defaultInput, None)(inputUnmarshaller).right.map(_.get)
    }

    value match {
      case Right(None) if default.isDefined ⇒ acc.updated(fieldName, getDefault)
      case r @ Right(None) if ofType.isInstanceOf[OptionInputType[_]] ⇒ acc
      case Right(Some(v)) ⇒ acc.updated(fieldName, Right(v))
      case Right(None) ⇒ acc.updated(fieldName, Left(Vector(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos))))
      case l @ Left(_) if allowErrorsOnDefault && default.isDefined ⇒ acc.updated(fieldName, getDefault)
      case l @ Left(_) ⇒ acc.updated(fieldName, l)
    }
  }

  def coerceInputValue[In](
      tpe: InputType[_],
      fieldPath: List[String],
      input: In,
      variables: Option[Map[String, Any]],
      errorPrefix: ⇒ String = "")(implicit iu: InputUnmarshaller[In]): Either[Vector[Violation], Option[Any]] = (tpe, input) match {
    case (_, node) if iu.isVariableNode(node)⇒
      val varName = iu.getVariableName(node)

      variables match {
        case Some(vars) ⇒
          Right(vars get varName)
        case None ⇒
          Left(Vector(VariableNotAllowedViolation(varName, sourceMapper, Nil)))
      }

    case (OptionInputType(ofType), value) ⇒
      coerceInputValue(ofType, fieldPath, value, variables, errorPrefix)

    case (ListInputType(ofType), values) if iu.isArrayNode(values) ⇒
      val res = iu.getListValue(values).map {
        case defined if iu.isDefined(defined) ⇒
          resolveListValue(ofType, fieldPath, coerceInputValue(ofType, fieldPath, defined, variables, errorPrefix), valuePosition(defined))
        case v ⇒
          resolveListValue(ofType, fieldPath, Right(None), valuePosition(v, values))
      }

      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(es) ⇒ es}.toVector.flatten)
      else Right(Some(successes.collect {case Right(v) ⇒ v}))

    case (ListInputType(ofType), value) ⇒
      val res = value match {
        case defined if iu.isDefined(defined) ⇒
          resolveListValue(ofType, fieldPath, coerceInputValue(ofType, fieldPath, defined, variables, errorPrefix), valuePosition(defined))
        case v ⇒
          resolveListValue(ofType, fieldPath, Right(None), valuePosition(v, value))
      }

      res match {
        case Right(v) ⇒ Right(Some(Vector(v)))
        case Left(violations) ⇒ Left(violations)
      }

    case (objTpe: InputObjectType[_], valueMap) if iu.isMapNode(valueMap) ⇒
      val res = objTpe.fields.foldLeft(Map.empty[String, Either[Vector[Violation], Any]]) {
        case (acc, field) ⇒ iu.getMapValue(valueMap, field.name) match {
          case Some(defined) if iu.isDefined(defined) ⇒
            resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, acc,
              coerceInputValue(field.fieldType, fieldPath :+ field.name, defined, variables, errorPrefix), valuePosition(defined))
          case v ⇒
            resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, acc, Right(None), valuePosition(v, valueMap))
        }
      }

      val errors = res.collect{case (_, Left(errors)) ⇒ errors}.toVector.flatten

      if (errors.nonEmpty) Left(errors)
      else Right(Some(res mapValues (_.right.get)))

    case (objTpe: InputObjectType[_], value) ⇒
      Left(Vector(InputObjectTypeMismatchViolation(fieldPath, SchemaRenderer.renderTypeName(objTpe), iu.render(value), sourceMapper, valuePosition(value))))

    case (scalar: ScalarType[_], value) if iu.isScalarNode(value) ⇒
      val coerced = iu.getScalarValue(value) match {
        case node: ast.Value ⇒ scalar.coerceInput(node)
        case other ⇒ scalar.coerceUserInput(other)
      }

      coerced.fold(
        violation ⇒ Left(Vector(FieldCoercionViolation(fieldPath, violation, sourceMapper, valuePosition(value), errorPrefix))),
        v ⇒ Right(Some(v)))

    case (enum: ScalarType[_], value) ⇒
      Left(Vector(FieldCoercionViolation(fieldPath, GenericInvalidValueViolation(sourceMapper, valuePosition(value)), sourceMapper, valuePosition(value), errorPrefix)))

    case (enum: EnumType[_], value) if iu.isEnumNode(value) ⇒
      val coerced = iu.getScalarValue(value) match {
        case node: ast.Value ⇒ enum.coerceInput(node)
        case other ⇒ enum.coerceUserInput(other)
      }

      coerced.fold(violation ⇒ Left(Vector(FieldCoercionViolation(fieldPath, violation, sourceMapper, valuePosition(value), errorPrefix))), {
        case (v, deprecated) ⇒
          if (deprecated && userContext.isDefined) deprecationTracker.deprecatedEnumValueUsed(enum, v, userContext.get)

          Right(Some(v))
      })

    case (enum: EnumType[_], value) ⇒
      Left(Vector(FieldCoercionViolation(fieldPath, EnumCoercionViolation, sourceMapper, valuePosition(value), errorPrefix)))
  }

  def valuePosition[T](value: T*): List[Position] = {
    val values = value.view.collect {
      case node: ast.AstNode if node.position.isDefined ⇒ node.position.toList
    }

    values.headOption.fold(Nil: List[Position])(identity)
  }

  def isValidValue[In](tpe: InputType[_], input: Option[In])(implicit um: InputUnmarshaller[In]): Vector[Violation] = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) if um.isDefined(value) ⇒ isValidValue(ofType, Some(value))
    case (OptionInputType(_), _) ⇒ Vector.empty
    case (_, None) ⇒ Vector(NotNullValueIsNullViolation(sourceMapper, Nil))

    case (ListInputType(ofType), Some(values)) if um.isArrayNode(values) ⇒
      um.getListValue(values).toVector.flatMap(v ⇒ isValidValue(ofType, v match {
        case opt: Option[In @unchecked] ⇒ opt
        case other ⇒ Option(other)
      }) map (ListValueViolation(0, _, sourceMapper, Nil)))

    case (ListInputType(ofType), Some(value)) if um.isDefined(value) ⇒
      isValidValue(ofType, value match {
        case opt: Option[In @unchecked] ⇒ opt
        case other ⇒ Option(other)
      }) map (ListValueViolation(0, _, sourceMapper, Nil))

    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) ⇒
      val unknownFields = um.getMapKeys(valueMap).toVector.collect {
        case f if !objTpe.fieldsByName.contains(f) ⇒
          UnknownInputObjectFieldViolation(SchemaRenderer.renderTypeName(objTpe, true), f, sourceMapper, Nil)
      }

      if (unknownFields.nonEmpty) unknownFields
      else {
        objTpe.fields.toVector.flatMap(f ⇒
          isValidValue(f.fieldType, um.getMapValue(valueMap, f.name)) map (MapValueViolation(f.name, _, sourceMapper, Nil)))
      }

    case (objTpe: InputObjectType[_], _) ⇒
      Vector(InputObjectIsOfWrongTypeMissingViolation(SchemaRenderer.renderTypeName(objTpe, true), sourceMapper, Nil))

    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) ⇒
      val coerced = um.getScalarValue(value) match {
        case node: ast.Value ⇒ scalar.coerceInput(node)
        case other ⇒ scalar.coerceUserInput(other)
      }

      coerced match {
        case Left(violation) ⇒ Vector(violation)
        case _ ⇒ Vector.empty
      }

    case (enum: EnumType[_], Some(value)) if um.isEnumNode(value) ⇒
      val coerced = um.getScalarValue(value) match {
        case node: ast.Value ⇒ enum.coerceInput(node)
        case other ⇒ enum.coerceUserInput(other)
      }

      coerced match {
        case Left(violation) ⇒ Vector(violation)
        case _ ⇒ Vector.empty
      }

    case (enum: EnumType[_], Some(value)) ⇒
      Vector(EnumCoercionViolation)

    case _ ⇒
      Vector(GenericInvalidValueViolation(sourceMapper, Nil))
  }
}

object ValueCoercionHelper {
  lazy val default = new ValueCoercionHelper[Unit]
}
