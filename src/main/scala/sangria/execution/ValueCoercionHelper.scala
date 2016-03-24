package sangria.execution

import org.parboiled2.Position
import sangria.ast
import sangria.marshalling.{RawResultMarshaller, ResultMarshaller, InputUnmarshaller, ToInput}
import sangria.parser.SourceMapper
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.validation._

import scala.collection.immutable.VectorBuilder

class ValueCoercionHelper[Ctx](sourceMapper: Option[SourceMapper] = None, deprecationTracker: DeprecationTracker = DeprecationTracker.empty, userContext: Option[Ctx] = None) {
  import ValueCoercionHelper.defaultValueMapFn

  def resolveListValue(
      ofType: InputType[_],
      fieldPath: List[String],
      marshaller: ResultMarshaller,
      pos: List[Position] = Nil)(value: Either[Vector[Violation], Option[Any]]): Either[Vector[Violation], marshaller.Node] = value match {
    case Right(v) if isOptional(ofType) ⇒ Right(marshaller.optionalArrayNodeValue(v.asInstanceOf[Option[marshaller.Node]]))
    case Right(Some(v)) ⇒ Right(v.asInstanceOf[marshaller.Node])
    case Right(None) ⇒ Left(Vector(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)))
    case Left(violations) ⇒ Left(violations)
  }

  def resolveMapValue(
      ofType: InputType[_],
      fieldPath: List[String],
      default: Option[(_, ToInput[_, _])],
      fieldName: String,
      marshaller: ResultMarshaller,
      firstKindMarshaller: ResultMarshaller,
      errors: VectorBuilder[Violation],
      pos: List[Position] = Nil,
      allowErrorsOnDefault: Boolean = false,
      valueMap: Nothing ⇒ Any = defaultValueMapFn)(acc: marshaller.MapBuilder, value: Option[Either[Vector[Violation], Option[marshaller.Node]]]): marshaller.MapBuilder = {
    val valueMapTyped = valueMap.asInstanceOf[Any ⇒ marshaller.Node]

    def getDefault = {
      val Some((defaultValue, toInput)) = default.asInstanceOf[Option[(Any, ToInput[Any, Any])]]
      val (defaultInput, inputUnmarshaller) = toInput.toInput(defaultValue)

      coerceInputValue(ofType, fieldPath, defaultInput, None, marshaller, firstKindMarshaller)(inputUnmarshaller).right.map(_.get) match {
        case Right(v) ⇒
          marshaller.addMapNodeElem(acc, fieldName, valueMapTyped(v), false)
        case Left(violations) ⇒
          errors ++= violations
          acc
      }
    }

    value match {
      case None if default.isDefined ⇒
        getDefault
      case None if isOptional(ofType) ⇒
        acc
      case None ⇒
        errors += NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)
        acc

      case Some(Right(None)) if default.isDefined ⇒
        getDefault
      case Some(Right(None)) if isOptional(ofType) ⇒
        marshaller.addMapNodeElem(acc, fieldName, marshaller.nullNode, true)
      case Some(Right(None)) ⇒
        errors += NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)
        acc

      case Some(Right(Some(v))) ⇒
        marshaller.addMapNodeElem(acc, fieldName, valueMapTyped(v), isOptional(ofType) && default.isEmpty)
      case Some(Left(_)) if allowErrorsOnDefault && default.isDefined ⇒
        getDefault
      case Some(Left(violations)) ⇒
        errors ++= violations
        acc
    }
  }

  def isOptional(tpe: InputType[_]) =
    tpe.isInstanceOf[OptionInputType[_]]

  def coerceInputValue[In](
      tpe: InputType[_],
      fieldPath: List[String],
      input: In,
      variables: Option[Map[String, VariableValue]],
      marshaller: ResultMarshaller,
      firstKindMarshaller: ResultMarshaller,
      errorPrefix: ⇒ String = "")(implicit iu: InputUnmarshaller[In]): Either[Vector[Violation], Option[marshaller.Node]] = (tpe, input) match {
    case (_, node) if iu.isVariableNode(node) ⇒
      val varName = iu.getVariableName(node)

      variables match {
        case Some(vars) ⇒
          vars.get(varName) match {
            case Some(vv) ⇒
              vv.resolve(marshaller, firstKindMarshaller) match {
                case resolved @ Right(_) ⇒ resolved.asInstanceOf[Either[Vector[Violation], Option[marshaller.Node]]]
                case errors @ Left(_) ⇒ errors.asInstanceOf[Either[Vector[Violation], Option[marshaller.Node]]]
              }
            case None ⇒
              Right(None)
          }

        case None ⇒
          Left(Vector(VariableNotAllowedViolation(varName, sourceMapper, Nil)))
      }

    case (OptionInputType(ofType), value) if iu.isDefined(value) ⇒
      coerceInputValue(ofType, fieldPath, value, variables, marshaller, firstKindMarshaller, errorPrefix)

    case (OptionInputType(ofType), value) ⇒
      Right(None)

    case (ListInputType(ofType), values) if iu.isListNode(values) ⇒
      val res = iu.getListValue(values).toVector.map {
        case defined if iu.isDefined(defined) ⇒
          resolveListValue(ofType, fieldPath, marshaller, valuePosition(defined))(
            coerceInputValue(ofType, fieldPath, defined, variables, firstKindMarshaller, firstKindMarshaller, errorPrefix))
        case v ⇒
          resolveListValue(ofType, fieldPath, marshaller, valuePosition(v, values))(Right(None))
      }

      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(es) ⇒ es}.toVector.flatten)
      else Right(Some(marshaller.arrayNode(successes.collect {case Right(v) ⇒ v})))

    case (ListInputType(ofType), value) ⇒
      val res = value match {
        case defined if iu.isDefined(defined) ⇒
          resolveListValue(ofType, fieldPath, marshaller, valuePosition(defined))(
            coerceInputValue(ofType, fieldPath, defined, variables, firstKindMarshaller, firstKindMarshaller, errorPrefix))
        case v ⇒
          resolveListValue(ofType, fieldPath, marshaller, valuePosition(v, value))(Right(None))
      }

      res match {
        case Right(v) ⇒ Right(Some(marshaller.arrayNode(Vector(v))))
        case Left(violations) ⇒ Left(violations)
      }

    case (objTpe: InputObjectType[_], valueMap) if iu.isMapNode(valueMap) ⇒
      val errors = new VectorBuilder[Violation]

      val res = objTpe.fields.foldLeft(firstKindMarshaller.emptyMapNode(objTpe.fields.map(_.name))) {
        case (acc, field) ⇒ iu.getMapValue(valueMap, field.name) match {
          case Some(defined) if iu.isDefined(defined) ⇒
            resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, firstKindMarshaller, firstKindMarshaller, errors, valuePosition(defined))(
              acc, Some(coerceInputValue(field.fieldType, fieldPath :+ field.name, defined, variables, firstKindMarshaller, firstKindMarshaller, errorPrefix)))
          case Some(defined) ⇒
            resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, firstKindMarshaller, firstKindMarshaller, errors, valuePosition(valueMap))(acc, Some(Right(None)))
          case _ ⇒
            resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, firstKindMarshaller, firstKindMarshaller, errors, valuePosition(valueMap))(acc, None)
        }
      }

      val errorRes = errors.result()

      if (errorRes.nonEmpty) Left(errorRes)
      else Right(Some(firstKindMarshaller.mapNode(res).asInstanceOf[marshaller.Node]))

    case (objTpe: InputObjectType[_], value) ⇒
      Left(Vector(InputObjectTypeMismatchViolation(fieldPath, SchemaRenderer.renderTypeName(objTpe), iu.render(value), sourceMapper, valuePosition(value))))

    case (scalar: ScalarType[_], value) if iu.isScalarNode(value) ⇒
      val coerced = iu.getScalarValue(value) match {
        case node: ast.Value ⇒ scalar.coerceInput(node)
        case other ⇒ scalar.coerceUserInput(other)
      }

      coerced.fold(
        violation ⇒ Left(Vector(FieldCoercionViolation(fieldPath, violation, sourceMapper, valuePosition(value), errorPrefix))),
        v ⇒ {
          val prepared = firstKindMarshaller match {
            case raw: RawResultMarshaller ⇒ raw.rawScalarNode(v)
            case standard ⇒ Resolver.marshalValue(scalar.coerceOutput(v), standard)
          }

          Right(Some(prepared.asInstanceOf[marshaller.Node]))
        })

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

          val prepared = firstKindMarshaller match {
            case raw: RawResultMarshaller ⇒ raw.rawScalarNode(v)
            case standard ⇒ Resolver.marshalValue(enum.coerceOutput(v), standard)
          }

          Right(Some(prepared.asInstanceOf[marshaller.Node]))
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

    case (ListInputType(ofType), Some(values)) if um.isListNode(values) ⇒
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
  private val defaultValueMapFn = (x: Any) ⇒ x

  lazy val default = new ValueCoercionHelper[Unit]
}
