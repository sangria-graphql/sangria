package sangria.execution

import org.parboiled2.Position
import sangria.{ast, execution}
import sangria.marshalling.{InputUnmarshaller, RawResultMarshaller, ResultMarshaller, ToInput}
import sangria.parser.SourceMapper
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema._
import sangria.validation._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.VectorBuilder

class ValueCoercionHelper[Ctx](sourceMapper: Option[SourceMapper] = None, deprecationTracker: DeprecationTracker = DeprecationTracker.empty, userContext: Option[Ctx] = None) {
  import ValueCoercionHelper.defaultValueMapFn

  private def resolveListValue(
      ofType: InputType[_],
      fieldPath: List[String],
      marshaller: ResultMarshaller,
      pos: List[Position] = Nil)(value: Either[Vector[Violation], Trinary[Any]]): Either[Vector[Violation], marshaller.Node] = value match {
    case Right(v) if ofType.isOptional ⇒ Right(marshaller.optionalArrayNodeValue(v.asInstanceOf[Trinary[marshaller.Node]].toOption))
    case Right(Trinary.Defined(v)) ⇒ Right(v.asInstanceOf[marshaller.Node])
    case Right(Trinary.Undefined) | Right(Trinary.Null) ⇒ Left(Vector(NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)))
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
      valueMap: Nothing ⇒ Any = defaultValueMapFn,
      defaultValueInfo: Option[TrieMap[String, Any]] = None,
      undefinedValues: Option[VectorBuilder[String]] = None)(acc: marshaller.MapBuilder, value: Option[Either[Vector[Violation], Trinary[marshaller.Node]]]): marshaller.MapBuilder = {
    val valueMapTyped = valueMap.asInstanceOf[Any ⇒ marshaller.Node]

    def getCoercedDefault = {
      val Some((defaultValue, toInput)) = default.asInstanceOf[Option[(Any, ToInput[Any, Any])]]
      val (defaultInput, inputUnmarshaller) = toInput.toInput(defaultValue)

      coerceInputValue(ofType, fieldPath, defaultInput, None, marshaller, firstKindMarshaller)(inputUnmarshaller)
    }

    def getDefault =
      getCoercedDefault match {
        case Right(Trinary.Defined(v)) ⇒
          marshaller.addMapNodeElem(acc, fieldName, valueMapTyped(v), optional = ofType.isOptional)
        case Right(Trinary.Undefined) | Right(Trinary.Null) | Right(_: Trinary.NullWithDefault[_]) ⇒
          acc
        case Left(violations) ⇒
          errors ++= violations
          acc
      }

    def updateDefaultInfo() =
      defaultValueInfo match {
        case Some(dvi) if default.isDefined ⇒
          getCoercedDefault match {
            case Right(Trinary.Defined(v)) ⇒
              dvi(fieldName) = valueMapTyped(v)
            case _ ⇒ // do nothing
          }
        case _ ⇒ // do nothing
      }

    def updateDefaultInfoWithValue(v: Any) =
      defaultValueInfo match {
        case Some(dvi) if default.isDefined ⇒
          dvi(fieldName) = valueMapTyped(v)
        case _ ⇒ // do nothing
      }

    def updateUndefined() =
      undefinedValues match {
        case Some(u) ⇒
          u += fieldName
        case _ ⇒ // do nothing
      }

    value match {
      case None if default.isDefined ⇒
        updateUndefined()
        getDefault
      case None if ofType.isOptional ⇒
        updateUndefined()
        acc
      case None ⇒
        updateUndefined()
        errors += NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)
        acc

      case Some(Right(Trinary.Null)) if ofType.isOptional ⇒
        updateDefaultInfo()
        marshaller.addMapNodeElem(acc, fieldName, marshaller.nullNode, optional = true)
      case Some(Right(Trinary.NullWithDefault(v))) if ofType.isOptional ⇒
        updateDefaultInfoWithValue(v)
        marshaller.addMapNodeElem(acc, fieldName, marshaller.nullNode, optional = true)
      case Some(Right(Trinary.Undefined)) if default.isDefined ⇒
        updateUndefined()
        getDefault
      case Some(Right(Trinary.Undefined)) if ofType.isOptional ⇒
        updateUndefined()
        acc
      case Some(Right(Trinary.Null)) | Some(Right(Trinary.Undefined)) | Some(Right(Trinary.NullWithDefault(_))) ⇒
        updateUndefined()
        errors += NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(ofType), sourceMapper, pos)
        acc

      case Some(Right(Trinary.Defined(v))) ⇒
        marshaller.addMapNodeElem(acc, fieldName, valueMapTyped(v), ofType.isOptional)
      case Some(Left(_)) if allowErrorsOnDefault && default.isDefined ⇒
        getDefault
      case Some(Left(violations)) ⇒
        errors ++= violations
        acc
    }
  }

  def coerceInputValue[In](
    tpe: InputType[_],
    fieldPath: List[String],
    input: In,
    variables: Option[Map[String, VariableValue]],
    marshaller: ResultMarshaller,
    firstKindMarshaller: ResultMarshaller,
    errorPrefix: ⇒ String = "",
    nullWithDefault: Boolean = false
  )(implicit iu: InputUnmarshaller[In]): Either[Vector[Violation], Trinary[marshaller.Node]] = {
    def defined(node: marshaller.Node): Trinary[marshaller.Node] =
      if (nullWithDefault) Trinary.NullWithDefault(node)
      else Trinary.Defined(node)

    (tpe, input) match {
      case (_, node) if iu.isVariableNode(node) ⇒
        val varName = iu.getVariableName(node)

        variables match {
          case Some(vars) ⇒
            vars.get(varName) match {
              case Some(vv) ⇒
                val res = vv.resolve(marshaller, firstKindMarshaller) match {
                  case resolved @ Right(_) ⇒ resolved.asInstanceOf[Either[Vector[Violation], Trinary[marshaller.Node]]]
                  case errors @ Left(_) ⇒ errors.asInstanceOf[Either[Vector[Violation], Trinary[marshaller.Node]]]
                }

                res
              case None ⇒
                Right(Trinary.Undefined)
            }

          case None ⇒
            Left(Vector(VariableNotAllowedViolation(varName, sourceMapper, Nil)))
        }

      case (OptionInputType(ofType), value) if iu.isDefined(value) ⇒
        coerceInputValue(ofType, fieldPath, value, variables, marshaller, firstKindMarshaller, errorPrefix, nullWithDefault)

      case (OptionInputType(ofType), value) ⇒
        Right(Trinary.Null)

      case (ListInputType(ofType), values) if iu.isListNode(values) ⇒
        val res = iu.getListValue(values).toVector.map {
          case defined if iu.isDefined(defined) ⇒
            resolveListValue(ofType, fieldPath, marshaller, valuePosition(defined))(
              coerceInputValue(ofType, fieldPath, defined, variables, firstKindMarshaller, firstKindMarshaller, errorPrefix, nullWithDefault))
          case v ⇒
            resolveListValue(ofType, fieldPath, marshaller, valuePosition(v, values))(Right(Trinary.Null))
        }

        val (errors, successes) = res.partition(_.isLeft)

        if (errors.nonEmpty) Left(errors.collect{case Left(es) ⇒ es}.flatten)
        else Right(defined(marshaller.arrayNode(successes.collect {case Right(v) ⇒ v})))

      case (ListInputType(ofType), value) if iu.isDefined(value) ⇒
        val res =
          resolveListValue(ofType, fieldPath, marshaller, valuePosition(value))(
            coerceInputValue(ofType, fieldPath, value, variables, firstKindMarshaller, firstKindMarshaller, errorPrefix, nullWithDefault))

        res match {
          case Right(v) ⇒ Right(defined(marshaller.arrayNode(Vector(v))))
          case Left(violations) ⇒ Left(violations)
        }

      case (lt @ ListInputType(ofType), value) ⇒
        Left(Vector(FieldCoercionViolation(
          fieldPath,
          NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(lt), sourceMapper, valuePosition(value)),
          sourceMapper,
          valuePosition(value),
          errorPrefix)))

      case (objTpe: InputObjectType[_], valueMap) if iu.isMapNode(valueMap) ⇒
        val errors = new VectorBuilder[Violation]

        val res = objTpe.fields.foldLeft(firstKindMarshaller.emptyMapNode(objTpe.fields.map(_.name))) {
          case (acc, field) ⇒ iu.getMapValue(valueMap, field.name) match {
            case Some(defined) if iu.isDefined(defined) ⇒
              resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, firstKindMarshaller, firstKindMarshaller, errors, valuePosition(defined))(
                acc, Some(coerceInputValue(field.fieldType, fieldPath :+ field.name, defined, variables, firstKindMarshaller, firstKindMarshaller, errorPrefix, nullWithDefault)))
            case Some(defined) ⇒
              resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, firstKindMarshaller, firstKindMarshaller, errors, valuePosition(valueMap))(acc, Some(Right(Trinary.Null)))
            case _ ⇒
              resolveMapValue(field.fieldType, fieldPath :+ field.name, field.defaultValue, field.name, firstKindMarshaller, firstKindMarshaller, errors, valuePosition(valueMap))(acc, None)
          }
        }

        val errorRes = errors.result()

        if (errorRes.nonEmpty) Left(errorRes)
        else Right(defined(firstKindMarshaller.mapNode(res).asInstanceOf[marshaller.Node]))

      case (objTpe: InputObjectType[_], value) if iu.isDefined(value) ⇒
        Left(Vector(InputObjectTypeMismatchViolation(fieldPath, SchemaRenderer.renderTypeName(objTpe), iu.render(value), sourceMapper, valuePosition(value))))

      case (objTpe: InputObjectType[_], value) ⇒
        Left(Vector(FieldCoercionViolation(
          fieldPath,
          NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(objTpe), sourceMapper, valuePosition(value)),
          sourceMapper,
          valuePosition(value),
          errorPrefix)))

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
              case standard ⇒ Resolver.marshalScalarValue(scalar.coerceOutput(v, standard.capabilities), standard, scalar.name, scalar.scalarInfo)
            }

            Right(defined(prepared.asInstanceOf[marshaller.Node]))
          })

      case (_: ScalarType[_], value) if iu.isDefined(value) ⇒
        Left(Vector(FieldCoercionViolation(fieldPath, GenericInvalidValueViolation(sourceMapper, valuePosition(value)), sourceMapper, valuePosition(value), errorPrefix)))

      case (scalar: ScalarType[_], value) ⇒
        Left(Vector(FieldCoercionViolation(
          fieldPath,
          NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(scalar), sourceMapper, valuePosition(value)),
          sourceMapper,
          valuePosition(value),
          errorPrefix)))

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
              case standard ⇒ Resolver.marshalEnumValue(enum.coerceOutput(v), standard, enum.name)
            }

            Right(defined(prepared.asInstanceOf[marshaller.Node]))
        })

      case (enum: EnumType[_], value) if iu.isDefined(value) ⇒
        Left(Vector(FieldCoercionViolation(fieldPath, EnumCoercionViolation, sourceMapper, valuePosition(value), errorPrefix)))

      case (enum: EnumType[_], value) ⇒
        Left(Vector(FieldCoercionViolation(
          fieldPath,
          NullValueForNotNullTypeViolation(fieldPath, SchemaRenderer.renderTypeName(enum), sourceMapper, valuePosition(value)),
          sourceMapper,
          valuePosition(value),
          errorPrefix)))
    }
  }

  private def valuePosition[T](value: T*): List[Position] = {
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

  def getVariableValue[In](definition: ast.VariableDefinition, tpe: InputType[_], input: Option[In])(implicit um: InputUnmarshaller[In]): Either[Vector[Violation], Option[VariableValue]] = {
    val violations = isValidValue(tpe, input)

    if (violations.isEmpty) {
      val fieldPath = s"$$${definition.name}" :: Nil

      if (input.isEmpty || !um.isDefined(input.get)) {
        import sangria.marshalling.queryAst.queryAstInputUnmarshaller

        definition.defaultValue match {
          case Some(dv) ⇒
            Right(Some(VariableValue((marshaller, firstKindMarshaller) ⇒
              coerceInputValue(tpe, fieldPath, dv, None, marshaller, firstKindMarshaller, nullWithDefault = input.nonEmpty))))

          case None ⇒
            val emptyValue =
              if (input.isEmpty) Trinary.Undefined
              else Trinary.Null

            Right(Some(VariableValue((marshaller, firstKindMarshaller) ⇒ Right(emptyValue))))
        }
      } else
        Right(Some(VariableValue((marshaller, firstKindMarshaller) ⇒ coerceInputValue(tpe, fieldPath, input.get, None, marshaller, firstKindMarshaller))))
    } else Left(violations.map(violation ⇒
      VarTypeMismatchViolation(definition.name, QueryRenderer.render(definition.tpe), input map um.render, violation: Violation, sourceMapper, definition.position.toList)))
  }
}

object ValueCoercionHelper {
  private val defaultValueMapFn = (x: Any) ⇒ x

  lazy val default = new ValueCoercionHelper[Unit]
}

sealed trait Trinary[+T] {
  def toOption: Option[T] = this match {
    case Trinary.Null | Trinary.Undefined | Trinary.NullWithDefault(_) ⇒ None
    case Trinary.Defined(v) ⇒ Some(v)
  }
}

object Trinary {
  case object Null extends Trinary[Nothing]
  case object Undefined extends Trinary[Nothing]

  case class Defined[T](value: T) extends Trinary[T]

  case class NullWithDefault[T](defaultValue: T) extends Trinary[T]
}
