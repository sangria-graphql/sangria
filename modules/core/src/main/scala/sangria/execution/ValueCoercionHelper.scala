package sangria.execution

import sangria.ast.AstLocation
import sangria.ast
import sangria.marshalling.{InputUnmarshaller, RawResultMarshaller, ResultMarshaller, ToInput}
import sangria.ast.SourceMapper
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema._
import sangria.util.Cache
import sangria.validation._

import scala.collection.immutable.VectorBuilder

class ValueCoercionHelper[Ctx](
    sourceMapper: Option[SourceMapper] = None,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    userContext: Option[Ctx] = None) {
  import ValueCoercionHelper.defaultValueMapFn

  private def resolveListValue(
      ofType: InputType[_],
      fieldPath: List[String],
      marshaller: ResultMarshaller,
      pos: List[AstLocation])(
      value: Either[Vector[Violation], Trinary[Any]]): Either[Vector[Violation], marshaller.Node] =
    value match {
      case Right(v) if ofType.isOptional =>
        Right(marshaller.optionalArrayNodeValue(v.asInstanceOf[Trinary[marshaller.Node]].toOption))
      case Right(Trinary.Defined(v)) => Right(v.asInstanceOf[marshaller.Node])
      case Right(Trinary.Undefined) | Right(Trinary.Null) =>
        Left(
          Vector(
            NullValueForNotNullTypeViolation(
              fieldPath,
              SchemaRenderer.renderTypeName(ofType),
              sourceMapper,
              pos)))
      case Left(violations) => Left(violations)
    }

  def resolveMapValue(
      ofType: InputType[_],
      fieldPath: List[String],
      default: Option[(_, ToInput[_, _])],
      inputFor: Option[ast.AstNode],
      fieldName: String,
      marshaller: ResultMarshaller,
      firstKindMarshaller: ResultMarshaller,
      errors: VectorBuilder[Violation],
      pos: List[AstLocation] = Nil,
      isArgument: Boolean,
      fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]],
      allowErrorsOnDefault: Boolean = false,
      valueMap: Nothing => Any = defaultValueMapFn,
      defaultValueInfo: Option[Cache[String, Any]] = None,
      undefinedValues: Option[VectorBuilder[String]] = None
  )(
      acc: marshaller.MapBuilder,
      value: Option[Either[Vector[Violation], Trinary[marshaller.Node]]]
  ): marshaller.MapBuilder = {
    val valueMapTyped = valueMap.asInstanceOf[Any => marshaller.Node]

    def locations =
      inputFor match {
        case Some(n) if n.location.isDefined && !pos.contains(n.location.get) =>
          n.location.get +: pos
        case _ => pos
      }

    def getCoercedDefault = {
      val Some((defaultValue, toInput)) = default.asInstanceOf[Option[(Any, ToInput[Any, Any])]]
      val (defaultInput, inputUnmarshaller) = toInput.toInput(defaultValue)

      coerceInputValue(
        ofType,
        fieldPath,
        defaultInput,
        inputFor,
        None,
        marshaller,
        firstKindMarshaller,
        isArgument,
        fromScalarMiddleware = fromScalarMiddleware)(inputUnmarshaller)
    }

    def getDefault =
      getCoercedDefault match {
        case Right(Trinary.Defined(v)) =>
          marshaller.addMapNodeElem(acc, fieldName, valueMapTyped(v), optional = ofType.isOptional)
        case Right(Trinary.Undefined) | Right(Trinary.Null) | Right(
              _: Trinary.NullWithDefault[_]) =>
          acc
        case Left(violations) =>
          errors ++= violations
          acc
      }

    def updateDefaultInfo() =
      defaultValueInfo match {
        case Some(dvi) if default.isDefined =>
          getCoercedDefault match {
            case Right(Trinary.Defined(v)) =>
              dvi(fieldName) = valueMapTyped(v)
            case _ => // do nothing
          }
        case _ => // do nothing
      }

    def updateDefaultInfoWithValue(v: Any) =
      defaultValueInfo match {
        case Some(dvi) if default.isDefined =>
          dvi(fieldName) = valueMapTyped(v)
        case _ => // do nothing
      }

    def updateUndefined() =
      undefinedValues match {
        case Some(u) =>
          u += fieldName
        case _ => // do nothing
      }

    value match {
      case None if default.isDefined =>
        updateUndefined()
        getDefault
      case None if ofType.isOptional =>
        updateUndefined()
        acc
      case None =>
        updateUndefined()
        errors += NullValueForNotNullTypeViolation(
          fieldPath,
          SchemaRenderer.renderTypeName(ofType),
          sourceMapper,
          locations)
        acc

      case Some(Right(Trinary.Null)) if ofType.isOptional =>
        updateDefaultInfo()
        marshaller.addMapNodeElem(acc, fieldName, marshaller.nullNode, optional = true)
      case Some(Right(Trinary.NullWithDefault(v))) if ofType.isOptional =>
        updateDefaultInfoWithValue(v)
        marshaller.addMapNodeElem(acc, fieldName, marshaller.nullNode, optional = true)
      case Some(Right(Trinary.Undefined)) if default.isDefined =>
        updateUndefined()
        getDefault
      case Some(Right(Trinary.Undefined)) if ofType.isOptional =>
        updateUndefined()
        acc
      case Some(Right(Trinary.Null)) | Some(Right(Trinary.Undefined)) | Some(
            Right(Trinary.NullWithDefault(_))) =>
        updateUndefined()
        errors += NullValueForNotNullTypeViolation(
          fieldPath,
          SchemaRenderer.renderTypeName(ofType),
          sourceMapper,
          locations)
        acc

      case Some(Right(Trinary.Defined(v))) =>
        marshaller.addMapNodeElem(acc, fieldName, valueMapTyped(v), ofType.isOptional)
      case Some(Left(_)) if allowErrorsOnDefault && default.isDefined =>
        getDefault
      case Some(Left(violations)) =>
        errors ++= violations
        acc
    }
  }

  def coerceInputValue[In](
      tpe: InputType[_],
      fieldPath: List[String],
      input: In,
      inputFor: Option[ast.AstNode],
      variables: Option[Map[String, VariableValue]],
      marshaller: ResultMarshaller,
      firstKindMarshaller: ResultMarshaller,
      isArgument: Boolean,
      errorPrefix: => String = "",
      nullWithDefault: Boolean = false,
      fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]] = None
  )(implicit iu: InputUnmarshaller[In]): Either[Vector[Violation], Trinary[marshaller.Node]] = {
    def defined(node: marshaller.Node): Trinary[marshaller.Node] =
      if (nullWithDefault) Trinary.NullWithDefault(node)
      else Trinary.Defined(node)

    def nullScalarViolation(scalar: ScalarType[_], value: In) =
      Left(
        Vector(
          FieldCoercionViolation(
            fieldPath,
            NullValueForNotNullTypeViolation(
              fieldPath,
              SchemaRenderer.renderTypeName(scalar),
              sourceMapper,
              valuePosition(inputFor, value)),
            sourceMapper,
            valuePosition(inputFor, value),
            errorPrefix,
            isArgument
          )))

    def invalidScalarViolation(value: In) =
      Left(
        Vector(
          FieldCoercionViolation(
            fieldPath,
            GenericInvalidValueViolation(sourceMapper, valuePosition(inputFor, value)),
            sourceMapper,
            valuePosition(inputFor, value),
            errorPrefix,
            isArgument
          )))

    def resolveSuccessfulCoercedScalar(
        v: Any,
        outFn: Any => Any,
        scalar: ScalarType[Any],
        value: In) = {
      val prepared = firstKindMarshaller match {
        case raw: RawResultMarshaller => raw.rawScalarNode(v)
        case standard =>
          Resolver.marshalScalarValue(
            scalar.coerceOutput(outFn(v), standard.capabilities),
            standard,
            scalar.name,
            scalar.scalarInfo)
      }

      Right(defined(prepared.asInstanceOf[marshaller.Node]))
    }

    def resolveCoercedScalar(
        coerced: Either[Violation, Any],
        outFn: Any => Any,
        scalar: ScalarType[Any],
        actualType: InputType[_],
        value: In) =
      coerced.fold(
        violation =>
          Left(
            Vector(
              FieldCoercionViolation(
                fieldPath,
                violation,
                sourceMapper,
                valuePosition(inputFor, value),
                errorPrefix,
                isArgument))),
        v =>
          fromScalarMiddleware match {
            case Some(fn) =>
              fn(v, actualType) match {
                case Some(Left(violation)) =>
                  Left(
                    Vector(
                      FieldCoercionViolation(
                        fieldPath,
                        violation,
                        sourceMapper,
                        valuePosition(inputFor, value),
                        errorPrefix,
                        isArgument)))
                case Some(Right(newv)) =>
                  resolveSuccessfulCoercedScalar(newv, outFn, scalar, value)
                case None =>
                  resolveSuccessfulCoercedScalar(v, outFn, scalar, value)
              }
            case None => resolveSuccessfulCoercedScalar(v, outFn, scalar, value)
          }
      )

    (tpe, input) match {
      case (_, node) if iu.isVariableNode(node) =>
        val varName = iu.getVariableName(node)

        variables match {
          case Some(vars) =>
            vars.get(varName) match {
              case Some(vv) =>
                val res = vv.resolve(marshaller, firstKindMarshaller, tpe) match {
                  case resolved @ Right(_) =>
                    resolved.asInstanceOf[Either[Vector[Violation], Trinary[marshaller.Node]]]
                  case errors @ Left(_) =>
                    errors.asInstanceOf[Either[Vector[Violation], Trinary[marshaller.Node]]]
                }

                res

              case None =>
                Right(Trinary.Undefined)
            }

          case None =>
            Left(Vector(VariableNotAllowedViolation(varName, sourceMapper, Nil)))
        }

      case (OptionInputType(ofType), value) if iu.isDefined(value) =>
        coerceInputValue(
          ofType,
          fieldPath,
          value,
          inputFor,
          variables,
          marshaller,
          firstKindMarshaller,
          isArgument,
          errorPrefix,
          nullWithDefault,
          fromScalarMiddleware)

      case (OptionInputType(ofType), value) =>
        Right(Trinary.Null)

      case (ListInputType(ofType), values) if iu.isListNode(values) =>
        val res = iu.getListValue(values).toVector.map {
          case defined if iu.isDefined(defined) =>
            resolveListValue(ofType, fieldPath, marshaller, valuePosition(inputFor, defined))(
              coerceInputValue(
                ofType,
                fieldPath,
                defined,
                inputFor,
                variables,
                firstKindMarshaller,
                firstKindMarshaller,
                isArgument,
                errorPrefix,
                nullWithDefault,
                fromScalarMiddleware))
          case v =>
            resolveListValue(ofType, fieldPath, marshaller, valuePosition(inputFor, v, values))(
              Right(Trinary.Null))
        }

        val (errors, successes) = res.partition(_.isLeft)

        if (errors.nonEmpty) Left(errors.collect { case Left(es) => es }.flatten)
        else Right(defined(marshaller.arrayNode(successes.collect { case Right(v) => v })))

      case (ListInputType(ofType), value) if iu.isDefined(value) =>
        val res =
          resolveListValue(ofType, fieldPath, marshaller, valuePosition(inputFor, value))(
            coerceInputValue(
              ofType,
              fieldPath,
              value,
              inputFor,
              variables,
              firstKindMarshaller,
              firstKindMarshaller,
              isArgument,
              errorPrefix,
              nullWithDefault,
              fromScalarMiddleware))

        res match {
          case Right(v) => Right(defined(marshaller.arrayNode(Vector(v))))
          case Left(violations) => Left(violations)
        }

      case (lt @ ListInputType(ofType), value) =>
        Left(
          Vector(
            FieldCoercionViolation(
              fieldPath,
              NullValueForNotNullTypeViolation(
                fieldPath,
                SchemaRenderer.renderTypeName(lt),
                sourceMapper,
                valuePosition(inputFor, value)),
              sourceMapper,
              valuePosition(inputFor, value),
              errorPrefix,
              isArgument
            )))

      case (objTpe: InputObjectType[_], valueMap) if iu.isMapNode(valueMap) =>
        val errors = new VectorBuilder[Violation]

        val res =
          objTpe.fields.foldLeft(firstKindMarshaller.emptyMapNode(objTpe.fields.map(_.name))) {
            case (acc, field) =>
              iu.getMapValue(valueMap, field.name) match {
                case Some(defined) if iu.isDefined(defined) =>
                  resolveMapValue(
                    field.fieldType,
                    fieldPath :+ field.name,
                    field.defaultValue,
                    inputFor,
                    field.name,
                    firstKindMarshaller,
                    firstKindMarshaller,
                    errors,
                    valuePosition(inputFor, defined),
                    isArgument,
                    fromScalarMiddleware
                  )(
                    acc,
                    Some(
                      coerceInputValue(
                        field.fieldType,
                        fieldPath :+ field.name,
                        defined,
                        inputFor,
                        variables,
                        firstKindMarshaller,
                        firstKindMarshaller,
                        false,
                        errorPrefix,
                        nullWithDefault,
                        fromScalarMiddleware
                      ))
                  )
                case Some(defined) =>
                  resolveMapValue(
                    field.fieldType,
                    fieldPath :+ field.name,
                    field.defaultValue,
                    inputFor,
                    field.name,
                    firstKindMarshaller,
                    firstKindMarshaller,
                    errors,
                    valuePosition(inputFor, valueMap),
                    isArgument,
                    fromScalarMiddleware
                  )(acc, Some(Right(Trinary.Null)))
                case _ =>
                  resolveMapValue(
                    field.fieldType,
                    fieldPath :+ field.name,
                    field.defaultValue,
                    inputFor,
                    field.name,
                    firstKindMarshaller,
                    firstKindMarshaller,
                    errors,
                    valuePosition(inputFor, valueMap),
                    isArgument,
                    fromScalarMiddleware
                  )(acc, None)
              }
          }

        val errorRes = errors.result()

        if (errorRes.nonEmpty) Left(errorRes)
        else Right(defined(firstKindMarshaller.mapNode(res).asInstanceOf[marshaller.Node]))

      case (objTpe: InputObjectType[_], value) if iu.isDefined(value) =>
        Left(
          Vector(
            InputObjectTypeMismatchViolation(
              fieldPath,
              SchemaRenderer.renderTypeName(objTpe),
              iu.render(value),
              sourceMapper,
              valuePosition(inputFor, value))))

      case (objTpe: InputObjectType[_], value) =>
        Left(
          Vector(
            FieldCoercionViolation(
              fieldPath,
              NullValueForNotNullTypeViolation(
                fieldPath,
                SchemaRenderer.renderTypeName(objTpe),
                sourceMapper,
                valuePosition(inputFor, value)),
              sourceMapper,
              valuePosition(inputFor, value),
              errorPrefix,
              isArgument
            )))

      case (scalar: ScalarType[_], value) if iu.isScalarNode(value) =>
        val coerced = iu.getScalarValue(value) match {
          case node: ast.Value => scalar.coerceInput(node)
          case other => scalar.coerceUserInput(other)
        }

        resolveCoercedScalar(coerced, identity, scalar.asInstanceOf[ScalarType[Any]], scalar, value)

      case (_: ScalarType[_], value) if iu.isDefined(value) =>
        invalidScalarViolation(value)

      case (scalar: ScalarType[_], value) =>
        nullScalarViolation(scalar, value)

      case (scalar: ScalarAlias[Any, Any] @unchecked, value) if iu.isScalarNode(value) =>
        val coerced = iu.getScalarValue(value) match {
          case node: ast.Value => scalar.aliasFor.coerceInput(node)
          case other => scalar.aliasFor.coerceUserInput(other)
        }

        val fromAlias = coerced match {
          case l: Left[Violation, Any] => l
          case Right(v) => scalar.fromScalar(v)
        }

        resolveCoercedScalar(fromAlias, scalar.toScalar, scalar.aliasFor, scalar, value)

      case (_: ScalarAlias[_, _], value) if iu.isDefined(value) =>
        invalidScalarViolation(value)

      case (scalar: ScalarAlias[_, _], value) =>
        nullScalarViolation(scalar.aliasFor, value)

      case (enum: EnumType[_], value) if iu.isEnumNode(value) =>
        val coerced = iu.getScalarValue(value) match {
          case node: ast.Value => enum.coerceInput(node)
          case other => enum.coerceUserInput(other)
        }

        coerced.fold(
          violation =>
            Left(
              Vector(
                FieldCoercionViolation(
                  fieldPath,
                  violation,
                  sourceMapper,
                  valuePosition(inputFor, value),
                  errorPrefix,
                  isArgument))),
          { case (v, deprecated) =>
            if (deprecated && userContext.isDefined)
              deprecationTracker.deprecatedEnumValueUsed(enum, v, userContext.get)

            val prepared = firstKindMarshaller match {
              case raw: RawResultMarshaller => raw.rawScalarNode(v)
              case standard => Resolver.marshalEnumValue(enum.coerceOutput(v), standard, enum.name)
            }

            Right(defined(prepared.asInstanceOf[marshaller.Node]))
          }
        )

      case (enum: EnumType[_], value) if iu.isDefined(value) =>
        Left(
          Vector(
            FieldCoercionViolation(
              fieldPath,
              EnumCoercionViolation,
              sourceMapper,
              valuePosition(inputFor, value),
              errorPrefix,
              isArgument)))

      case (enum: EnumType[_], value) =>
        Left(
          Vector(
            FieldCoercionViolation(
              fieldPath,
              NullValueForNotNullTypeViolation(
                fieldPath,
                SchemaRenderer.renderTypeName(enum),
                sourceMapper,
                valuePosition(inputFor, value)),
              sourceMapper,
              valuePosition(inputFor, value),
              errorPrefix,
              isArgument
            )))
    }
  }

  private def valuePosition[T](forNode: Option[ast.AstNode], value: T*): List[AstLocation] = {
    val firstValue = value.collectFirst {
      case node: ast.AstNode if node.location.isDefined => node.location.get
    }

    val nodeLocation: Option[AstLocation] = forNode.collect {
      case n if n.location.isDefined => n.location.get
    }

    nodeLocation.toList ++ firstValue.toList
  }

  def isValidValue[In](tpe: InputType[_], input: Option[In])(implicit
      um: InputUnmarshaller[In]): Vector[Violation] = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) if um.isDefined(value) =>
      isValidValue(ofType, Some(value))
    case (OptionInputType(_), _) => Vector.empty
    case (_, None) => Vector(NotNullValueIsNullViolation(sourceMapper, Nil))

    case (ListInputType(ofType), Some(values)) if um.isListNode(values) =>
      um.getListValue(values)
        .toVector
        .flatMap(v =>
          isValidValue(
            ofType,
            v match {
              case opt: Option[In @unchecked] => opt
              case other => Option(other)
            }).map(ListValueViolation(0, _, sourceMapper, Nil)))

    case (ListInputType(ofType), Some(value)) if um.isDefined(value) =>
      isValidValue(
        ofType,
        value match {
          case opt: Option[In @unchecked] => opt
          case other => Option(other)
        }).map(ListValueViolation(0, _, sourceMapper, Nil))

    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) =>
      val unknownFields = um.getMapKeys(valueMap).toVector.collect {
        case f if !objTpe.fieldsByName.contains(f) =>
          UnknownInputObjectFieldViolation(
            SchemaRenderer.renderTypeName(objTpe, true),
            f,
            sourceMapper,
            Nil)
      }

      val fieldViolations =
        objTpe.fields.toVector.flatMap(f =>
          isValidValue(f.fieldType, um.getMapValue(valueMap, f.name))
            .map(MapValueViolation(f.name, _, sourceMapper, Nil)))

      fieldViolations ++ unknownFields

    case (objTpe: InputObjectType[_], _) =>
      Vector(
        InputObjectIsOfWrongTypeMissingViolation(
          SchemaRenderer.renderTypeName(objTpe, true),
          sourceMapper,
          Nil))

    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) =>
      val coerced = um.getScalarValue(value) match {
        case node: ast.Value => scalar.coerceInput(node)
        case other => scalar.coerceUserInput(other)
      }

      coerced match {
        case Left(violation) => Vector(violation)
        case _ => Vector.empty
      }

    case (scalar: ScalarAlias[_, _], Some(value)) if um.isScalarNode(value) =>
      val coerced = um.getScalarValue(value) match {
        case node: ast.Value => scalar.aliasFor.coerceInput(node)
        case other => scalar.aliasFor.coerceUserInput(other)
      }

      coerced match {
        case Left(violation) => Vector(violation)
        case Right(v) =>
          scalar.fromScalar(v) match {
            case Left(violation) => Vector(violation)
            case _ => Vector.empty
          }
      }

    case (enum: EnumType[_], Some(value)) if um.isEnumNode(value) =>
      val coerced = um.getScalarValue(value) match {
        case node: ast.Value => enum.coerceInput(node)
        case other => enum.coerceUserInput(other)
      }

      coerced match {
        case Left(violation) => Vector(violation)
        case _ => Vector.empty
      }

    case (enum: EnumType[_], Some(value)) =>
      Vector(EnumCoercionViolation)

    case _ =>
      Vector(GenericInvalidValueViolation(sourceMapper, Nil))
  }

  def getVariableValue[In](
      definition: ast.VariableDefinition,
      tpe: InputType[_],
      input: Option[In],
      fromScalarMiddleware: Option[(Any, InputType[_]) => Option[Either[Violation, Any]]])(implicit
      um: InputUnmarshaller[In]): Either[Vector[Violation], Option[VariableValue]] = {
    val violations = isValidValue(tpe, input)

    if (violations.isEmpty) {
      val fieldPath = s"$$${definition.name}" :: Nil

      if (input.isEmpty || !um.isDefined(input.get)) {
        import sangria.marshalling.queryAst.queryAstInputUnmarshaller

        definition.defaultValue match {
          case Some(dv) =>
            Right(
              Some(
                VariableValue((marshaller, firstKindMarshaller, actualType) =>
                  coerceInputValue(
                    actualType,
                    fieldPath,
                    dv,
                    Some(definition),
                    None,
                    marshaller,
                    firstKindMarshaller,
                    nullWithDefault = input.nonEmpty,
                    fromScalarMiddleware = fromScalarMiddleware,
                    isArgument = false
                  ))))

          case None =>
            val emptyValue =
              if (input.isEmpty) Trinary.Undefined
              else Trinary.Null

            Right(Some(VariableValue((_, _, _) => Right(emptyValue))))
        }
      } else
        Right(
          Some(
            VariableValue((marshaller, firstKindMarshaller, actualType) =>
              coerceInputValue(
                actualType,
                fieldPath,
                input.get,
                Some(definition),
                None,
                marshaller,
                firstKindMarshaller,
                fromScalarMiddleware = fromScalarMiddleware,
                isArgument = false))))
    } else
      Left(
        violations.map(violation =>
          VarTypeMismatchViolation(
            definition.name,
            QueryRenderer.render(definition.tpe),
            input.map(um.render),
            violation: Violation,
            sourceMapper,
            definition.location.toList)))
  }
}

object ValueCoercionHelper {
  private val defaultValueMapFn = (x: Any) => x

  lazy val default = new ValueCoercionHelper[Unit]
}

sealed trait Trinary[+T] {
  def toOption: Option[T] = this match {
    case Trinary.Null | Trinary.Undefined | Trinary.NullWithDefault(_) => None
    case Trinary.Defined(v) => Some(v)
  }

  def map[R](fn: T => R): Trinary[R]
}

object Trinary {
  case object Null extends Trinary[Nothing] {
    def map[R](fn: Nothing => R) = this
  }

  case object Undefined extends Trinary[Nothing] {
    def map[R](fn: Nothing => R) = this
  }

  case class Defined[T](value: T) extends Trinary[T] {
    def map[R](fn: T => R) = Defined(fn(value))
  }

  case class NullWithDefault[T](defaultValue: T) extends Trinary[T] {
    def map[R](fn: T => R) = NullWithDefault(fn(defaultValue))
  }
}
