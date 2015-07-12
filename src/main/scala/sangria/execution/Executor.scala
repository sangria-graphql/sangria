package sangria.execution

import sangria.DeliveryScheme
import sangria.ast
import sangria.ast.VariableDefinition
import sangria.renderer.QueryRenderer
import sangria.schema._
import sangria.validation.{FieldCoercionViolation, UnknownVariableTypeViolation, VarTypeMismatchViolation, Violation}

import scala.util.{Success, Failure, Try}

object Executor {
  def execute[Ctx, Root, Input](
      schema: Schema[Ctx, Root],
      queryAst: ast.Document,
      root: Root = (),
      userContext: Ctx = (),
      operationName: Option[String] = None,
      arguments: Option[Input] = None,
      marshaller: ResultMarshaller = ScalaResultMarshaller,
      unmarshaller: InputUnmarshaller[Input] = ScalaInputUnmarshaller,
      deferredResolver: DeferredResolver = NilDeferredResolver)(implicit scheme: DeliveryScheme[ExecutionResult[marshaller.Node]]): scheme.Result = {

    val foo = for {
      operation <- getOperation(queryAst, operationName)
      variables <- getVariableValues[Input](schema, operation.variables, arguments getOrElse unmarshaller.emptyNode, unmarshaller)
    } yield ExecutionResult(marshaller.booleanNode(true), Nil, marshaller.booleanNode(true))

    foo match {
      case Success(res) => scheme success res
      case Failure(error) => scheme failure error
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(new ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(new ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }

  def getVariableValues[Input](schema: Schema[_, _], definitions: List[VariableDefinition], inputVars: Input, unmarshaller: InputUnmarshaller[Input]): Try[Map[String, Any]] = {
    val res = definitions.foldLeft(List[(String, Either[List[Violation], Any])]()) {
      case (acc, varDef) =>
        val value = schema.inputTypes.get(varDef.tpe.name)
          .map(getVariableValue(schema, varDef, _, unmarshaller)(unmarshaller.getRootMapValue(inputVars, varDef.name)))
          .getOrElse(Left(UnknownVariableTypeViolation(varDef.name, QueryRenderer.render(varDef.tpe)) :: Nil))

        value match {
          case s @ Right(Some(v)) => acc :+ (varDef.name, s)
          case Right(None) => acc
          case l: Left[_, _] => acc :+ (varDef.name, l)
        }
    }

    val (errors, values) = res.partition(_._2.isLeft)

    if (errors.nonEmpty) Failure(VariableCoercionError(errors.collect{case (name, Left(errors)) => errors}.flatten))
    else Success(Map(values.collect {case (name, Right(v)) => name -> v}: _*))
  }

  def getVariableValue[Input](schema: Schema[_, _], definition: VariableDefinition, tpe: InputType[_], um: InputUnmarshaller[Input])(input: Option[um.LeafNode]): Either[List[Violation], Option[Any]] =
    if (isValidValue(tpe, um)(input)) {
      if (input.isEmpty)
        definition.defaultValue map (coerceAstValue(tpe, _)) getOrElse coerceInputValue(tpe, um, s"$$${definition.name}" :: Nil)(input)
      else coerceInputValue(tpe, um, s"$$${definition.name}" :: Nil)(input)
    } else Left(VarTypeMismatchViolation(definition.name, QueryRenderer.render(definition.tpe), input map um.render) :: Nil)

  def isValidValue[Input](tpe: InputType[_], um: InputUnmarshaller[Input])(input: Option[um.LeafNode]): Boolean = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) => isValidValue(ofType, um)(Some(value))
    case (OptionInputType(_), None) => true
    case (ListInputType(ofType), Some(values)) if um.isArrayNode(values) =>
      um.getArrayValue(values).forall(v => isValidValue(ofType, um)(v match {
        case opt: Option[um.LeafNode @unchecked] => opt
        case other => Option(other)
      }))
    case (ListInputType(ofType), Some(value)) =>
      isValidValue(ofType, um)(value match {
        case opt: Option[um.LeafNode @unchecked] => opt
        case other => Option(other)
      })
    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) =>
      objTpe.fields.forall(f => isValidValue(f.fieldType, um)(um.getMapValue(valueMap, f.name)))
    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) => scalar.coerceUserInput(um.getScalarValue(value)).isRight
    case (enum: EnumType[_], Some(value)) if um.isScalarNode(value) => enum.coerceUserInput(um.getScalarValue(value)).isRight
    case _ => false
  }

  def coerceInputValue[Input](tpe: InputType[_], um: InputUnmarshaller[Input], fieldPath: List[String])(input: Option[um.LeafNode]): Either[List[Violation], Option[Any]] = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) => coerceInputValue(ofType, um, fieldPath)(Some(value))
    case (ListInputType(ofType), Some(values)) if um.isArrayNode(values) =>
      val res = um.getArrayValue(values).map {
        case opt: Option[um.LeafNode @unchecked] => coerceInputValue(ofType, um, fieldPath)(opt)
        case other => coerceInputValue(ofType, um, fieldPath)(Option(other)) match {
          case Right(v) => Right(v.get) // todo verify whether it's safe to do this here!
          case l @ Left(_) => l
        }
      }

      val (errors, successes) = res.partition(_.isLeft)

      if (errors.nonEmpty) Left(errors.collect{case Left(errors) => errors}.toList.flatten)
      else Right(Some(successes.collect {case Right(v) => v}))
    case (ListInputType(ofType), Some(value)) =>
      coerceInputValue(ofType, um, fieldPath)(Option(value)) match {
        case Right(v) => Right(Some(Seq(v.get))) // todo verify whether it's safe to do this here!
        case l @ Left(_) => l
      }
    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) =>
      val res = objTpe.fields.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
        case (acc, field) =>
          coerceInputValue(field.fieldType, um, fieldPath :+ field.name)(um.getMapValue(valueMap, field.name)) match {
            case Right(Some(v)) => acc.updated(field.name, Right(v))
            case Right(None) => acc
            case l @ Left(errors) => acc.updated(field.name, Left(errors))
          }
      }

      val errors = res.collect{case (_, Left(errors)) => errors}.toList.flatten

      if (errors.nonEmpty) Left(errors)
      else Right(Some(res mapValues (_.right.get)))
    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) =>
      scalar.coerceUserInput(um.getScalarValue(value))
          .fold(violation => Left(FieldCoercionViolation(fieldPath, violation) :: Nil), v => Right(Some(v)))
    case (enum: EnumType[_], Some(value)) if um.isScalarNode(value) =>
      enum.coerceUserInput(um.getScalarValue(value))
          .fold(violation => Left(FieldCoercionViolation(fieldPath, violation) :: Nil), v => Right(Some(v)))
    case (_, None) => Right(None)
  }

  def coerceAstValue(tpe: InputType[_], value: ast.Value): Either[List[Violation], Option[Any]] = ???
}

case class ExecutionResult[T](data: T, errors: List[T], result: T)
