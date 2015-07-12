package sangria.execution

import sangria.DeliveryScheme
import sangria.ast
import sangria.ast.VariableDefinition
import sangria.renderer.QueryRenderer
import sangria.schema._

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
      Failure(ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }

  def getVariableValues[Input](schema: Schema[_, _], definitions: List[VariableDefinition], inputVars: Input, unmarshaller: InputUnmarshaller[Input]): Try[Map[String, Any]] = {
    val res = definitions.foldLeft(List[(String, Try[Any])]()) {
      case (acc, varDef) =>
        val value = schema.inputTypes.get(varDef.tpe.name)
          .map(getVariableValue(schema, varDef, _, unmarshaller)(unmarshaller.getRootMapValue(inputVars, varDef.name)))
          .getOrElse(Failure(ExecutionError("Variable type not found! It should be verified at the validation phase!")))

        acc :+ (varDef.name, value)
    }

    val (errors, values) = res.partition(_._2.isFailure)

    if (errors.nonEmpty) errors.map(_._2).head.map(_ => Map.empty)
    else Success(Map(values.collect {case (name, Success(v)) => name -> v}: _*))
  }

  def getVariableValue[Input](schema: Schema[_, _], definition: VariableDefinition, tpe: InputType[_], um: InputUnmarshaller[Input])(input: Option[um.LeafNode]): Try[Any] =
    if (isValidValue(tpe, um)(input)) {
      if (input.isEmpty)
        definition.defaultValue map (coerceAstValue(tpe, _)) getOrElse coerceInputValue(tpe, input)
      else coerceInputValue(tpe, input)
    } else Failure(ExecutionError(s"Variable $$${definition.name} expected value of type " +
        s"${QueryRenderer.render(definition.tpe)} but got: ${renderInput(input)}."))

  def isValidValue[Input](tpe: InputType[_], um: InputUnmarshaller[Input])(input: Option[um.LeafNode]): Boolean = (tpe, input) match {
    case (OptionInputType(ofType), Some(value)) => isValidValue(ofType, um)(input)
    case (OptionInputType(_), None) => true
    case (ListInputType(ofType), Some(values)) if um.isArrayNode(values) =>
      um.getArrayValue(values).forall(v => isValidValue(ofType, um)(v match {
        case opt: Option[um.LeafNode @unchecked] => opt
        case other => Option(other)
      }))
    case (objTpe: InputObjectType[_], Some(valueMap)) if um.isMapNode(valueMap) =>
      objTpe.fields.forall(f => isValidValue(f.fieldType, um)(um.getMapValue(valueMap, f.name)))
    case (scalar: ScalarType[_], Some(value)) if um.isScalarNode(value) => scalar.coerceUserInput(um.getScalarValue(value)).isRight
    case (enum: EnumType[_], Some(value)) if um.isScalarNode(value) => enum.coerceUserInput(um.getScalarValue(value)).isRight
    case _ => false
  }

  def coerceInputValue(tpe: Type, input: Option[Any]): Try[Any] = ???
  def coerceAstValue(tpe: Type, value: ast.Value): Try[Any] = ???

  def renderInput(input: Any) = input.toString
}

case class ExecutionResult[T](data: T, errors: List[T], result: T)
