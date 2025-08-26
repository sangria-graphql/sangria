package sangria.execution

import sangria.ast.{AstVisitor, InputDocument, VariableDefinition}
import sangria.ast
import sangria.execution
import sangria.marshalling.{FromInput, InputUnmarshaller}
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.visitor.VisitorCommand
import sangria.marshalling.queryAst._
import sangria.validation.QueryValidator

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

case class InputDocumentMaterializer[Vars](
    schema: Schema[_, _],
    variables: Vars = InputUnmarshaller.emptyMapVars)(implicit iu: InputUnmarshaller[Vars]) {
  def to[T](document: InputDocument, inputType: InputType[T])(implicit
      fromInput: FromInput[T]): Try[Vector[T]] = {
    val collector = new ValueCollector[Unit, Vars](
      schema,
      variables,
      document.sourceMapper,
      None,
      (),
      ExceptionHandler.empty,
      None,
      false)(iu)

    val variableDefinitions = inferVariableDefinitions(document, inputType)

    collector.getVariableValues(variableDefinitions, None) match {
      case Failure(e) =>
        // return validation errors without variables first if variables is what failed
        val violations =
          QueryValidator.default.validateInputDocument(
            schema,
            document,
            inputType,
            Map.empty[String, execution.VariableValue]
          )

        if (violations.nonEmpty)
          Failure(InputDocumentMaterializationError(violations, ExceptionHandler.empty))
        else
          Failure(e)
      case Success(vars) =>
        val violations =
          QueryValidator.default.validateInputDocument(schema, document, inputType, vars)

        if (violations.nonEmpty)
          Failure(InputDocumentMaterializationError(violations, ExceptionHandler.empty))
        else {
          try
            Success(document.values.flatMap { value =>
              collector.coercionHelper.coerceInputValue(
                inputType,
                Nil,
                value,
                None,
                Some(vars),
                fromInput.marshaller,
                fromInput.marshaller,
                isArgument = false) match {
                case Left(vs) => throw InputDocumentMaterializationError(vs, ExceptionHandler.empty)
                case Right(coerced) => coerced.toOption.map(res => fromInput.fromResult(res))
              }
            })
          catch {
            case NonFatal(e) => Failure(e)
          }
        }
    }
  }

  def inferVariableDefinitions[T](
      document: InputDocument,
      inputType: InputType[T]): Vector[VariableDefinition] =
    document.values.flatMap { v =>
      AstVisitor
        .visitAstWithState(schema, v, new mutable.HashMap[String, VariableDefinition]) {
          (typeInfo, state) =>
            typeInfo.withInputType(inputType)

            AstVisitor {
              case v2: ast.VariableValue if typeInfo.inputType.isDefined =>
                val parentType = typeInfo.inputType.get
                val parentTypeAst = SchemaRenderer.renderTypeNameAst(parentType)

                state.get(v2.name) match {
                  case None =>
                    state(v2.name) = ast.VariableDefinition(v2.name, parentTypeAst, None)
                    VisitorCommand.Continue
                  case _ => VisitorCommand.Continue
                }
            }
        }
        .values
        .toVector
    }
}

object InputDocumentMaterializer {
  def to[T](
      schema: Schema[_, _],
      document: InputDocument,
      inputType: InputType[T]
  )(implicit fromInput: FromInput[T]): Try[Vector[T]] =
    InputDocumentMaterializer(schema, InputUnmarshaller.emptyMapVars).to(document, inputType)

  def to[T, Vars](
      schema: Schema[_, _],
      document: InputDocument,
      inputType: InputType[T],
      variables: Vars
  )(implicit iu: InputUnmarshaller[Vars], fromInput: FromInput[T]): Try[Vector[T]] =
    InputDocumentMaterializer(schema, variables).to(document, inputType)

  def to[T](
      document: InputDocument,
      inputType: InputType[T]
  )(implicit fromInput: FromInput[T]): Try[Vector[T]] =
    to(emptyStubSchema(inputType), document, inputType, InputUnmarshaller.emptyMapVars)

  def to[T, Vars](document: InputDocument, inputType: InputType[T], variables: Vars)(implicit
      iu: InputUnmarshaller[Vars],
      fromInput: FromInput[T]): Try[Vector[T]] =
    to(emptyStubSchema(inputType), document, inputType, variables)

  private def emptyStubSchema[T: FromInput](inputType: InputType[T]): Schema[_, _] =
    Schema(
      ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "stub",
            StringType,
            arguments = Argument("stub", inputType) :: Nil,
            resolve = _ => "stub"))))
}
