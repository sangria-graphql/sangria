package sangria.execution

import sangria.ast.{AstVisitor, InputDocument, VariableDefinition}
import sangria.ast
import sangria.marshalling.{FromInput, InputUnmarshaller}
import sangria.parser.DeliveryScheme
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.visitor.VisitorCommand
import sangria.marshalling.queryAst._
import sangria.validation.QueryValidator

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

case class InputDocumentMaterializer[Vars](schema: Schema[_, _], variables: Vars = InputUnmarshaller.emptyMapVars)(implicit iu: InputUnmarshaller[Vars]) {
  def to[T](document: InputDocument, inputType: InputType[T])(implicit fromInput: FromInput[T], scheme: DeliveryScheme[Vector[T]]): scheme.Result = {
    val collector = new ValueCollector[Unit, Vars](schema, variables, document.sourceMapper, DeprecationTracker.empty, (), ExceptionHandler.empty, None, false)(iu)

    val violations = QueryValidator.default.validateInputDocument(schema, document, inputType)

    if (violations.nonEmpty)
      scheme.failure(InputDocumentMaterializationError(violations, ExceptionHandler.empty))
    else {
      val variableDefinitions = inferVariableDefinitions(document, inputType)

      collector.getVariableValues(variableDefinitions, None) match {
        case Failure(e) ⇒ scheme.failure(e)
        case Success(vars) ⇒
          try {
            scheme.success(document.values flatMap { value ⇒
              collector.coercionHelper.coerceInputValue(inputType, Nil, value, Some(vars), fromInput.marshaller, fromInput.marshaller, isArgument = false) match {
                case Left(vs) ⇒ throw InputDocumentMaterializationError(vs, ExceptionHandler.empty)
                case Right(coerced) ⇒ coerced.toOption.map(res ⇒ fromInput.fromResult(res))
              }
            })
          } catch {
            case NonFatal(e) ⇒ scheme.failure(e)
          }
      }
    }
  }

  def inferVariableDefinitions[T](document: InputDocument, inputType: InputType[T]) = {
    document.values.flatMap { v ⇒
      AstVisitor.visitAstWithState(schema, v, new mutable.HashMap[String, VariableDefinition]) { (typeInfo, state) ⇒
        typeInfo.withInputType(inputType)

        AstVisitor {
          case v: ast.VariableValue if typeInfo.inputType.isDefined ⇒
            val parentType = typeInfo.inputType.get
            val parentTypeAst = SchemaRenderer.renderTypeNameAst(parentType)

            state.get(v.name) match {
              case None ⇒
                state(v.name) = ast.VariableDefinition(v.name, parentTypeAst, None)
                VisitorCommand.Continue
              case _ ⇒ VisitorCommand.Continue
            }
        }
      }.values.toVector
    }
  }
}

object InputDocumentMaterializer {
  def to[T](
    schema: Schema[_, _],
    document: InputDocument,
    inputType: InputType[T]
  )(implicit fromInput: FromInput[T], scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    InputDocumentMaterializer(schema, InputUnmarshaller.emptyMapVars).to(document, inputType)

  def to[T, Vars](
    schema: Schema[_, _],
    document: InputDocument,
    inputType: InputType[T],
    variables: Vars
  )(implicit iu: InputUnmarshaller[Vars], fromInput: FromInput[T], scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    InputDocumentMaterializer(schema, variables).to(document, inputType)

  def to[T](
    document: InputDocument,
    inputType: InputType[T]
  )(implicit fromInput: FromInput[T], scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    to(emptyStubSchema(inputType), document, inputType, InputUnmarshaller.emptyMapVars)

  def to[T, Vars](
    document: InputDocument,
    inputType: InputType[T],
    variables: Vars = InputUnmarshaller.emptyMapVars
  )(implicit iu: InputUnmarshaller[Vars], fromInput: FromInput[T], scheme: DeliveryScheme[Vector[T]]): scheme.Result =
    to(emptyStubSchema(inputType), document, inputType, variables)

  private def emptyStubSchema[T : FromInput](inputType: InputType[T]): Schema[_, _] =
    Schema(ObjectType("Query", fields[Unit, Unit](
      Field("stub", StringType,
        arguments = Argument("stub", inputType) :: Nil,
        resolve = _ ⇒ "stub"))))

}
