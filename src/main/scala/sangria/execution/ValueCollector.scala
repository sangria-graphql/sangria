package sangria.execution

import sangria.ast
import sangria.integration.InputUnmarshaller
import sangria.parser.SourceMapper
import sangria.renderer.QueryRenderer
import sangria.schema._
import sangria.validation._

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Failure, Try}

class ValueCollector[Ctx, Input](schema: Schema[_, _], inputVars: Input, sourceMapper: Option[SourceMapper], deprecationTracker: DeprecationTracker, userContext: Ctx)(implicit um: InputUnmarshaller[Input]) {
  val coercionHelper = new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(userContext))

  import coercionHelper._

  private val argumentCache = TrieMap[(Vector[String], List[ast.Argument]), Try[Args]]()

  def getVariableValues(definitions: List[ast.VariableDefinition]): Try[Map[String, Any]] =
    if (!um.isMapNode(inputVars))
      Failure(new ExecutionError(s"Variables should be a map-like object, like JSON object. Got: ${um.render(inputVars)}"))
    else {
      val res = definitions.foldLeft(List[(String, Either[List[Violation], Any])]()) {
        case (acc, varDef) ⇒
          val value = schema.getInputType(varDef.tpe)
            .map(getVariableValue(varDef, _, um.getRootMapValue(inputVars, varDef.name)))
            .getOrElse(Left(UnknownVariableTypeViolation(varDef.name, QueryRenderer.render(varDef.tpe), sourceMapper, varDef.position.toList) :: Nil))

          value match {
            case Right(Some(v)) ⇒ acc :+ (varDef.name, Right(v))
            case Right(None) ⇒ acc
            case l: Left[_, _] ⇒ acc :+ (varDef.name, l)
          }
      }

      val (errors, values) = res.partition(_._2.isLeft)

      if (errors.nonEmpty) Failure(VariableCoercionError(errors.collect{case (name, Left(errors)) ⇒ errors}.flatten))
      else Success(Map(values.collect {case (name, Right(v)) ⇒ name → v}: _*))
    }

  def getVariableValue(definition: ast.VariableDefinition, tpe: InputType[_], input: Option[Input]): Either[List[Violation], Option[Any]] =
    if (isValidValue(tpe, input)) {
      val fieldPath = s"$$${definition.name}" :: Nil

      if (input.isEmpty || !um.isDefined(input.get))
        definition.defaultValue map (coerceAstValue(tpe, fieldPath, _, Map.empty)) getOrElse Right(None)
      else coerceInputValue(tpe, fieldPath, input.get)
    } else Left(VarTypeMismatchViolation(definition.name, QueryRenderer.render(definition.tpe), input map um.render, sourceMapper, definition.position.toList) :: Nil)

  private val emtyArgs = Success(Args.empty)

  def getFieldArgumentValues(path: Vector[String], argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, Any]): Try[Args] =
    if(argumentDefs.isEmpty)
      emtyArgs
    else
      argumentCache.getOrElseUpdate(path → argumentAsts, getArgumentValues(argumentDefs, argumentAsts, variables))

  def getArgumentValues(argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, Any]): Try[Args] =
    if (argumentDefs.isEmpty)
      emtyArgs
    else {
      val astArgMap = argumentAsts groupBy (_.name) mapValues (_.head)

      val res = argumentDefs.foldLeft(Map.empty[String, Either[List[Violation], Any]]) {
        case (acc, argDef) ⇒
          val argPath = argDef.name :: Nil
          val astValue = astArgMap get argDef.name map (_.value)

          resolveMapValue(argDef.argumentType, argPath, argDef.defaultValue, argDef.name, acc,
            astValue map (coerceAstValue(argDef.argumentType, argPath, _, variables)) getOrElse Right(None), allowErrorsOnDefault = true)
      }

      val errors = res.collect{case (_, Left(errors)) ⇒ errors}.toList.flatten

      if (errors.nonEmpty) Failure(AttributeCoercionError(errors))
      else Success(Args(res mapValues (_.right.get)))
    }
}
