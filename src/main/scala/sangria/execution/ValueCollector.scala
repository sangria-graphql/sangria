package sangria.execution

import sangria.ast
import sangria.marshalling.InputUnmarshaller
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
      val res = definitions.foldLeft(Vector.empty[(String, Either[Vector[Violation], Any])]) {
        case (acc, varDef) ⇒
          val value = schema.getInputType(varDef.tpe)
            .map(getVariableValue(varDef, _, um.getRootMapValue(inputVars, varDef.name)))
            .getOrElse(Left(Vector(UnknownVariableTypeViolation(varDef.name, QueryRenderer.render(varDef.tpe), sourceMapper, varDef.position.toList))))

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

  def getVariableValue(definition: ast.VariableDefinition, tpe: InputType[_], input: Option[Input]): Either[Vector[Violation], Option[Any]] = {
    val violations = isValidValue(tpe, input)

    if (violations.isEmpty) {
      val fieldPath = s"$$${definition.name}" :: Nil

      if (input.isEmpty || !um.isDefined(input.get)) {
        import sangria.marshalling.queryAst.queryAstInputUnmarshaller

        definition.defaultValue map (coerceInputValue(tpe, fieldPath, _, None)) getOrElse Right(None)
      } else
        coerceInputValue(tpe, fieldPath, input.get, None)
    } else Left(violations.map(violation ⇒
      VarTypeMismatchViolation(definition.name, QueryRenderer.render(definition.tpe), input map um.render, violation: Violation, sourceMapper, definition.position.toList)))
  }

  private val emptyArgs = Success(Args.empty)

  def getFieldArgumentValues(path: Vector[String], argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, Any]): Try[Args] =
    if(argumentDefs.isEmpty)
      emptyArgs
    else
      argumentCache.getOrElseUpdate(path → argumentAsts, getArgumentValues(argumentDefs, argumentAsts, variables))

  def getArgumentValues(argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, Any]): Try[Args] =
    if (argumentDefs.isEmpty)
      emptyArgs
    else {
      val astArgMap = argumentAsts groupBy (_.name) mapValues (_.head)

      val res = argumentDefs.foldLeft(Map.empty[String, Either[Vector[Violation], Any]]) {
        case (acc, argDef) ⇒
          val argPath = argDef.name :: Nil
          val astValue = astArgMap get argDef.name map (_.value)

          import sangria.marshalling.queryAst.queryAstInputUnmarshaller

          resolveMapValue(argDef.argumentType, argPath, argDef.defaultValue, argDef.name, acc,
            astValue map (coerceInputValue(argDef.argumentType, argPath, _, Some(variables))) getOrElse Right(None), allowErrorsOnDefault = true)
      }

      val errors = res.collect{case (_, Left(errors)) ⇒ errors}.toVector.flatten

      if (errors.nonEmpty) Failure(AttributeCoercionError(errors))
      else Success(Args(res mapValues (_.right.get)))
    }
}
