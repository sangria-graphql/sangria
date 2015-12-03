package sangria.execution

import sangria.ast
import sangria.marshalling.{FromInput, CoercedScalaResultMarshaller, ResultMarshaller, InputUnmarshaller}
import sangria.parser.SourceMapper
import sangria.renderer.QueryRenderer
import sangria.schema._
import sangria.validation._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.VectorBuilder
import scala.util.{Success, Failure, Try}

class ValueCollector[Ctx, Input](schema: Schema[_, _], inputVars: Input, sourceMapper: Option[SourceMapper], deprecationTracker: DeprecationTracker, userContext: Ctx)(implicit um: InputUnmarshaller[Input]) {
  val coercionHelper = new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(userContext))

  import coercionHelper._

  private val argumentCache = TrieMap[(Vector[String], List[ast.Argument]), Try[Args]]()

  def getVariableValues(definitions: List[ast.VariableDefinition]): Try[Map[String, VariableValue]] =
    if (!um.isMapNode(inputVars))
      Failure(new ExecutionError(s"Variables should be a map-like object, like JSON object. Got: ${um.render(inputVars)}"))
    else {
      val res = definitions.foldLeft(Vector.empty[(String, Either[Vector[Violation], VariableValue])]) {
        case (acc, varDef) ⇒
          val value = schema.getInputType(varDef.tpe)
            .map(getVariableValue(varDef, _, um.getRootMapValue(inputVars, varDef.name)))
            .getOrElse(Left(Vector(UnknownVariableTypeViolation(varDef.name, QueryRenderer.render(varDef.tpe), sourceMapper, varDef.position.toList))))

          value match {
            case Right(Some(v)) ⇒ acc :+ (varDef.name, Right(v))
            case Right(None) ⇒ acc
            case Left(violations) ⇒ acc :+ (varDef.name, Left(violations))
          }
      }

      val (errors, values) = res.partition(_._2.isLeft)

      if (errors.nonEmpty) Failure(VariableCoercionError(errors.collect{case (name, Left(errors)) ⇒ errors}.flatten))
      else Success(Map(values.collect {case (name, Right(v)) ⇒ name → v}: _*))
    }

  def getVariableValue(definition: ast.VariableDefinition, tpe: InputType[_], input: Option[Input]): Either[Vector[Violation], Option[VariableValue]] = {
    val violations = isValidValue(tpe, input)

    if (violations.isEmpty) {
      val fieldPath = s"$$${definition.name}" :: Nil

      if (input.isEmpty || !um.isDefined(input.get)) {
        import sangria.marshalling.queryAst.queryAstInputUnmarshaller

        definition.defaultValue match {
          case Some(dv) ⇒ Right(Some(VariableValue((marshaller, firstKindMarshaller) ⇒ coerceInputValue(tpe, fieldPath, dv, None, marshaller, firstKindMarshaller))))
          case None ⇒ Right(None)
        }
      } else
        Right(Some(VariableValue((marshaller, firstKindMarshaller) ⇒ coerceInputValue(tpe, fieldPath, input.get, None, marshaller, firstKindMarshaller))))
    } else Left(violations.map(violation ⇒
      VarTypeMismatchViolation(definition.name, QueryRenderer.render(definition.tpe), input map um.render, violation: Violation, sourceMapper, definition.position.toList)))
  }

  private val emptyArgs = Success(Args.empty)

  def getFieldArgumentValues(path: Vector[String], argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, VariableValue]): Try[Args] =
    if(argumentDefs.isEmpty)
      emptyArgs
    else
      argumentCache.getOrElseUpdate(path → argumentAsts, getArgumentValues(argumentDefs, argumentAsts, variables))

  def getArgumentValues(argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, VariableValue]): Try[Args] =
    if (argumentDefs.isEmpty)
      emptyArgs
    else {
      val astArgMap = argumentAsts groupBy (_.name) mapValues (_.head)
      val marshaller = CoercedScalaResultMarshaller.default
      val errors = new VectorBuilder[Violation]

      val res = argumentDefs.foldLeft(marshaller.emptyMapNode: marshaller.Node) {
        case (acc, argDef) ⇒
          val argPath = argDef.name :: Nil
          val astValue = astArgMap get argDef.name map (_.value)
          val fromInput = argDef.fromInput

          import sangria.marshalling.queryAst.queryAstInputUnmarshaller

          try {
            resolveMapValue(argDef.argumentType, argPath, argDef.defaultValue, argDef.name, marshaller, fromInput.marshaller, allowErrorsOnDefault = true, errors = errors, valueMap = fromInput.fromResult)(
              acc, astValue map (coerceInputValue(argDef.argumentType, argPath, _, Some(variables), marshaller, fromInput.marshaller)))
          } catch {
            case InputParsingError(e) =>
              errors ++= e.map(InvalidInputValueViolation(argDef.name, _, sourceMapper, astValue.flatMap(_.position).toList))
              acc
          }
      }

      val errorRes = errors.result()

      if (errorRes.nonEmpty) Failure(AttributeCoercionError(errorRes))
      else Success(Args(res.asInstanceOf[Map[String, Any]]))
    }
}

case class VariableValue(fn: (ResultMarshaller, ResultMarshaller) ⇒ Either[Vector[Violation], Option[ResultMarshaller#Node]]) {
  private val cache = TrieMap[Int, Either[Vector[Violation], Option[ResultMarshaller#Node]]]()

  def resolve(marshaller: ResultMarshaller, firstKindMarshaller: ResultMarshaller): Either[Vector[Violation], Option[firstKindMarshaller.Node]] =
    cache.getOrElseUpdate(System.identityHashCode(firstKindMarshaller),
      fn(marshaller, firstKindMarshaller)).asInstanceOf[Either[Vector[Violation], Option[firstKindMarshaller.Node]]]
}