package sangria.execution

import sangria.ast
import sangria.marshalling._
import sangria.parser.SourceMapper
import sangria.renderer.QueryRenderer
import sangria.schema._
import sangria.validation._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.VectorBuilder
import scala.util.{Success, Failure, Try}

class ValueCollector[Ctx, Input](schema: Schema[_, _], inputVars: Input, sourceMapper: Option[SourceMapper], deprecationTracker: DeprecationTracker, userContext: Ctx, exceptionHandler: Executor.ExceptionHandler)(implicit um: InputUnmarshaller[Input]) {
  val coercionHelper = new ValueCoercionHelper[Ctx](sourceMapper, deprecationTracker, Some(userContext))

  import coercionHelper._

  private val argumentCache = TrieMap[(ExecutionPath.PathCacheKey, List[ast.Argument]), Try[Args]]()

  def getVariableValues(definitions: List[ast.VariableDefinition]): Try[Map[String, VariableValue]] =
    if (!um.isMapNode(inputVars))
      Failure(new ExecutionError(s"Variables should be a map-like object, like JSON object. Got: ${um.render(inputVars)}", exceptionHandler))
    else {
      val res = definitions.foldLeft(Vector.empty[(String, Either[Vector[Violation], VariableValue])]) {
        case (acc, varDef) ⇒
          val value = schema.getInputType(varDef.tpe)
            .map(coercionHelper.getVariableValue(varDef, _, um.getRootMapValue(inputVars, varDef.name)))
            .getOrElse(Left(Vector(UnknownVariableTypeViolation(varDef.name, QueryRenderer.render(varDef.tpe), sourceMapper, varDef.position.toList))))

          value match {
            case Right(Some(v)) ⇒ acc :+ (varDef.name → Right(v))
            case Right(None) ⇒ acc
            case Left(violations) ⇒ acc :+ (varDef.name → Left(violations))
          }
      }

      val (errors, values) = res.partition(_._2.isLeft)

      if (errors.nonEmpty) Failure(VariableCoercionError(errors.collect{case (name, Left(errors)) ⇒ errors}.flatten, exceptionHandler))
      else Success(Map(values.collect {case (name, Right(v)) ⇒ name → v}: _*))
    }

  private val emptyArgs = Success(Args.empty)

  def getFieldArgumentValues(path: ExecutionPath, argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, VariableValue]): Try[Args] =
    if(argumentDefs.isEmpty)
      emptyArgs
    else
      argumentCache.getOrElseUpdate(path.cacheKey → argumentAsts, getArgumentValues(argumentDefs, argumentAsts, variables))

  def getArgumentValues(argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument], variables: Map[String, VariableValue], ignoreErrors: Boolean = false): Try[Args] =
    if (argumentDefs.isEmpty)
      emptyArgs
    else {
      val astArgMap = argumentAsts groupBy (_.name) mapValues (_.head)
      val marshaller = CoercedScalaResultMarshaller.default
      val errors = new VectorBuilder[Violation]
      val defaultInfo = Some(TrieMap.empty[String, Any])
      val undefinedArgs = Some(new VectorBuilder[String])

      val res = argumentDefs.foldLeft(marshaller.emptyMapNode(argumentDefs.map(_.name)): marshaller.MapBuilder) {
        case (acc, argDef) ⇒
          val argPath = argDef.name :: Nil
          val astValue = astArgMap get argDef.name map (_.value)
          val fromInput = argDef.fromInput

          import sangria.marshalling.queryAst.queryAstInputUnmarshaller

          try {
            resolveMapValue(argDef.argumentType, argPath, argDef.defaultValue, argDef.name, marshaller, fromInput.marshaller, allowErrorsOnDefault = true, errors = errors, valueMap = fromInput.fromResult, defaultValueInfo = defaultInfo, undefinedValues = undefinedArgs)(
              acc, astValue map (coerceInputValue(argDef.argumentType, argPath, _, Some(variables), marshaller, fromInput.marshaller)))
          } catch {
            case InputParsingError(e) ⇒
              errors ++= e.map(InvalidInputValueViolation(argDef.name, _, sourceMapper, astValue.flatMap(_.position).toList))
              acc
          }
      }

      val errorRes = errors.result()

      if (errorRes.nonEmpty && !ignoreErrors) Failure(AttributeCoercionError(errorRes, exceptionHandler))
      else {
        val optionalArgs = argumentDefs.filter(_.argumentType.isOptional).map(_.name).toSet
        val argsWithDefault = argumentDefs.filter(_.defaultValue.isDefined).map(_.name).toSet

        Success(Args(marshaller.mapNode(res).asInstanceOf[Map[String, Any]], argsWithDefault, optionalArgs, undefinedArgs.get.result().toSet, defaultInfo.get))
      }
    }
}

case class VariableValue(fn: (ResultMarshaller, ResultMarshaller) ⇒ Either[Vector[Violation], Trinary[ResultMarshaller#Node]]) {
  private val cache = TrieMap[Int, Either[Vector[Violation], Trinary[ResultMarshaller#Node]]]()

  def resolve(marshaller: ResultMarshaller, firstKindMarshaller: ResultMarshaller): Either[Vector[Violation], Trinary[firstKindMarshaller.Node]] =
    cache.getOrElseUpdate(System.identityHashCode(firstKindMarshaller),
      fn(marshaller, firstKindMarshaller)).asInstanceOf[Either[Vector[Violation], Trinary[firstKindMarshaller.Node]]]
}