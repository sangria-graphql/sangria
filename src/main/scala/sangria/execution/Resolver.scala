package sangria.execution

import sangria.ast
import sangria.parser.SourceMapper
import sangria.schema._

import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.util.control.NonFatal
import scala.util.{Success, Failure, Try}

class Resolver[Ctx](
    val marshaller: ResultMarshaller,
    schema: Schema[Ctx, _],
    fieldExecutor: FieldExecutor[Ctx, _],
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node],
    sourceMapper: Option[SourceMapper])(implicit executionContext: ExecutionContext) {

  def handleException(exception: Throwable): marshaller.Node = exception match {
    case e: UserFacingError => marshaller.stringNode(e.getMessage)
    case e if exceptionHandler isDefinedAt (marshaller -> e) => exceptionHandler(marshaller -> e).asInstanceOf[marshaller.Node]
    case e =>
      e.printStackTrace() // todo proper logging?
      marshaller.stringNode("Internal server error")
  }

  def executeFields(
       path: List[String],
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, Try[List[ast.Field]]],
       errorReg: ErrorRegistry = ErrorRegistry.empty): (ErrorRegistry, List[DeferResolve], Option[Future[(ErrorRegistry, marshaller.Node)]]) = {

    val (errors, res) = fields.foldLeft((errorReg, Some(Nil): Option[List[(ast.Field, Field[Ctx, _], ResolveResult)]])) {
      case (acc @ (_, None), _) => acc
      case (acc, (name, _)) if !tpe.fieldsByName.contains(name) => acc
      case ((errors, s @ Some(acc)), (name, Failure(error))) =>
        errors.add(path :+ name, error) -> (if (isOptional(tpe, name)) s else None)
      case ((errors, s @ Some(acc)), (name, Success(fields))) =>
        resolveField(errors, name, fields) match {
          case (updatedErrors, Some(result)) => updatedErrors -> Some(acc :+ (fields(0), tpe.fieldsByName(name), result))
          case (updatedErrors, None) if isOptional(tpe, name) => updatedErrors -> s
          case (updatedErrors, None) => updatedErrors -> None
        }
    }

    res match {
      case None => (errors, Nil, None)
      case Some(results) =>
        results.foldLeft((errors, List[DeferResolve](), List[Future[Any]](), Map.empty[String, marshaller.Node])) {
          case ((err, deferred, futures, acc), (astField, field, EagerResolve(value))) =>
            val fieldPath = path :+ field.name
            resolveValue(fieldPath, astField, field.fieldType, field, value, err)
            ???
        }
        //        val f = Future.sequence(fields)
        ???
    }
  }

  def resolveValue(
      path: List[String],
      astField: ast.Field,
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any,
      errorReg: ErrorRegistry): (ErrorRegistry, List[DeferResolve], Option[Future[(ErrorRegistry, marshaller.Node)]])  =

    field.fieldType match {
      case OptionType(optTpe) =>
        val actualValue = value.asInstanceOf[Option[_]]

        actualValue match {
          case Some(someValue) => resolveValue(path, astField, optTpe, field, someValue, errorReg)
          case None => (errorReg, Nil, None)
        }
      case ListType(listTpe) =>
        val actualValue = value.asInstanceOf[Seq[_]]

        val (errors, deferred, maybeFutures) = actualValue.foldLeft((errorReg, Nil: List[DeferResolve], Some(Nil): Option[List[Future[(ErrorRegistry, marshaller.Node)]]])) {
          case (acc @ (elemErrors, elemDeferred, None), elem) => acc
          case ((elemErrors, elemDeferred, Some(futures)), elem) =>
            val (newErrors, newDeferred, maybeFuture) = resolveValue(path, astField, listTpe, field, elem, elemErrors)

            maybeFuture match {
              case Some(f) => (newErrors, elemDeferred ++ newDeferred, Some(futures :+ f))
              case None => (newErrors, elemDeferred ++ newDeferred, None)
            }
        }

        maybeFutures match {
          case Some(futures) => (errors, deferred, Some(Future.sequence(futures).map { results =>
            val (errors, res) = results.foldLeft((ErrorRegistry.empty, Nil: List[marshaller.Node])) {
              case ((accErrors, accValues), (errors, value)) => accErrors.add(errors) -> (accValues :+ value)
            }

            errors -> marshaller.arrayNode(res)
          }))
          case None => (errors, Nil, None)
        }
      case scalar: ScalarType[Any @unchecked] =>
        try {
          (errorReg, Nil, Some(Future.successful(ErrorRegistry.empty -> marshalValue(scalar.coerceOutput(value)))))
        } catch {
          case NonFatal(e) => (errorReg.add(path, e), Nil, None)
        }
      case enum: EnumType[Any @unchecked] =>
        try {
          (errorReg, Nil, Some(Future.successful(ErrorRegistry.empty -> marshalValue(enum.coerceOutput(value)))))
        } catch {
          case NonFatal(e) => (errorReg.add(path, e), Nil, None)
        }
      case obj: ObjectType[Ctx, _] =>
        fieldExecutor.collectFields(path, obj, astField.selections) match {
          case Success(fields) => executeFields(path, obj, value, fields, errorReg)
          case Failure(error) => (errorReg.add(path, error), Nil, None)
        }
      case abst: AbstractType =>
        abst.typeOf(value, schema) match {
          case Some(obj) =>
            fieldExecutor.collectFields(path, obj, astField.selections) match {
              case Success(fields) => executeFields(path, obj, value, fields, errorReg)
              case Failure(error) => (errorReg.add(path, error), Nil, None)
            }
          case None => (errorReg.add(path,
            new ExecutionError(s"Can't find appropriate subtype for field at path ${path mkString ", "}", sourceMapper, astField.position)), Nil, None)
        }

    }

  def marshalValue(value: ast.Value): marshaller.Node = value match {
    case ast.StringValue(str, _) => marshaller.stringNode(str)
    case ast.IntValue(i, _) => marshaller.intNode(i)
    case ast.FloatValue(f, _) => marshaller.floatNode(f)
    case ast.BooleanValue(b, _) => marshaller.booleanNode(b)
    case ast.EnumValue(enum, _) => marshaller.stringNode(enum)
    case ast.ArrayValue(values, _) => marshaller.arrayNode(values map (marshalValue(_)))
    case ast.ObjectValue(values, _) => marshaller.mapNode(values map (v => v.name -> marshalValue(v.value)))
    case ast.VariableValue(_, _) => throw new IllegalStateException("Can't marshall variable values!")
  }

  def resolveField(errors: ErrorRegistry, name: String, fields: List[ast.Field]): (ErrorRegistry, Option[ResolveResult]) = ???

  def isOptional(tpe: ObjectType[_, _], fieldName: String) =
    tpe.fieldsByName(fieldName).fieldType.isInstanceOf[OptionType[_]]

  case class ErrorRegistry(errorList: List[ErrorPath]) {
    def add(path: List[String], error: String) = copy(errorList:+ ErrorPath(path, marshaller.stringNode(error)))
    def add(path: List[String], error: Throwable) = copy(errorList :+ ErrorPath(path, handleException(error)))
    def add(other: ErrorRegistry) = ErrorRegistry(errorList ++ other.errorList)
  }

  object ErrorRegistry {
    val empty = ErrorRegistry(Nil)
  }

  case class ErrorPath(path: List[String], error: marshaller.Node)
}

sealed trait ResolveResult
case class EagerResolve(value: Any) extends ResolveResult
case class FutureResolve(value: Future[Any]) extends ResolveResult
case class DeferResolve(value: Deferred[Any], promise: Promise[Any]) extends ResolveResult