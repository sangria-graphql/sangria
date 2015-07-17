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
    deferredResolver: DeferredResolver,
    sourceMapper: Option[SourceMapper])(implicit executionContext: ExecutionContext) {

  trait Foo

  case class ExtractionResult(deferred: List[Future[List[Def]]], futureValue: Future[Res]) extends Foo
  case class Def(promise: Promise[Any], deferred: Deferred[Any])
  case class Res(errors: ErrorRegistry, value: Option[marshaller.Node]) extends Foo {
    def addToMap(other: Res, key: String, optional: Boolean) = copy(errors = errors add other.errors, value =
      if (optional && other.value.isEmpty) value else for {myVal <- value; otherVal <- other.value} yield marshaller.addMapNodeElem(myVal, key, otherVal))
  }

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
       fields: Map[String, (ast.Field, Try[List[ast.Field]])],
       errorReg: ErrorRegistry = ErrorRegistry.empty): Foo = {

    val (errors, res) = fields.foldLeft((errorReg, Some(Nil): Option[List[(ast.Field, Field[Ctx, _], Action[Ctx, _])]])) {
      case (acc @ (_, None), _) => acc
      case (acc, (name, (origField, _))) if !tpe.fieldsByName.contains(origField.name) => acc
      case ((errors, s @ Some(acc)), (name, (origField, Failure(error)))) =>
        errors.add(path :+ name, error) -> (if (isOptional(tpe, origField.name)) s else None)
      case ((errors, s @ Some(acc)), (name, (origField, Success(fields)))) =>
        resolveField(errors, name, fields) match {
          case (updatedErrors, Some(result)) => updatedErrors -> Some(acc :+ (fields(0), tpe.fieldsByName(origField.name), result))
          case (updatedErrors, None) if isOptional(tpe, origField.name) => updatedErrors -> s
          case (updatedErrors, None) => updatedErrors -> None
        }
    }

    res match {
      case None => Res(errors, None)
      case Some(results) =>
        val resolvedValues = results.map {
          // todo updatectx
          case (astField, field, Value(value)) =>
            astField -> resolveValue(path :+ field.name, astField, field.fieldType, field, value)
          case (astField, field, DeferredValue(deferred)) =>
            val promise = Promise[Any]()

            astField -> ExtractionResult(Future.successful(Def(promise, deferred) :: Nil):: Nil,
              promise.future
                .flatMap{ v =>
                  resolveValue(path :+ field.name, astField, field.fieldType, field, v) match {
                    case r: Res => Future.successful(r)
                    case er: ExtractionResult => resolveDeferred(er)
                  }
                }
                .recover{
                  case e => Res(ErrorRegistry.empty.add(path :+ field.name, e), None)
                })
          case (astField, field, FutureValue(future)) =>
            val resolved = future.map(v => resolveValue(path :+ field.name, astField, field.fieldType, field, v))
            val deferred = resolved flatMap {
              case r: Res => Future.successful(Nil)
              case r: ExtractionResult => Future.sequence(r.deferred) map (_.flatten)
            }
            val value = resolved flatMap {
              case r: Res => Future.successful(r)
              case er: ExtractionResult => er.futureValue
            }

            astField -> ExtractionResult(deferred :: Nil, value)
          case (astField, field, DeferredFutureValue(deferredValue)) =>
            val promise = Promise[Any]()

            astField -> ExtractionResult(deferredValue.map(d => Def(promise, d) :: Nil) :: Nil,
              promise.future
                .flatMap{ v =>
                resolveValue(path :+ field.name, astField, field.fieldType, field, v) match {
                  case r: Res => Future.successful(r)
                  case er: ExtractionResult => resolveDeferred(er)
                }
              }
              .recover{
                case e => Res(ErrorRegistry.empty.add(path :+ field.name, e), None)
              })
        }

        val simpleRes = resolvedValues.collect {case (af, r: Res) => af -> r}

        val resSoFar = simpleRes.foldLeft(Res(ErrorRegistry.empty, Some(marshaller.emptyMapNode))) {
          case (res, (astField, other)) => res addToMap (other, astField.outputName, isOptional(tpe, astField.name))
        }

        val complexRes = resolvedValues.collect{case (af, r: ExtractionResult) => af -> r}

        if (complexRes.isEmpty) resSoFar
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future.sequence(complexRes.map {case (astField, ExtractionResult(_, future)) =>  future map (astField -> _)}) map { results =>
            results.foldLeft(resSoFar) {
              case (res, (astField, other)) => res addToMap (other, astField.outputName, isOptional(tpe, astField.name))
            }
          }

          ExtractionResult(allDeferred, finalValue)
        }
    }
  }

  def resolveDeferred(res: ExtractionResult) = {
    val foo = Future.sequence(res.deferred).map(listOfDef => ???)

    res.futureValue
  }

  def resolveValue(
      path: List[String],
      astField: ast.Field,
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any): Foo  =

    field.fieldType match {
      case OptionType(optTpe) =>
        val actualValue = value.asInstanceOf[Option[_]]

        actualValue match {
          case Some(someValue) => resolveValue(path, astField, optTpe, field, someValue)
          case None => Res(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) =>
        val actualValue = value.asInstanceOf[Seq[_]]

        val res = actualValue map (resolveValue(path, astField, listTpe, field, _))

        val simpleRes = res.collect{case r: Res => r}

        if (simpleRes.size == res.size)
          simpleRes.collect{case r @ Res(errors, None) => r}.headOption match {
            case Some(r) => r
            case None => simpleRes.foldLeft(Res(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))) {
              case (acc, Res(errors, Some(value))) => acc.copy(errors = acc.errors.add(errors), value = acc.value map (marshaller.addArrayNodeElem(_, value)))
            }
          }
        else
          res.foldLeft(ExtractionResult(Nil, Future.successful(Res(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))))) {
            case (acc, Res(errors, value)) => acc.copy(futureValue =
                acc.futureValue map (r => r.copy(errors = r.errors.add(errors), value = for {v1 <- r.value; v2 <- value} yield marshaller.addArrayNodeElem(v1, v2))))

            case (acc, ExtractionResult(deferred, futureValue)) => acc.copy(deferred = acc.deferred ++ deferred, futureValue =
                for {accRes <- acc.futureValue; res <- futureValue} yield Res(accRes.errors add res.errors, for {v1 <- accRes.value; v2 <- res.value} yield marshaller.addArrayNodeElem(v1, v2)))
          }
      case scalar: ScalarType[Any @unchecked] =>
        try {
          Res(ErrorRegistry.empty, Some(marshalValue(scalar.coerceOutput(value))))
        } catch {
          case NonFatal(e) => Res(ErrorRegistry.empty.add(path, e), None)
        }
      case enum: EnumType[Any @unchecked] =>
        try {
          Res(ErrorRegistry.empty, Some(marshalValue(enum.coerceOutput(value))))
        } catch {
          case NonFatal(e) => Res(ErrorRegistry.empty.add(path, e), None)
        }
      case obj: ObjectType[Ctx, _] =>
        fieldExecutor.collectFields(path, obj, astField.selections) match {
          case Success(fields) => executeFields(path, obj, value, fields)
          case Failure(error) => Res(ErrorRegistry.empty.add(path, error), None)
        }
      case abst: AbstractType =>
        abst.typeOf(value, schema) match {
          case Some(obj) =>
            fieldExecutor.collectFields(path, obj, astField.selections) match {
              case Success(fields) => executeFields(path, obj, value, fields)
              case Failure(error) => Res(ErrorRegistry.empty.add(path, error), None)
            }
          case None => Res(ErrorRegistry.empty.add(path,
            new ExecutionError(s"Can't find appropriate subtype for field at path ${path mkString ", "}", sourceMapper, astField.position)), None)
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

  def resolveField(errors: ErrorRegistry, name: String, fields: List[ast.Field]): (ErrorRegistry, Option[Action[Ctx, _]]) = ???

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