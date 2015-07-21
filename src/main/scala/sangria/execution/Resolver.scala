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
    valueExecutor: ValueExecutor[_],
    variables: Map[String, Any],
    fieldExecutor: FieldExecutor[Ctx, _],
    userContext: Ctx,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node],
    deferredResolver: DeferredResolver,
    sourceMapper: Option[SourceMapper])(implicit executionContext: ExecutionContext) {

  val resultResolver = new ResultResolver(marshaller, exceptionHandler)

  import resultResolver._

  trait Resolve

  case class DeferredResult(deferred: List[Future[List[Defer]]], futureValue: Future[Result]) extends Resolve
  case class Defer(promise: Promise[Any], deferred: Deferred[Any])
  case class Result(errors: ErrorRegistry, value: Option[marshaller.Node]) extends Resolve {
    def addToMap(other: Result, key: String, optional: Boolean) = copy(errors = errors add other.errors, value =
      if (optional && other.value.isEmpty) value else for {myVal <- value; otherVal <- other.value} yield marshaller.addMapNodeElem(myVal, key, otherVal))
  }

  def resolveFields(
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, (ast.Field, Try[List[ast.Field]])]): Future[marshaller.Node] = {
    resolveFields(Nil, tpe, value, fields, ErrorRegistry.empty) match {
      case Result(errors, data) =>
        Future.successful(
          marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]],
            marshalErrors(errors)).asInstanceOf[marshaller.Node])

      case dr: DeferredResult =>
        resolveDeferred(dr).map { case (Result(errors, data)) =>
          marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]], marshalErrors(errors)).asInstanceOf[marshaller.Node]
        }
    }
  }

  def resolveFields(
       path: List[String],
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, (ast.Field, Try[List[ast.Field]])],
       errorReg: ErrorRegistry): Resolve = {

    val (errors, res) = fields.foldLeft((errorReg, Some(Nil): Option[List[(ast.Field, Field[Ctx, _], LeafAction[Ctx, _])]])) {
      case (acc @ (_, None), _) => acc
      case (acc, (name, (origField, _))) if !tpe.fieldsByName.contains(origField.name) => acc
      case ((errors, s @ Some(acc)), (name, (origField, Failure(error)))) =>
        errors.add(path :+ name, error) -> (if (isOptional(tpe, origField.name)) s else None)
      case ((errors, s @ Some(acc)), (name, (origField, Success(fields)))) =>
        resolveField(userContext, tpe, path :+ name, value, errors, name, fields) match {
          case (updatedErrors, Some(result), _) => updatedErrors -> Some(acc :+ (fields(0), tpe.fieldsByName(origField.name), result))
          case (updatedErrors, None, _) if isOptional(tpe, origField.name) => updatedErrors -> s
          case (updatedErrors, None, _) => updatedErrors -> None
        }
    }

    res match {
      case None => Result(errors, None)
      case Some(results) =>
        val resolvedValues = results.map {
          case (astField, field, Value(v)) =>
            astField -> resolveValue(path :+ field.name, astField, field.fieldType, field, v)
          case (astField, field, DeferredValue(deferred)) =>
            val promise = Promise[Any]()

            astField -> DeferredResult(Future.successful(Defer(promise, deferred) :: Nil):: Nil,
              promise.future
                .flatMap { v =>
                  resolveValue(path :+ field.name, astField, field.fieldType, field, v) match {
                    case r: Result => Future.successful(r)
                    case er: DeferredResult => resolveDeferred(er)
                  }
                }
                .recover {
                  case e => Result(ErrorRegistry(path :+ field.name, e), None)
                })
          case (astField, field, FutureValue(future)) =>
            val resolved = future.map(v => resolveValue(path :+ field.name, astField, field.fieldType, field, v))
            val deferred = resolved flatMap {
              case r: Result => Future.successful(Nil)
              case r: DeferredResult => Future.sequence(r.deferred) map (_.flatten)
            }
            val value = resolved flatMap {
              case r: Result => Future.successful(r)
              case er: DeferredResult => er.futureValue
            }

            astField -> DeferredResult(deferred :: Nil, value)
          case (astField, field, DeferredFutureValue(deferredValue)) =>
            val promise = Promise[Any]()

            astField -> DeferredResult(deferredValue.map(d => Defer(promise, d) :: Nil) :: Nil,
              promise.future
                .flatMap{ v =>
                resolveValue(path :+ field.name, astField, field.fieldType, field, v) match {
                  case r: Result => Future.successful(r)
                  case er: DeferredResult => resolveDeferred(er)
                }
              }
              .recover{
                case e => Result(ErrorRegistry(path :+ field.name, e), None)
              })
        }

        val simpleRes = resolvedValues.collect {case (af, r: Result) => af -> r}

        val resSoFar = simpleRes.foldLeft(Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode))) {
          case (res, (astField, other)) => res addToMap (other, astField.outputName, isOptional(tpe, astField.name))
        }

        val complexRes = resolvedValues.collect{case (af, r: DeferredResult) => af -> r}

        if (complexRes.isEmpty) resSoFar
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future.sequence(complexRes.map {case (astField, DeferredResult(_, future)) =>  future map (astField -> _)}) map { results =>
            results.foldLeft(resSoFar) {
              case (res, (astField, other)) => res addToMap (other, astField.outputName, isOptional(tpe, astField.name))
            }
          }

          DeferredResult(allDeferred, finalValue)
        }
    }
  }

  def resolveDeferred(res: DeferredResult) = {
    Future.sequence(res.deferred).map { listOfDef =>
      val toResolve = listOfDef.flatten
      deferredResolver.resolve(listOfDef.flatten map (_.deferred)).onComplete {
        case Success(resolved) => toResolve zip resolved foreach {case (d, r) => d.promise.success(r)}
        case Failure(error) => toResolve foreach(_.promise.failure(error))
      }
    }

    res.futureValue
  }

  def resolveValue(
      path: List[String],
      astField: ast.Field,
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any): Resolve  =

    tpe match {
      case OptionType(optTpe) =>
        val actualValue = value.asInstanceOf[Option[_]]

        actualValue match {
          case Some(someValue) => resolveValue(path, astField, optTpe, field, someValue)
          case None => Result(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) =>
        val actualValue = value.asInstanceOf[Seq[_]]

        val res = actualValue map (resolveValue(path, astField, listTpe, field, _))

        val simpleRes = res.collect{case r: Result => r}

        if (simpleRes.size == res.size)
          simpleRes.collect{case r @ Result(errors, None) => r}.headOption match {
            case Some(r) => r
            case None => simpleRes.foldLeft(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))) {
              case (acc, Result(errors, Some(value))) => acc.copy(errors = acc.errors.add(errors), value = acc.value map (marshaller.addArrayNodeElem(_, value)))
            }
          }
        else
          res.foldLeft(DeferredResult(Nil, Future.successful(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))))) {
            case (acc, Result(errors, value)) => acc.copy(futureValue =
                acc.futureValue map (r => r.copy(errors = r.errors.add(errors), value = for {v1 <- r.value; v2 <- value} yield marshaller.addArrayNodeElem(v1, v2))))

            case (acc, DeferredResult(deferred, futureValue)) => acc.copy(deferred = acc.deferred ++ deferred, futureValue =
                for {accRes <- acc.futureValue; res <- futureValue} yield Result(accRes.errors add res.errors, for {v1 <- accRes.value; v2 <- res.value} yield marshaller.addArrayNodeElem(v1, v2)))
          }
      case scalar: ScalarType[Any @unchecked] =>
        try {
          Result(ErrorRegistry.empty, Some(marshalValue(scalar.coerceOutput(value))))
        } catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case enum: EnumType[Any @unchecked] =>
        try {
          Result(ErrorRegistry.empty, Some(marshalValue(enum.coerceOutput(value))))
        } catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case obj: ObjectType[Ctx, _] =>
        fieldExecutor.collectFields(path, obj, astField.selections) match {
          case Success(fields) => resolveFields(path, obj, value, fields, ErrorRegistry.empty)
          case Failure(error) => Result(ErrorRegistry(path, error), None)
        }
      case abst: AbstractType =>
        abst.typeOf(value, schema) match {
          case Some(obj) =>
            fieldExecutor.collectFields(path, obj, astField.selections) match {
              case Success(fields) => resolveFields(path, obj, value, fields, ErrorRegistry.empty)
              case Failure(error) => Result(ErrorRegistry(path, error), None)
            }
          case None => Result(ErrorRegistry(path,
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

  def resolveField(userCtx: Ctx, tpe: ObjectType[Ctx, _], path: List[String], value: Any, errors: ErrorRegistry, name: String, fields: List[ast.Field]): (ErrorRegistry, Option[LeafAction[Ctx, Any]], Option[Any => Ctx]) = {
    val astField = fields.head
    val field = tpe.fieldsByName(astField.name).asInstanceOf[Field[Ctx, Any]]

    valueExecutor.getAttributeValues(field.arguments, astField.arguments, variables) match {
      case Success(args) =>
        val ctx = Context[Ctx, Any](value, userCtx, args, schema.asInstanceOf[Schema[Ctx, Any]], field, fields)

        try {
          field.resolve(ctx) match {
            case resolved: LeafAction[Ctx, Any] => (errors, Some(resolved), None)
            case res: UpdateCtx[Ctx, Any @unchecked] => (errors, Some(res.action), Some(res.nextCtx))
          }
        } catch {
          case NonFatal(e) => (errors.add(path, e), None, None)
        }
      case Failure(error) => (errors.add(path, error), None, None)
    }
  }

  def isOptional(tpe: ObjectType[_, _], fieldName: String) =
    tpe.fieldsByName(fieldName).fieldType.isInstanceOf[OptionType[_]]
}