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
    valueCollector: ValueCollector[_],
    variables: Map[String, Any],
    fieldCollector: FieldCollector[Ctx, _],
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
    def addToList(other: Result, optional: Boolean) = copy(errors = errors add other.errors, value =
        if (optional && other.value.isEmpty)
          value map (marshaller.addArrayNodeElem(_, marshaller.nullNode))
        else
          for {myVal <- value; otherVal <- other.value} yield marshaller.addArrayNodeElem(myVal, otherVal))
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
    val (errors, res) = fields.foldLeft((errorReg, Some(Nil): Option[List[(List[ast.Field], Field[Ctx, _], LeafAction[Ctx, _])]])) {
      case (acc @ (_, None), _) => acc
      case (acc, (name, (origField, _))) if !tpe.fieldsByName.contains(origField.name) => acc
      case ((errors, s @ Some(acc)), (name, (origField, Failure(error)))) =>
        errors.add(path :+ name, error) -> (if (isOptional(tpe, origField.name)) s else None)
      case ((errors, s @ Some(acc)), (name, (origField, Success(fields)))) =>
        resolveField(userContext, tpe, path :+ name, value, errors, name, fields) match {
          case (updatedErrors, Some(result), _) => updatedErrors -> Some(acc :+ (fields, tpe.fieldsByName(origField.name), result))
          case (updatedErrors, None, _) if isOptional(tpe, origField.name) => updatedErrors -> s
          case (updatedErrors, None, _) => updatedErrors -> None
        }
    }

    res match {
      case None => Result(errors, None)
      case Some(results) =>
        val resolvedValues = results.map {
          case (astFields, field, Value(v)) =>
            astFields.head -> resolveValue(path :+ field.name, astFields, field.fieldType, field, v)
          case (astFields, field, DeferredValue(deferred)) =>
            val promise = Promise[Any]()

            astFields.head -> DeferredResult(Future.successful(Defer(promise, deferred) :: Nil):: Nil,
              promise.future
                .flatMap { v =>
                  resolveValue(path :+ field.name, astFields, field.fieldType, field, v) match {
                    case r: Result => Future.successful(r)
                    case er: DeferredResult => resolveDeferred(er)
                  }
                }
                .recover {
                  case e => Result(ErrorRegistry(path :+ field.name, e), None)
                })
          case (astFields, field, FutureValue(future)) =>
            val resolved = future.map(v => resolveValue(path :+ field.name, astFields, field.fieldType, field, v)).recover {
              case e => Result(ErrorRegistry(path :+ field.name, e), None)
            }
            val deferred = resolved flatMap {
              case r: Result => Future.successful(Nil)
              case r: DeferredResult => Future.sequence(r.deferred) map (_.flatten)
            }
            val value = resolved flatMap {
              case r: Result => Future.successful(r)
              case dr: DeferredResult => dr.futureValue
            }

            astFields.head -> DeferredResult(deferred :: Nil, value)
          case (astFields, field, DeferredFutureValue(deferredValue)) =>
            val promise = Promise[Any]()

            astFields.head -> DeferredResult(deferredValue.map(d => Defer(promise, d) :: Nil) :: Nil,
              promise.future
                .flatMap{ v =>
                resolveValue(path :+ field.name, astFields, field.fieldType, field, v) match {
                  case r: Result => Future.successful(r)
                  case er: DeferredResult => resolveDeferred(er)
                }
              }
              .recover{
                case e => Result(ErrorRegistry(path :+ field.name, e), None)
              })
        }

        val simpleRes = resolvedValues.collect {case (af, r: Result) => af -> r}

        val resSoFar = simpleRes.foldLeft(Result(errors, Some(marshaller.emptyMapNode))) {
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
      astFields: List[ast.Field],
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any): Resolve  =

    tpe match {
      case OptionType(optTpe) =>
        val actualValue = value match {
          case v: Option[_] => v
          case v => Option(v)
        }

        actualValue match {
          case Some(someValue) => resolveValue(path, astFields, optTpe, field, someValue)
          case None => Result(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) =>
        val actualValue = value match {
          case seq: Seq[_] => seq
          case other => Seq(other)
        }

        val res = actualValue map (resolveValue(path, astFields, listTpe, field, _))
        val simpleRes = res.collect{case r: Result => r}

        if (simpleRes.size == res.size)
          simpleRes.foldLeft(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))) {
            case (acc, res) => acc.addToList(res, isOptional(listTpe))
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
        fieldCollector.collectFields(path, obj, astFields) match {
          case Success(fields) => resolveFields(path, obj, value, fields, ErrorRegistry.empty)
          case Failure(error) => Result(ErrorRegistry(path, error), None)
        }
      case abst: AbstractType =>
        abst.typeOf(value, schema) match {
          case Some(obj) => resolveValue(path, astFields, obj, field, value)
          case None => Result(ErrorRegistry(path,
            new ExecutionError(s"Can't find appropriate subtype for field at path ${path mkString ", "}", sourceMapper, astFields.head.position)), None)
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

  def resolveField(userCtx: Ctx, tpe: ObjectType[Ctx, _], path: List[String], value: Any, errors: ErrorRegistry, name: String, astFields: List[ast.Field]): (ErrorRegistry, Option[LeafAction[Ctx, Any]], Option[Any => Ctx]) = {
    val astField = astFields.head
    val field = tpe.fieldsByName(astField.name).asInstanceOf[Field[Ctx, Any]]

    valueCollector.getArgumentValues(field.arguments, astField.arguments, variables) match {
      case Success(args) =>
        val ctx = Context[Ctx, Any](value, userCtx, args, schema.asInstanceOf[Schema[Ctx, Any]], field, astFields)

        try {
          val res = field.resolve match {
            case pfn: Projector[Ctx, _, _] => pfn(ctx, collectProjections(path, field, astFields))
            case fn => fn(ctx)
          }

          res match {
            case resolved: LeafAction[Ctx, Any] => (errors, Some(resolved), None)
            case res: UpdateCtx[Ctx, Any @unchecked] => (errors, Some(res.action), Some(res.nextCtx))
          }
        } catch {
          case NonFatal(e) => (errors.add(path, e), None, None)
        }
      case Failure(error) => (errors.add(path, error), None, None)
    }
  }

  def collectProjections(path: List[String], field: Field[Ctx, _], astFields: List[ast.Field]): Vector[ProjectedName] = {
    def loop(path: List[String], tpe: OutputType[_], astFields: List[ast.Field]): Vector[ProjectedName] = tpe match {
      case OptionType(ofType) => loop(path, ofType, astFields)
      case ListType(ofType) => loop(path, ofType, astFields)
      case objTpe: ObjectType[Ctx, _] =>
        fieldCollector.collectFields(path, objTpe, astFields) match {
          case Success(ff) =>
            ff.values.toVector collect {
              case (_, Success(fields)) if objTpe.fieldsByName.contains(fields.head.name) && objTpe.fieldsByName(fields.head.name).resolve.isInstanceOf[Projection[_, _, _]] =>
                val astField = fields.head
                val field = objTpe.fieldsByName(astField.name)
                val projection = field.resolve.asInstanceOf[Projection[_, _, _]]
                val projectedName = projection.projectedName getOrElse field.name

                ProjectedName(projectedName, loop(path :+ projectedName, field.fieldType, fields))
            }
          case Failure(_) => Vector.empty
        }
      case abst: AbstractType =>
        schema.possibleTypes
          .get (abst.name)
          .map (_.flatMap(loop(path, _, astFields)).groupBy(_.name).map(_._2.head).toVector)
          .getOrElse (Vector.empty)
      case _ => Vector.empty
    }

    loop(path, field.fieldType, astFields)
  }

  def isOptional(tpe: ObjectType[_, _], fieldName: String): Boolean =
    isOptional(tpe.fieldsByName(fieldName).fieldType)

  def isOptional(tpe: OutputType[_]): Boolean =
    tpe.isInstanceOf[OptionType[_]]
}