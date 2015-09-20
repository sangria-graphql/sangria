package sangria.execution

import org.parboiled2.Position
import sangria.ast
import sangria.integration.ResultMarshaller
import sangria.parser.SourceMapper
import sangria.schema._

import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.util.control.NonFatal
import scala.util.{Success, Failure, Try}

class Resolver[Ctx](
    val marshaller: ResultMarshaller,
    schema: Schema[Ctx, _],
    valueCollector: ValueCollector[Ctx, _],
    variables: Map[String, Any],
    fieldCollector: FieldCollector[Ctx, _],
    userContext: Ctx,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException],
    deferredResolver: DeferredResolver,
    sourceMapper: Option[SourceMapper],
    deprecationTracker: DeprecationTracker)(implicit executionContext: ExecutionContext) {

  val resultResolver = new ResultResolver(marshaller, exceptionHandler)

  import resultResolver._
  import Resolver._

  def resolveFieldsPar(
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, (ast.Field, Try[List[ast.Field]])]): Future[marshaller.Node] = {
    val actions = collectActionsPar(Nil, tpe, value, fields, ErrorRegistry.empty, userContext)

    processFinalResolve(resolveActionsPar(Nil, tpe, actions, userContext))
  }

  def resolveFieldsSeq(
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, (ast.Field, Try[List[ast.Field]])]): Future[marshaller.Node] = {
    val actions = resolveSeq(Nil, tpe, value, fields, ErrorRegistry.empty)

    actions flatMap processFinalResolve
  }

  def processFinalResolve(resolve: Resolve) = resolve match {
    case Result(errors, data) =>
      Future.successful(
        marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]],
          marshalErrors(errors)).asInstanceOf[marshaller.Node])

    case dr: DeferredResult =>
      resolveDeferred(dr).map { case (Result(errors, data)) =>
        marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]], marshalErrors(errors)).asInstanceOf[marshaller.Node]
      }
  }

  private type Actions = (ErrorRegistry, Option[List[(List[ast.Field], Option[(Field[Ctx, _], Option[MappedCtxUpdate[Ctx, Any, Any]], LeafAction[Ctx, _])])]])

  def resolveSeq(
      path: List[String],
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: Map[String, (ast.Field, Try[List[ast.Field]])],
      errorReg: ErrorRegistry): Future[Result] = {
    fields.foldLeft(Future.successful((Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode)), userContext))) {
      case (future, elem) => future.flatMap { resAndCtx =>
        (resAndCtx, elem) match {
          case (acc @ (Result(_, None), _), _) => Future.successful(acc)
          case (acc, (name, (origField, _))) if tpe.getField(schema, origField.name).isEmpty => Future.successful(acc)
          case ((Result(errors, s @ Some(acc)), uc), (name, (origField, Failure(error)))) =>
            Future.successful(Result(errors.add(path :+ name, error), if (isOptional(tpe, origField.name)) Some(marshaller.addMapNodeElem(acc, origField.outputName, marshaller.nullNode)) else None) -> uc)
          case ((accRes @ Result(errors, s @ Some(acc)), uc), (name, (origField, Success(fields)))) =>
            resolveField(uc, tpe, path :+ name, value, errors, name, fields) match {
              case (updatedErrors, None, _) if isOptional(tpe, origField.name) =>
                Future.successful(Result(updatedErrors, Some(marshaller.addMapNodeElem(acc, fields.head.outputName, marshaller.nullNode))) -> uc)
              case (updatedErrors, None, _) => Future.successful(Result(updatedErrors, None), uc)
              case (updatedErrors, Some(result), newUc) =>
                val sfield = tpe.getField(schema, origField.name).get

                def resolveUc(v: Any) = newUc map (_.ctxFn(v)) getOrElse uc

                def resolveVal(v: Any) = newUc match {
                  case Some(MappedCtxUpdate(_, mapFn)) => mapFn(v)
                  case None => v
                }

                val resolve = result match {
                  case Value(v) =>
                    Future.successful(resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) -> resolveUc(v))
                  case DeferredValue(d) =>
                    val p = Promise[Any]()

                    resolveDeferred(DeferredResult(Future.successful(Defer(p, d) :: Nil) :: Nil, p.future.flatMap { v =>
                      resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) match {
                        case r: Result => Future.successful(r)
                        case er: DeferredResult => resolveDeferred(er)
                      }
                    }.recover {
                      case e => Result(ErrorRegistry(path :+ fields.head.outputName, e, fields.head.position), None)
                    })).flatMap(r => p.future map (r -> resolveUc(_)) recover {case _ => r -> uc})
                  case FutureValue(f) =>
                    f.map (v => resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) -> resolveUc(v))
                      .recover{case e => Result(errors.add(path :+ name, e, fields.head.position), None) -> uc}
                  case DeferredFutureValue(df) =>
                    val p = Promise[Any]()

                    resolveDeferred(DeferredResult(df.map(Defer(p, _) :: Nil) :: Nil, p.future.flatMap { v =>
                      resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) match {
                        case r: Result => Future.successful(r)
                        case er: DeferredResult => resolveDeferred(er)
                      }
                    }.recover {
                      case e => Result(ErrorRegistry(path :+ fields.head.outputName, e, fields.head.position), None)
                    })).flatMap(r => p.future map (r -> resolveUc(_)) recover {case _ => r -> uc})
                }

                resolve.flatMap {
                  case (r : Result, newUc) =>
                    Future.successful(accRes.addToMap(r, fields.head.outputName, isOptional(tpe, fields.head.name), path :+ fields.head.outputName, fields.head.position) -> newUc)
                  case (dr : DeferredResult, newUc) =>
                    resolveDeferred(dr) map (accRes.addToMap(_, fields.head.outputName, isOptional(tpe, fields.head.name), path :+ fields.head.outputName, fields.head.position) -> newUc)
                }
            }
        }
      }
    }.map(_._1)
  }

  def collectActionsPar(
      path: List[String],
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: Map[String, (ast.Field, Try[List[ast.Field]])],
      errorReg: ErrorRegistry,
      userCtx: Ctx): Actions =
    fields.foldLeft((errorReg, Some(Nil)): Actions) {
      case (acc @ (_, None), _) => acc
      case (acc, (name, (origField, _))) if tpe.getField(schema, origField.name).isEmpty => acc
      case ((errors, s @ Some(acc)), (name, (origField, Failure(error)))) =>
        errors.add(path :+ name, error) -> (if (isOptional(tpe, origField.name)) Some(acc :+ (origField :: Nil, None)) else None)
      case ((errors, s @ Some(acc)), (name, (origField, Success(fields)))) =>
        resolveField(userCtx, tpe, path :+ name, value, errors, name, fields) match {
          case (updatedErrors, Some(result), updateCtx) => updatedErrors -> Some(acc :+ (fields, Some((tpe.getField(schema, origField.name).get, updateCtx, result))))
          case (updatedErrors, None, _) if isOptional(tpe, origField.name) => updatedErrors -> Some(acc :+ (origField :: Nil, None))
          case (updatedErrors, None, _) => updatedErrors -> None
        }
    }

  def resolveActionsPar(path: List[String], tpe: ObjectType[Ctx, _], actions: Actions, userCtx: Ctx): Resolve = {
    val (errors, res) = actions

    def resolveUc(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any) = newUc map (_.ctxFn(v)) getOrElse userCtx

    def resolveVal(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any) = newUc match {
      case Some(MappedCtxUpdate(_, mapFn)) => mapFn(v)
      case None => v
    }

    res match {
      case None => Result(errors, None)
      case Some(results) =>
        val resolvedValues = results.map {
          case (astFields, None) => astFields.head -> Result(ErrorRegistry.empty, None)
          case (astFields, Some((field, updateCtx, Value(v)))) =>
            astFields.head -> resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))
          case (astFields, Some((field, updateCtx, DeferredValue(deferred)))) =>
            val promise = Promise[Any]()

            astFields.head -> DeferredResult(Future.successful(Defer(promise, deferred) :: Nil):: Nil,
              promise.future
                .flatMap { v =>
                  resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v)) match {
                    case r: Result => Future.successful(r)
                    case er: DeferredResult => resolveDeferred(er)
                  }
                }
                .recover {
                  case e => Result(ErrorRegistry(path :+ astFields.head.outputName, e, astFields.head.position), None)
                })
          case (astFields, Some((field, updateCtx, FutureValue(future)))) =>
            val resolved = future.map(v => resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))).recover {
              case e => Result(ErrorRegistry(path :+ astFields.head.outputName, e, astFields.head.position), None)
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
          case (astFields, Some((field, updateCtx, DeferredFutureValue(deferredValue)))) =>
            val promise = Promise[Any]()

            astFields.head -> DeferredResult(deferredValue.map(d => Defer(promise, d) :: Nil) :: Nil,
              promise.future
                .flatMap{ v =>
                resolveValue(path :+ field.name, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v)) match {
                  case r: Result => Future.successful(r)
                  case er: DeferredResult => resolveDeferred(er)
                }
              }
              .recover{
                case e => Result(ErrorRegistry(path :+ field.name, e, astFields.head.position), None)
              })
        }

        val simpleRes = resolvedValues.collect {case (af, r: Result) => af -> r}

        val resSoFar = simpleRes.foldLeft(Result(errors, Some(marshaller.emptyMapNode))) {
          case (res, (astField, other)) => res addToMap (other, astField.outputName, isOptional(tpe, astField.name), path :+ astField.outputName, astField.position)
        }

        val complexRes = resolvedValues.collect{case (af, r: DeferredResult) => af -> r}

        if (complexRes.isEmpty) resSoFar
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future.sequence(complexRes.map {case (astField, DeferredResult(_, future)) =>  future map (astField -> _)}) map { results =>
            results.foldLeft(resSoFar) {
              case (res, (astField, other)) => res addToMap (other, astField.outputName, isOptional(tpe, astField.name), path :+ astField.outputName, astField.position)
            }
          }

          DeferredResult(allDeferred, finalValue)
        }
    }
  }

  def resolveDeferred(res: DeferredResult) =
    Future.sequence(res.deferred).flatMap { listOfDef =>
      val toResolve = listOfDef.flatten

      def findActualDeferred(deferred: Deferred[_]): Deferred[_] = deferred match {
        case MappingDeferred(d, _) => findActualDeferred(d)
        case d => d
      }

      def mapAllDeferred(deferred: Deferred[_], value: Future[Any]): Future[Any] = deferred match {
        case MappingDeferred(d, fn) => mapAllDeferred(d, value) map (fn)
        case d => value
      }

      try {
        val resolved = deferredResolver.resolve(toResolve map (d => findActualDeferred(d.deferred)))

        toResolve zip resolved foreach {
          case (toRes, f) => toRes.promise tryCompleteWith (mapAllDeferred(toRes.deferred, f))
        }
      } catch {
        case NonFatal(error) => toResolve foreach (_.promise.failure(error))
      }

      res.futureValue
    }

  def resolveValue(
      path: List[String],
      astFields: List[ast.Field],
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any,
      userCtx: Ctx): Resolve  =
    tpe match {
      case OptionType(optTpe) =>
        val actualValue = value match {
          case v: Option[_] => v
          case v => Option(v)
        }

        actualValue match {
          case Some(someValue) => resolveValue(path, astFields, optTpe, field, someValue, userCtx)
          case None => Result(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) =>
        if (value == null)
          Result(ErrorRegistry.empty, None)
        else {
          val actualValue = value match {
            case seq: Seq[_] => seq
            case other => Seq(other)
          }

          val res = actualValue map (resolveValue(path, astFields, listTpe, field, _, userCtx))
          val simpleRes = res.collect { case r: Result => r}

          if (simpleRes.size == res.size)
            simpleRes.foldLeft(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))) {
              case (acc, res) => acc.addToList(res, isOptional(listTpe), path, astFields.head.position)
            }
          else
            res.foldLeft(DeferredResult(Nil, Future.successful(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))))) {
              case (acc, res: Result) => acc.copy(futureValue = acc.futureValue map (r => r.addToList(res, isOptional(listTpe), path, astFields.head.position)))

              case (acc, DeferredResult(deferred, futureValue)) => acc.copy(deferred = acc.deferred ++ deferred, futureValue =
                  for {accRes <- acc.futureValue; res <- futureValue} yield accRes.addToList(res, isOptional(listTpe), path, astFields.head.position))
            }
        }
      case scalar: ScalarType[Any @unchecked] =>
        try {
          Result(ErrorRegistry.empty, if (value == null) None else Some(marshalValue(scalar.coerceOutput(value), marshaller)))
        } catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case enum: EnumType[Any @unchecked] =>
        try {
          Result(ErrorRegistry.empty, if (value == null) None else Some(marshalValue(enum.coerceOutput(value), marshaller)))
        } catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case obj: ObjectType[Ctx, _] =>
        fieldCollector.collectFields(path, obj, astFields) match {
          case Success(fields) =>
            val actions = collectActionsPar(path, obj, value, fields, ErrorRegistry.empty, userCtx)

            resolveActionsPar(path, obj, actions, userCtx)
          case Failure(error) => Result(ErrorRegistry(path, error), None)
        }
      case abst: AbstractType =>
        abst.typeOf(value, schema) match {
          case Some(obj) => resolveValue(path, astFields, obj, field, value, userCtx)
          case None => Result(ErrorRegistry(path,
            new ExecutionError(s"Can't find appropriate subtype for field at path ${path mkString ", "}", sourceMapper, astFields.head.position.toList)), None)
        }
    }

  def resolveField(userCtx: Ctx, tpe: ObjectType[Ctx, _], path: List[String], value: Any, errors: ErrorRegistry, name: String, astFields: List[ast.Field]): (ErrorRegistry, Option[LeafAction[Ctx, Any]], Option[MappedCtxUpdate[Ctx, Any, Any]]) = {
    val astField = astFields.head
    val field = tpe.getField(schema, astField.name).get.asInstanceOf[Field[Ctx, Any]]

    field.deprecationReason foreach (_ => deprecationTracker.deprecatedFieldUsed(path, field, userCtx))

    valueCollector.getArgumentValues(field.arguments, astField.arguments, variables) match {
      case Success(args) =>
        val ctx = Context[Ctx, Any](
          value,
          userCtx,
          args,
          schema.asInstanceOf[Schema[Ctx, Any]],
          field,
          tpe.asInstanceOf[ObjectType[Ctx, Any]],
          marshaller,
          astFields)

        try {
          val res = field.resolve match {
            case pfn: Projector[Ctx, _, _] => pfn(ctx, collectProjections(path, field, astFields, pfn.maxLevel))
            case pfn: Projection[Ctx, _, _] =>
              pfn.fn match {
                case projectorFn: Projector[Ctx, _, _] => projectorFn(ctx, collectProjections(path, field, astFields, projectorFn.maxLevel))
                case _ => pfn(ctx)
              }
            case fn => fn(ctx)
          }

          res match {
            case resolved: LeafAction[Ctx, Any] =>
              (errors, Some(resolved), None)
            case res: UpdateCtx[Ctx, Any] =>
              (errors, Some(res.action), Some(MappedCtxUpdate(res.nextCtx, identity)))
            case res: MappedUpdateCtx[Ctx, Any @unchecked, Any @unchecked] =>
              (errors, Some(res.action), Some(MappedCtxUpdate(res.nextCtx, res.mapFn)))
          }
        } catch {
          case NonFatal(e) => (errors.add(path, e, astField.position), None, None)
        }
      case Failure(error) => (errors.add(path, error), None, None)
    }
  }

  def collectProjections(path: List[String], field: Field[Ctx, _], astFields: List[ast.Field], maxLevel: Int): Vector[ProjectedName] = {
    def loop(path: List[String], tpe: OutputType[_], astFields: List[ast.Field], currLevel: Int): Vector[ProjectedName] =
      if (currLevel > maxLevel) Vector.empty
      else tpe match {
        case OptionType(ofType) => loop(path, ofType, astFields, currLevel)
        case ListType(ofType) => loop(path, ofType, astFields, currLevel)
        case objTpe: ObjectType[Ctx, _] =>
          fieldCollector.collectFields(path, objTpe, astFields) match {
            case Success(ff) =>
              ff.values.toVector collect {
                case (_, Success(fields)) if objTpe.getField(schema, fields.head.name).isDefined && !objTpe.getField(schema, fields.head.name).get.resolve.isInstanceOf[NoProjection[_, _, _]] =>
                  val astField = fields.head
                  val field = objTpe.getField(schema, astField.name).get
                  val projectedName = field.resolve match {
                    case proj: Projection[_, _, _] => proj.projectedName
                    case _ => field.name
                  }

                  ProjectedName(projectedName, loop(path :+ projectedName, field.fieldType, fields, currLevel + 1))
              }
            case Failure(_) => Vector.empty
          }
        case abst: AbstractType =>
          schema.possibleTypes
            .get (abst.name)
            .map (_.flatMap(loop(path, _, astFields, currLevel + 1)).groupBy(_.name).map(_._2.head).toVector)
            .getOrElse (Vector.empty)
        case _ => Vector.empty
      }

    loop(path, field.fieldType, astFields, 1)
  }

  def isOptional(tpe: ObjectType[_, _], fieldName: String): Boolean =
    isOptional(tpe.getField(schema, fieldName).get.fieldType)

  def isOptional(tpe: OutputType[_]): Boolean =
    tpe.isInstanceOf[OptionType[_]]

  trait Resolve

  case class DeferredResult(deferred: List[Future[List[Defer]]], futureValue: Future[Result]) extends Resolve
  case class Defer(promise: Promise[Any], deferred: Deferred[Any])
  case class Result(errors: ErrorRegistry, value: Option[marshaller.Node]) extends Resolve {
    def addToMap(other: Result, key: String, optional: Boolean, path: List[String], position: Option[Position]) =
      copy(
        errors =
            if (!optional && other.value.isEmpty && other.errors.errorList.isEmpty)
              errors.add(other.errors).add(path, new ExecutionError("Cannot return null for non-nullable type", sourceMapper, position.toList))
            else
              errors.add(other.errors),
        value =
            if (optional && other.value.isEmpty)
              value map (marshaller.addMapNodeElem(_, key, marshaller.nullNode))
            else
              for {myVal <- value; otherVal <- other.value} yield marshaller.addMapNodeElem(myVal, key, otherVal))

    def addToList(other: Result, optional: Boolean, path: List[String], position: Option[Position]) = copy(
      errors =
          if (!optional && other.value.isEmpty && other.errors.errorList.isEmpty)
            errors.add(other.errors).add(path, new ExecutionError("Cannot return null for non-nullable type", sourceMapper, position.toList))
          else
            errors.add(other.errors),
      value =
          if (optional && other.value.isEmpty)
            value map (marshaller.addArrayNodeElem(_, marshaller.nullNode))
          else
            for {myVal <- value; otherVal <- other.value} yield marshaller.addArrayNodeElem(myVal, otherVal))
  }
}

case class MappedCtxUpdate[Ctx, Val, NewVal](ctxFn: Val => Ctx, mapFn: Val => NewVal)

object Resolver {
  def marshalValue(value: ast.Value, marshaller: ResultMarshaller): marshaller.Node = value match {
    case ast.StringValue(str, _) => marshaller.stringNode(str)
    case ast.IntValue(i, _) => marshaller.intNode(i)
    case ast.BigIntValue(i, _) => marshaller.bigIntNode(i)
    case ast.FloatValue(f, _) => marshaller.floatNode(f)
    case ast.BigDecimalValue(f, _) => marshaller.bigDecimalNode(f)
    case ast.BooleanValue(b, _) => marshaller.booleanNode(b)
    case ast.EnumValue(enum, _) => marshaller.stringNode(enum)
    case ast.ListValue(values, _) => marshaller.arrayNode(values map (marshalValue(_, marshaller)))
    case ast.ObjectValue(values, _) => marshaller.mapNode(values map (v => v.name -> marshalValue(v.value, marshaller)))
    case ast.VariableValue(_, _) => throw new IllegalStateException("Can't marshall variable values!")
  }
}