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
    middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
    schema: Schema[Ctx, _],
    valueCollector: ValueCollector[Ctx, _],
    variables: Map[String, Any],
    fieldCollector: FieldCollector[Ctx, _],
    userContext: Ctx,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException],
    deferredResolver: DeferredResolver[Ctx],
    sourceMapper: Option[SourceMapper],
    deprecationTracker: DeprecationTracker,
    middleware: List[(Any, Middleware[_])],
    maxQueryDepth: Option[Int])(implicit executionContext: ExecutionContext) {

  val resultResolver = new ResultResolver(marshaller, exceptionHandler)

  import resultResolver._
  import Resolver._

  def resolveFieldsPar(
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, (ast.Field, Try[Vector[ast.Field]])]): Future[marshaller.Node] = {
    val actions = collectActionsPar(Vector.empty, tpe, value, fields, ErrorRegistry.empty, userContext)

    processFinalResolve(resolveActionsPar(Vector.empty, tpe, actions, userContext))
  }

  def resolveFieldsSeq(
       tpe: ObjectType[Ctx, _],
       value: Any,
       fields: Map[String, (ast.Field, Try[Vector[ast.Field]])]): Future[marshaller.Node] = {
    val actions = resolveSeq(Vector.empty, tpe, value, fields, ErrorRegistry.empty)

    actions flatMap processFinalResolve
  }

  def processFinalResolve(resolve: Resolve) = resolve match {
    case Result(errors, data) ⇒
      Future.successful(
        marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]],
          marshalErrors(errors)).asInstanceOf[marshaller.Node])

    case dr: DeferredResult ⇒
      resolveDeferred(userContext, dr).map { case (Result(errors, data)) ⇒
        marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]], marshalErrors(errors)).asInstanceOf[marshaller.Node]
      }
  }

  private type Actions = (ErrorRegistry, Option[Vector[(Vector[ast.Field], Option[(Field[Ctx, _], Option[MappedCtxUpdate[Ctx, Any, Any]], LeafAction[Ctx, _])])]])

  def resolveSeq(
      path: Vector[String],
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: Map[String, (ast.Field, Try[Vector[ast.Field]])],
      errorReg: ErrorRegistry): Future[Result] = {
    fields.foldLeft(Future.successful((Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode)), userContext))) {
      case (future, elem) ⇒ future.flatMap { resAndCtx ⇒
        (resAndCtx, elem) match {
          case (acc @ (Result(_, None), _), _) ⇒ Future.successful(acc)
          case (acc, (name, (origField, _))) if tpe.getField(schema, origField.name).isEmpty ⇒ Future.successful(acc)
          case ((Result(errors, s @ Some(acc)), uc), (name, (origField, Failure(error)))) ⇒
            Future.successful(Result(errors.add(path :+ name, error), if (isOptional(tpe, origField.name)) Some(marshaller.addMapNodeElem(acc, origField.outputName, marshaller.nullNode)) else None) → uc)
          case ((accRes @ Result(errors, s @ Some(acc)), uc), (name, (origField, Success(fields)))) ⇒
            resolveField(uc, tpe, path :+ name, value, errors, name, fields) match {
              case (updatedErrors, None, _) if isOptional(tpe, origField.name) ⇒
                Future.successful(Result(updatedErrors, Some(marshaller.addMapNodeElem(acc, fields.head.outputName, marshaller.nullNode))) → uc)
              case (updatedErrors, None, _) ⇒ Future.successful(Result(updatedErrors, None), uc)
              case (updatedErrors, Some(result), newUc) ⇒
                val sfield = tpe.getField(schema, origField.name).head

                def resolveUc(v: Any) = newUc map (_.ctxFn(v)) getOrElse uc

                def resolveError(e: Throwable) = {
                  try {
                    newUc map (_.onError(e))
                  } catch {
                    case NonFatal(ee) ⇒ ee.printStackTrace()
                  }

                  e
                }

                def resolveVal(v: Any) = newUc match {
                  case Some(MappedCtxUpdate(_, mapFn, _)) ⇒ mapFn(v)
                  case None ⇒ v
                }

                val resolve =
                  try {
                    result match {
                      case Value(v) ⇒
                        Future.successful(resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) → resolveUc(v))
                      case TryValue(v) ⇒
                        Future.successful(v match {
                          case Success(success) ⇒
                            resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(success), uc) → resolveUc(v)
                          case Failure(e) ⇒ Result(ErrorRegistry(path :+ fields.head.outputName, resolveError(e), fields.head.position), None) → uc
                        })
                      case DeferredValue(d) ⇒
                        val p = Promise[Any]()

                        resolveDeferred(uc, DeferredResult(Vector(Future.successful(Vector(Defer(p, d)))), p.future.flatMap { v ⇒
                          resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) match {
                            case r: Result ⇒ Future.successful(r)
                            case er: DeferredResult ⇒ resolveDeferred(uc, er)
                          }
                        }.recover {
                          case e ⇒ Result(ErrorRegistry(path :+ fields.head.outputName, resolveError(e), fields.head.position), None)
                        })).flatMap(r ⇒ p.future map (r → resolveUc(_)) recover { case _ ⇒ r → uc})
                      case FutureValue(f) ⇒
                        f.map(v ⇒ resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) → resolveUc(v))
                            .recover { case e ⇒ Result(errors.add(path :+ name, resolveError(e), fields.head.position), None) → uc}
                      case DeferredFutureValue(df) ⇒
                        val p = Promise[Any]()

                        resolveDeferred(uc, DeferredResult(Vector(df.map(d ⇒ Vector(Defer(p, d)))), p.future.flatMap { v ⇒
                          resolveValue(path :+ fields.head.outputName, fields, sfield.fieldType, sfield, resolveVal(v), uc) match {
                            case r: Result ⇒ Future.successful(r)
                            case er: DeferredResult ⇒ resolveDeferred(uc, er)
                          }
                        }.recover {
                          case e ⇒ Result(ErrorRegistry(path :+ fields.head.outputName, resolveError(e), fields.head.position), None)
                        })).flatMap(r ⇒ p.future map (r → resolveUc(_)) recover { case _ ⇒ r → uc})
                    }
                  } catch {
                    case NonFatal(e) ⇒
                      Future.successful(Result(ErrorRegistry(path :+ fields.head.outputName, resolveError(e), fields.head.position), None) → uc)
                  }

                resolve.flatMap {
                  case (r : Result, newUc) ⇒
                    Future.successful(accRes.addToMap(r, fields.head.outputName, isOptional(tpe, fields.head.name), path :+ fields.head.outputName, fields.head.position) → newUc)
                  case (dr : DeferredResult, newUc) ⇒
                    resolveDeferred(newUc, dr) map (accRes.addToMap(_, fields.head.outputName, isOptional(tpe, fields.head.name), path :+ fields.head.outputName, fields.head.position) → newUc)
                }
            }
        }
      }
    }.map(_._1)
  }

  def collectActionsPar(
      path: Vector[String],
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: Map[String, (ast.Field, Try[Vector[ast.Field]])],
      errorReg: ErrorRegistry,
      userCtx: Ctx): Actions =
    fields.foldLeft((errorReg, Some(Vector.empty)): Actions) {
      case (acc @ (_, None), _) ⇒ acc
      case (acc, (name, (origField, _))) if tpe.getField(schema, origField.name).isEmpty ⇒ acc
      case ((errors, s @ Some(acc)), (name, (origField, Failure(error)))) ⇒
        errors.add(path :+ name, error) → (if (isOptional(tpe, origField.name)) Some(acc :+ (Vector(origField), None)) else None)
      case ((errors, s @ Some(acc)), (name, (origField, Success(fields)))) ⇒
        resolveField(userCtx, tpe, path :+ name, value, errors, name, fields) match {
          case (updatedErrors, Some(result), updateCtx) ⇒ updatedErrors → Some(acc :+ (fields, Some((tpe.getField(schema, origField.name).head, updateCtx, result))))
          case (updatedErrors, None, _) if isOptional(tpe, origField.name) ⇒ updatedErrors → Some(acc :+ (Vector(origField), None))
          case (updatedErrors, None, _) ⇒ updatedErrors → None
        }
    }

  def resolveActionsPar(path: Vector[String], tpe: ObjectType[Ctx, _], actions: Actions, userCtx: Ctx): Resolve = {
    val (errors, res) = actions

    def resolveUc(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any) = newUc map (_.ctxFn(v)) getOrElse userCtx

    def resolveError(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], e: Throwable) = {
      try {
        newUc map (_.onError(e))
      } catch {
        case NonFatal(ee) ⇒ ee.printStackTrace()
      }

      e
    }

    def resolveVal(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any) = newUc match {
      case Some(MappedCtxUpdate(_, mapFn, _)) ⇒ mapFn(v)
      case None ⇒ v
    }

    res match {
      case None ⇒ Result(errors, None)
      case Some(results) ⇒
        val resolvedValues = results.map {
          case (astFields, None) ⇒ astFields.head → Result(ErrorRegistry.empty, None)
          case (astFields, Some((field, updateCtx, Value(v)))) ⇒
            try {
              astFields.head → resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))
            } catch {
              case NonFatal(e) ⇒
                astFields.head → Result(ErrorRegistry(path :+ astFields.head.outputName, resolveError(updateCtx, e), astFields.head.position), None)
            }
          case (astFields, Some((field, updateCtx, TryValue(v)))) ⇒
            v match {
              case Success(success) ⇒
                try {
                  astFields.head → resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, success), resolveUc(updateCtx, success))
                } catch {
                  case NonFatal(e) ⇒
                    astFields.head → Result(ErrorRegistry(path :+ astFields.head.outputName, resolveError(updateCtx, e), astFields.head.position), None)
                }
              case Failure(e) ⇒
                astFields.head → Result(ErrorRegistry(path :+ astFields.head.outputName, resolveError(updateCtx, e), astFields.head.position), None)
            }
          case (astFields, Some((field, updateCtx, DeferredValue(deferred)))) ⇒
            val promise = Promise[Any]()

            astFields.head → DeferredResult(Vector(Future.successful(Vector(Defer(promise, deferred)))),
              promise.future
                .flatMap { v ⇒
                  val uc = resolveUc(updateCtx, v)

                  resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc) match {
                    case r: Result ⇒ Future.successful(r)
                    case er: DeferredResult ⇒ resolveDeferred(uc, er)
                  }
                }
                .recover {
                  case e ⇒ Result(ErrorRegistry(path :+ astFields.head.outputName, resolveError(updateCtx, e), astFields.head.position), None)
                })
          case (astFields, Some((field, updateCtx, FutureValue(future)))) ⇒
            val resolved = future.map(v ⇒ resolveValue(path :+ astFields.head.outputName, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))).recover {
              case e ⇒ Result(ErrorRegistry(path :+ astFields.head.outputName, resolveError(updateCtx, e), astFields.head.position), None)
            }

            val deferred = resolved flatMap {
              case r: Result ⇒ Future.successful(Vector.empty)
              case r: DeferredResult ⇒ Future.sequence(r.deferred) map (_.flatten)
            }
            val value = resolved flatMap {
              case r: Result ⇒ Future.successful(r)
              case dr: DeferredResult ⇒ dr.futureValue
            }

            astFields.head → DeferredResult(Vector(deferred), value)
          case (astFields, Some((field, updateCtx, DeferredFutureValue(deferredValue)))) ⇒
            val promise = Promise[Any]()

            astFields.head → DeferredResult(Vector(deferredValue.map(d ⇒ Vector(Defer(promise, d)))),
              promise.future.flatMap { v ⇒
                val uc = resolveUc(updateCtx, v)

                resolveValue(path :+ field.name, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc) match {
                  case r: Result ⇒ Future.successful(r)
                  case er: DeferredResult ⇒ resolveDeferred(uc, er)
                }
              }
              .recover{
                case e ⇒ Result(ErrorRegistry(path :+ field.name, resolveError(updateCtx, e), astFields.head.position), None)
              })
        }

        val simpleRes = resolvedValues.collect {case (af, r: Result) ⇒ af → r}

        val resSoFar = simpleRes.foldLeft(Result(errors, Some(marshaller.emptyMapNode))) {
          case (res, (astField, other)) ⇒ res addToMap (other, astField.outputName, isOptional(tpe, astField.name), path :+ astField.outputName, astField.position)
        }

        val complexRes = resolvedValues.collect{case (af, r: DeferredResult) ⇒ af → r}

        if (complexRes.isEmpty) resSoFar
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future.sequence(complexRes.map {case (astField, DeferredResult(_, future)) ⇒  future map (astField → _)}) map { results ⇒
            results.foldLeft(resSoFar) {
              case (res, (astField, other)) ⇒ res addToMap (other, astField.outputName, isOptional(tpe, astField.name), path :+ astField.outputName, astField.position)
            }
          }

          DeferredResult(allDeferred, finalValue)
        }
    }
  }

  def resolveDeferred(uc: Ctx, res: DeferredResult) =
    Future.sequence(res.deferred).flatMap { listOfDef ⇒
      val toResolve = listOfDef.flatten

      def findActualDeferred(deferred: Deferred[_]): Deferred[_] = deferred match {
        case MappingDeferred(d, _) ⇒ findActualDeferred(d)
        case d ⇒ d
      }

      def mapAllDeferred(deferred: Deferred[_], value: Future[Any]): Future[Any] = deferred match {
        case MappingDeferred(d, fn) ⇒ mapAllDeferred(d, value) map (fn)
        case d ⇒ value
      }

      try {
        val resolved = deferredResolver.resolve(toResolve map (d ⇒ findActualDeferred(d.deferred)), uc)

        toResolve zip resolved foreach {
          case (toRes, f) ⇒ toRes.promise tryCompleteWith (mapAllDeferred(toRes.deferred, f))
        }
      } catch {
        case NonFatal(error) ⇒ toResolve foreach (_.promise.failure(error))
      }

      res.futureValue
    }

  def resolveValue(
      path: Vector[String],
      astFields: Vector[ast.Field],
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any,
      userCtx: Ctx): Resolve  =
    tpe match {
      case OptionType(optTpe) ⇒
        val actualValue = value match {
          case v: Option[_] ⇒ v
          case v ⇒ Option(v)
        }

        actualValue match {
          case Some(someValue) ⇒ resolveValue(path, astFields, optTpe, field, someValue, userCtx)
          case None ⇒ Result(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) ⇒
        if (value == null)
          Result(ErrorRegistry.empty, None)
        else {
          val actualValue = value match {
            case seq: Seq[_] ⇒ seq
            case other ⇒ Seq(other)
          }

          val res = actualValue map (resolveValue(path, astFields, listTpe, field, _, userCtx))
          val simpleRes = res.collect { case r: Result ⇒ r}

          if (simpleRes.size == res.size)
            simpleRes.foldLeft(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))) {
              case (acc, res) ⇒ acc.addToList(res, isOptional(listTpe), path, astFields.head.position)
            }
          else
            res.foldLeft(DeferredResult(Vector.empty, Future.successful(Result(ErrorRegistry.empty, Some(marshaller.emptyArrayNode))))) {
              case (acc, res: Result) ⇒ acc.copy(futureValue = acc.futureValue map (r ⇒ r.addToList(res, isOptional(listTpe), path, astFields.head.position)))

              case (acc, DeferredResult(deferred, futureValue)) ⇒ acc.copy(deferred = acc.deferred ++ deferred, futureValue =
                  for {accRes ← acc.futureValue; res ← futureValue} yield accRes.addToList(res, isOptional(listTpe), path, astFields.head.position))
            }
        }
      case scalar: ScalarType[Any @unchecked] ⇒
        try {
          Result(ErrorRegistry.empty, if (value == null) None else Some(marshalValue(scalar.coerceOutput(value), marshaller)))
        } catch {
          case NonFatal(e) ⇒ Result(ErrorRegistry(path, e), None)
        }
      case enum: EnumType[Any @unchecked] ⇒
        try {
          Result(ErrorRegistry.empty, if (value == null) None else Some(marshalValue(enum.coerceOutput(value), marshaller)))
        } catch {
          case NonFatal(e) ⇒ Result(ErrorRegistry(path, e), None)
        }
      case obj: ObjectType[Ctx, _] ⇒
        fieldCollector.collectFields(path, obj, astFields) match {
          case Success(fields) ⇒
            val actions = collectActionsPar(path, obj, value, fields, ErrorRegistry.empty, userCtx)

            resolveActionsPar(path, obj, actions, userCtx)
          case Failure(error) ⇒ Result(ErrorRegistry(path, error), None)
        }
      case abst: AbstractType ⇒
        abst.typeOf(value, schema) match {
          case Some(obj) ⇒ resolveValue(path, astFields, obj, field, value, userCtx)
          case None ⇒ Result(ErrorRegistry(path,
            new ExecutionError(s"Can't find appropriate subtype for field at path ${path mkString ", "}", sourceMapper, astFields.head.position.toList)), None)
        }
    }

  def resolveField(userCtx: Ctx, tpe: ObjectType[Ctx, _], path: Vector[String], value: Any, errors: ErrorRegistry, name: String, astFields: Vector[ast.Field]): (ErrorRegistry, Option[LeafAction[Ctx, Any]], Option[MappedCtxUpdate[Ctx, Any, Any]]) = {
    val astField = astFields.head
    val allFields = tpe.getField(schema, astField.name).asInstanceOf[List[Field[Ctx, Any]]]
    val field = allFields.head

    maxQueryDepth match {
      case Some(max) if path.size > max ⇒ (errors.add(path, new ExecutionError(s"Max query depth $max is reached."), astField.position), None, None)
      case _ ⇒
        valueCollector.getFieldArgumentValues(path, field.arguments, astField.arguments, variables) match {
          case Success(args) ⇒
            val ctx = Context[Ctx, Any](
              value,
              userCtx,
              args,
              schema.asInstanceOf[Schema[Ctx, Any]],
              field,
              tpe.asInstanceOf[ObjectType[Ctx, Any]],
              marshaller,
              sourceMapper,
              deprecationTracker,
              astFields,
              path)

            if (allFields.exists(_.deprecationReason.isDefined))
              deprecationTracker.deprecatedFieldUsed(ctx)

            try {
              val mBefore = middleware flatMap {
                case (mv, m: MiddlewareBeforeField[Ctx]) ⇒
                  Some((m.beforeField(mv.asInstanceOf[m.QueryVal], middlewareCtx, ctx), mv, m))
                case _ ⇒
                  None
              }

              val beforeAction = mBefore.flatMap{case ((_, action), _, _) ⇒ action}.lastOption

              val mAfter = mBefore.filter(_._3.isInstanceOf[MiddlewareAfterField[Ctx]])
              val mError = mBefore.filter(_._3.isInstanceOf[MiddlewareErrorField[Ctx]])

              def doAfterMiddleware[Val](v: Val): Val = {
                val results = mAfter.flatMap {
                  case ((cv, _), mv, m: MiddlewareAfterField[Ctx]) ⇒
                    m.afterField(mv.asInstanceOf[m.QueryVal], cv.asInstanceOf[m.FieldVal], v, middlewareCtx, ctx).asInstanceOf[Option[Val]]
                  case _ ⇒ None
                }

                results.lastOption getOrElse v
              }

              def doErrorMiddleware(error: Throwable): Unit =
                mAfter.collect {
                  case ((cv, _), mv, m: MiddlewareErrorField[Ctx]) ⇒
                    m.fieldError(mv.asInstanceOf[m.QueryVal], cv.asInstanceOf[m.FieldVal], error, middlewareCtx, ctx)
                }

              def doAfterMiddlewareWithMap[Val, NewVal](fn: Val ⇒ NewVal)(v: Val): NewVal = {
                val mapped = fn(v)

                val results = mAfter.flatMap {
                  case ((cv, _), mv, m: MiddlewareAfterField[Ctx]) ⇒
                    m.afterField(mv.asInstanceOf[m.QueryVal], cv.asInstanceOf[m.FieldVal], mapped, middlewareCtx, ctx).asInstanceOf[Option[NewVal]]
                  case _ ⇒ None
                }

                results.lastOption getOrElse mapped
              }

              try {
                val res =
                  beforeAction match {
                    case Some(action) ⇒ action
                    case None ⇒
                      field.resolve match {
                        case pfn: Projector[Ctx, _, _] ⇒ pfn(ctx, collectProjections(path, field, astFields, pfn.maxLevel))
                        case fn ⇒ fn(ctx)
                      }
                  }

                res match {
                  case resolved: LeafAction[Ctx, Any @unchecked] ⇒
                    (errors,
                      Some(resolved),
                      if (mAfter.nonEmpty || mError.nonEmpty)
                        Some(MappedCtxUpdate(
                          _ ⇒ userCtx,
                          if (mAfter.nonEmpty) doAfterMiddleware else identity,
                          if (mError.nonEmpty) doErrorMiddleware else identity))
                      else None)

                  case res: UpdateCtx[Ctx, Any @unchecked] ⇒
                    (errors,
                      Some(res.action),
                      Some(MappedCtxUpdate(
                        res.nextCtx,
                        if (mAfter.nonEmpty) doAfterMiddleware else identity,
                        if (mError.nonEmpty) doErrorMiddleware else identity)))

                  case res: MappedUpdateCtx[Ctx, Any @unchecked, Any @unchecked] ⇒
                    (errors,
                      Some(res.action),
                      Some(MappedCtxUpdate(
                        res.nextCtx,
                        if (mAfter.nonEmpty) doAfterMiddlewareWithMap(res.mapFn) else res.mapFn,
                        if (mError.nonEmpty) doErrorMiddleware else identity)))
                }
              } catch {
                case NonFatal(e) ⇒
                  try {
                    if (mError.nonEmpty) doErrorMiddleware(e)

                    (errors.add(path, e, astField.position), None, None)
                  } catch {
                    case NonFatal(me) ⇒ (errors.add(path, e, astField.position).add(path, me, astField.position), None, None)
                  }
              }
            } catch {
              case NonFatal(e) ⇒ (errors.add(path, e, astField.position), None, None)
            }
          case Failure(error) ⇒ (errors.add(path, error), None, None)
        }
    }
  }

  def collectProjections(path: Vector[String], field: Field[Ctx, _], astFields: Vector[ast.Field], maxLevel: Int): Vector[ProjectedName] = {
    def loop(path: Vector[String], tpe: OutputType[_], astFields: Vector[ast.Field], currLevel: Int): Vector[ProjectedName] =
      if (currLevel > maxLevel) Vector.empty
      else tpe match {
        case OptionType(ofType) ⇒ loop(path, ofType, astFields, currLevel)
        case ListType(ofType) ⇒ loop(path, ofType, astFields, currLevel)
        case objTpe: ObjectType[Ctx, _] ⇒
          fieldCollector.collectFields(path, objTpe, astFields) match {
            case Success(ff) ⇒
              ff.values.toVector collect {
                case (_, Success(fields)) if objTpe.getField(schema, fields.head.name).nonEmpty && !objTpe.getField(schema, fields.head.name).head.tags.contains(ProjectionExclude) ⇒
                  val astField = fields.head
                  val field = objTpe.getField(schema, astField.name).head
                  val projectedName =
                    field.tags collect {case ProjectionName(name) ⇒ name} match {
                    case name :: _ ⇒ name
                    case _ ⇒ field.name
                  }

                  ProjectedName(projectedName, loop(path :+ projectedName, field.fieldType, fields, currLevel + 1))
              }
            case Failure(_) ⇒ Vector.empty
          }
        case abst: AbstractType ⇒
          schema.possibleTypes
            .get (abst.name)
            .map (_.flatMap(loop(path, _, astFields, currLevel + 1)).groupBy(_.name).map(_._2.head).toVector)
            .getOrElse (Vector.empty)
        case _ ⇒ Vector.empty
      }

    loop(path, field.fieldType, astFields, 1)
  }

  def isOptional(tpe: ObjectType[_, _], fieldName: String): Boolean =
    isOptional(tpe.getField(schema, fieldName).head.fieldType)

  def isOptional(tpe: OutputType[_]): Boolean =
    tpe.isInstanceOf[OptionType[_]]

  trait Resolve

  case class DeferredResult(deferred: Vector[Future[Vector[Defer]]], futureValue: Future[Result]) extends Resolve
  case class Defer(promise: Promise[Any], deferred: Deferred[Any])
  case class Result(errors: ErrorRegistry, value: Option[marshaller.Node]) extends Resolve {
    def addToMap(other: Result, key: String, optional: Boolean, path: Vector[String], position: Option[Position]) =
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
              for {myVal ← value; otherVal ← other.value} yield marshaller.addMapNodeElem(myVal, key, otherVal))

    def addToList(other: Result, optional: Boolean, path: Vector[String], position: Option[Position]) = copy(
      errors =
          if (!optional && other.value.isEmpty && other.errors.errorList.isEmpty)
            errors.add(other.errors).add(path, new ExecutionError("Cannot return null for non-nullable type", sourceMapper, position.toList))
          else
            errors.add(other.errors),
      value =
          if (optional && other.value.isEmpty)
            value map (marshaller.addArrayNodeElem(_, marshaller.nullNode))
          else
            for {myVal ← value; otherVal ← other.value} yield marshaller.addArrayNodeElem(myVal, otherVal))
  }
}

case class MappedCtxUpdate[Ctx, Val, NewVal](ctxFn: Val ⇒ Ctx, mapFn: Val ⇒ NewVal, onError: Throwable ⇒ Unit)

object Resolver {
  val DefaultComplexity = 1.0D

  def marshalValue(value: ast.Value, marshaller: ResultMarshaller): marshaller.Node = value match {
    case ast.StringValue(str, _) ⇒ marshaller.stringNode(str)
    case ast.IntValue(i, _) ⇒ marshaller.intNode(i)
    case ast.BigIntValue(i, _) ⇒ marshaller.bigIntNode(i)
    case ast.FloatValue(f, _) ⇒ marshaller.floatNode(f)
    case ast.BigDecimalValue(f, _) ⇒ marshaller.bigDecimalNode(f)
    case ast.BooleanValue(b, _) ⇒ marshaller.booleanNode(b)
    case ast.EnumValue(enum, _) ⇒ marshaller.stringNode(enum)
    case ast.ListValue(values, _) ⇒ marshaller.arrayNode(values map (marshalValue(_, marshaller)))
    case ast.ObjectValue(values, _) ⇒ marshaller.mapNode(values map (v ⇒ v.name → marshalValue(v.value, marshaller)))
    case ast.VariableValue(_, _) ⇒ throw new IllegalStateException("Can't marshall variable values!")
  }
}