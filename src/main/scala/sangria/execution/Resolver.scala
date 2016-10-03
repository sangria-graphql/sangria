package sangria.execution

import language.existentials
import org.parboiled2.Position
import sangria.ast
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.marshalling.{ResultMarshaller, ScalarValueInfo}
import sangria.parser.SourceMapper
import sangria.schema._

import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Resolver[Ctx](
    val marshaller: ResultMarshaller,
    middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
    schema: Schema[Ctx, _],
    valueCollector: ValueCollector[Ctx, _],
    variables: Map[String, VariableValue],
    fieldCollector: FieldCollector[Ctx, _],
    userContext: Ctx,
    exceptionHandler: Executor.ExceptionHandler,
    deferredResolver: DeferredResolver[Ctx],
    sourceMapper: Option[SourceMapper],
    deprecationTracker: DeprecationTracker,
    middleware: List[(Any, Middleware[_])],
    maxQueryDepth: Option[Int],
    deferredResolverState: Any
)(implicit executionContext: ExecutionContext) {
  val resultResolver = new ResultResolver(marshaller, exceptionHandler)

  import resultResolver._
  import Resolver._

  def resolveFieldsPar(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val actions = collectActionsPar(ExecutionPath.empty, tpe, value, fields, ErrorRegistry.empty, userContext)

    handleScheme(processFinalResolve(resolveActionsPar(ExecutionPath.empty, tpe, actions, userContext, fields.namesOrdered)), scheme)
  }

  def resolveFieldsSeq(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val actions = resolveSeq(ExecutionPath.empty, tpe, value, fields, ErrorRegistry.empty)

    handleScheme(actions flatMap processFinalResolve, scheme)
  }

  def handleScheme(result: Future[(Vector[RegisteredError], marshaller.Node)], scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = scheme match {
    case ExecutionScheme.Default ⇒
      result.map{case (_, res) ⇒ res}.asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case ExecutionScheme.FullResult ⇒
      result.map{case (errors, res) ⇒ ExecutionResult(userContext, res, errors, middleware)}.asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case s ⇒
      throw new IllegalStateException(s"Unsupported execution scheme: $s")
  }

  def processFinalResolve(resolve: Resolve) = resolve match {
    case Result(errors, data, _) ⇒
      Future.successful(
        errors.originalErrors →
          marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]],
            marshalErrors(errors)).asInstanceOf[marshaller.Node])

    case dr: DeferredResult ⇒
      immediatelyResolveDeferred(userContext, dr, _ map { case (Result(errors, data, _)) ⇒
        errors.originalErrors →
          marshalResult(data.asInstanceOf[Option[resultResolver.marshaller.Node]],
            marshalErrors(errors)).asInstanceOf[marshaller.Node]
      })
  }

  private def immediatelyResolveDeferred[T](uc: Ctx, dr: DeferredResult, fn: Future[Result] ⇒ Future[T]): Future[T] = {
    val res = fn(dr.futureValue)

    resolveDeferredWithGrouping(dr.deferred).foreach(groups ⇒
      groups.foreach(group ⇒ resolveDeferred(uc, group)))

    res
  }

  private def resolveDeferredWithGrouping(deferred: Vector[Future[Vector[Defer]]]) =
    Future.sequence(deferred).map(listOfDef ⇒ deferredResolver.groupDeferred(listOfDef.flatten))

  private type Actions = (ErrorRegistry, Option[Vector[(Vector[ast.Field], Option[(Field[Ctx, _], Option[MappedCtxUpdate[Ctx, Any, Any]], LeafAction[Ctx, _])])]])

  def resolveSeq(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields,
      errorReg: ErrorRegistry): Future[Result] = {
    fields.fields.foldLeft(Future.successful((Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode(fields.namesOrdered))), userContext))) {
      case (future, elem) ⇒ future.flatMap { resAndCtx ⇒
        (resAndCtx, elem) match {
          case (acc @ (Result(_, None, _), _), _) ⇒ Future.successful(acc)
          case (acc, CollectedField(name, origField, _)) if tpe.getField(schema, origField.name).isEmpty ⇒ Future.successful(acc)
          case ((Result(errors, s @ Some(acc), _), uc), CollectedField(name, origField, Failure(error))) ⇒
            Future.successful(Result(errors.add(path + name, error),
              if (isOptional(tpe, origField.name)) Some(marshaller.addMapNodeElem(acc.asInstanceOf[marshaller.MapBuilder], origField.outputName, marshaller.nullNode, optional = true))
              else None) → uc)
          case ((accRes @ Result(errors, s @ Some(acc), _), uc), CollectedField(name, origField, Success(fields))) ⇒
            resolveField(uc, tpe, path + name, value, errors, name, fields) match {
              case (updatedErrors, None, _) if isOptional(tpe, origField.name) ⇒
                Future.successful(Result(updatedErrors, Some(marshaller.addMapNodeElem(acc.asInstanceOf[marshaller.MapBuilder], fields.head.outputName, marshaller.nullNode, optional = isOptional(tpe, origField.name)))) → uc)
              case (updatedErrors, None, _) ⇒ Future.successful(Result(updatedErrors, None) → uc)
              case (updatedErrors, Some(result), newUc) ⇒
                val sfield = tpe.getField(schema, origField.name).head
                val fieldPath = path + fields.head

                def resolveUc(v: Any) = newUc map (_.ctxFn(v)) getOrElse uc

                def resolveError(e: Throwable) = {
                  try {
                    newUc foreach (_.onError(e))
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
                        val updatedUc = resolveUc(v)

                        Future.successful(resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc) → updatedUc)
                      case PartialValue(v, es) ⇒
                        val updatedUc = resolveUc(v)

                        Future.successful(
                          resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
                            .appendErrors(fieldPath, es, fields.head.position) → updatedUc)
                      case TryValue(v) ⇒
                        Future.successful(v match {
                          case Success(success) ⇒
                            val updatedUc = resolveUc(success)

                            resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(success), updatedUc) → updatedUc
                          case Failure(e) ⇒ Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.position), None) → uc
                        })
                      case DeferredValue(d) ⇒
                        val p = Promise[(ChildDeferredContext, Any)]()
                        val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
                        val defer = Defer(p, d, complexity, sfield, fields, args)

                        immediatelyResolveDeferred(uc, DeferredResult(Vector(Future.successful(Vector(defer))), p.future.flatMap { case (dctx, v) ⇒
                          val updatedUc = resolveUc(v)

                          resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc) match {
                            case r: Result ⇒ dctx.resolveResult(r.copy(userContext = Some(updatedUc)))
                            case er: DeferredResult ⇒
                              dctx.resolveDeferredResult(updatedUc, er).map(_.copy(userContext = Some(updatedUc)))
                          }
                        }.recover {
                          case e ⇒ Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.position), None)
                        }), _.map(r ⇒ r → r.userContext.getOrElse(uc)))
                      case FutureValue(f) ⇒
                        f.map { v ⇒
                          val updatedUc = resolveUc(v)

                          resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc) → updatedUc
                        }.recover { case e ⇒ Result(errors.add(path + name, resolveError(e), fields.head.position), None) → uc}
                      case PartialFutureValue(f) ⇒
                        f.map{
                          case PartialValue(v, es) ⇒
                            val updatedUc = resolveUc(v)

                            resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
                              .appendErrors(fieldPath, es, fields.head.position) → updatedUc
                        }.recover { case e ⇒ Result(errors.add(path + name, resolveError(e), fields.head.position), None) → uc}
                      case DeferredFutureValue(df) ⇒
                        val p = Promise[(ChildDeferredContext, Any)]()
                        def defer(d: Deferred[Any]) = {
                          val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
                          Defer(p, d, complexity, sfield, fields, args)
                        }

                        immediatelyResolveDeferred(uc, DeferredResult(Vector(df.map(d ⇒ Vector(defer(d)))), p.future.flatMap { case (dctx, v) ⇒
                          val updatedUc = resolveUc(v)

                          resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc) match {
                            case r: Result ⇒ dctx.resolveResult(r.copy(userContext = Some(updatedUc)))
                            case er: DeferredResult ⇒ dctx.resolveDeferredResult(updatedUc, er).map(_.copy(userContext = Some(updatedUc)))
                          }
                        }.recover {
                          case e ⇒ Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.position), None)
                        }), _.map(r ⇒ r → r.userContext.getOrElse(uc)))
                    }
                  } catch {
                    case NonFatal(e) ⇒
                      Future.successful(Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.position), None) → uc)
                  }

                resolve.flatMap {
                  case (r : Result, newUc) ⇒
                    Future.successful(accRes.addToMap(r, fields.head.outputName, isOptional(tpe, fields.head.name), fieldPath, fields.head.position) → newUc)
                  case (dr : DeferredResult, newUc) ⇒
                    immediatelyResolveDeferred(newUc, dr,
                      _.map(accRes.addToMap(_, fields.head.outputName, isOptional(tpe, fields.head.name), fieldPath, fields.head.position) → newUc))
                }
            }
        }
      }
    } map {
      case (res, ctx) ⇒ res.buildValue
    }
  }

  private def calcComplexity(path: ExecutionPath, astField: ast.Field, field: Field[Ctx, _], uc: Ctx) = {
    val args = valueCollector.getFieldArgumentValues(path, field.arguments, astField.arguments, variables)

    args match {
      case Success(a) ⇒ a → field.complexity.fold(DefaultComplexity)(_(uc, a, DefaultComplexity))
      case _ ⇒ Args.empty → DefaultComplexity
    }
  }

  def collectActionsPar(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields,
      errorReg: ErrorRegistry,
      userCtx: Ctx): Actions =
    fields.fields.foldLeft((errorReg, Some(Vector.empty)): Actions) {
      case (acc @ (_, None), _) ⇒ acc
      case (acc, CollectedField(name, origField, _)) if tpe.getField(schema, origField.name).isEmpty ⇒ acc
      case ((errors, s @ Some(acc)), CollectedField(name, origField, Failure(error))) ⇒
        errors.add(path + name, error) → (if (isOptional(tpe, origField.name)) Some(acc :+ (Vector(origField) → None)) else None)
      case ((errors, s @ Some(acc)), CollectedField(name, origField, Success(fields))) ⇒
        resolveField(userCtx, tpe, path + name, value, errors, name, fields) match {
          case (updatedErrors, Some(result), updateCtx) ⇒ updatedErrors → Some(acc :+ (fields → Some((tpe.getField(schema, origField.name).head, updateCtx, result))))
          case (updatedErrors, None, _) if isOptional(tpe, origField.name) ⇒ updatedErrors → Some(acc :+ (Vector(origField) → None))
          case (updatedErrors, None, _) ⇒ updatedErrors → None
        }
    }

  def resolveActionsPar(path: ExecutionPath, tpe: ObjectType[Ctx, _], actions: Actions, userCtx: Ctx, fieldsNamesOrdered: Vector[String]): Resolve = {
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
            val fieldsPath = path + astFields.head

            try {
              astFields.head → resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))
            } catch {
              case NonFatal(e) ⇒
                astFields.head → Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
            }
          case (astFields, Some((field, updateCtx, PartialValue(v, es)))) ⇒
            val fieldsPath = path + astFields.head

            try {
              astFields.head →
                resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))
                  .appendErrors(fieldsPath, es, astFields.head.position)
            } catch {
              case NonFatal(e) ⇒
                astFields.head → Result(
                  ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position)
                    .append(fieldsPath, es, astFields.head.position),
                  None)
            }
          case (astFields, Some((field, updateCtx, TryValue(v)))) ⇒
            val fieldsPath = path + astFields.head

            v match {
              case Success(success) ⇒
                try {
                  astFields.head → resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, success), resolveUc(updateCtx, success))
                } catch {
                  case NonFatal(e) ⇒
                    astFields.head → Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
                }
              case Failure(e) ⇒
                astFields.head → Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
            }
          case (astFields, Some((field, updateCtx, DeferredValue(deferred)))) ⇒
            val fieldsPath = path + astFields.head
            val promise = Promise[(ChildDeferredContext, Any)]()
            val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
            val defer = Defer(promise, deferred, complexity, field, astFields, args)

            astFields.head → DeferredResult(Vector(Future.successful(Vector(defer))),
              promise.future
                .flatMap { case (dctx, v) ⇒
                  val uc = resolveUc(updateCtx, v)

                  resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc) match {
                    case r: Result ⇒ dctx.resolveResult(r)
                    case er: DeferredResult ⇒ dctx.resolveDeferredResult(uc, er)
                  }
                }
                .recover {
                  case e ⇒ Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
                })
          case (astFields, Some((field, updateCtx, FutureValue(future)))) ⇒
            val fieldsPath = path + astFields.head

            val resolved = future.map(v ⇒ resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))).recover {
              case e ⇒ Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
            }

            def process() = {
              val deferred = resolved flatMap {
                case r: Result ⇒ Future.successful(Vector.empty)
                case r: DeferredResult ⇒ Future.sequence(r.deferred) map (_.flatten)
              }

              val value = resolved flatMap {
                case r: Result ⇒ Future.successful(r)
                case dr: DeferredResult ⇒ dr.futureValue
              }

              astFields.head → DeferredResult(Vector(deferred), value)
            }

            def processAndResolveDeferred() = {
              val value = resolved flatMap {
                case r: Result ⇒ Future.successful(r)
                case dr: DeferredResult ⇒ immediatelyResolveDeferred(userContext, dr, identity)
              }

              astFields.head → DeferredResult(Vector.empty, value)
            }

            deferredResolver.includeDeferredFromField match {
              case Some(fn) ⇒
                val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)

                if (fn(field, astFields, args, complexity))
                  process()
                else
                  processAndResolveDeferred()
              case None ⇒
                process()
            }

          case (astFields, Some((field, updateCtx, PartialFutureValue(future)))) ⇒
            val fieldsPath = path + astFields.head

            val resolved = future.map {case PartialValue(v, es) ⇒
              resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), resolveUc(updateCtx, v))
                .appendErrors(fieldsPath, es, astFields.head.position)}
              .recover {
                case e ⇒ Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
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
            val fieldsPath = path + astFields.head
            val promise = Promise[(ChildDeferredContext, Any)]()

            def defer(d: Deferred[Any]) = {
              val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
              Defer(promise, d, complexity, field, astFields, args)
            }

            astFields.head → DeferredResult(Vector(deferredValue.map(d ⇒ Vector(defer(d)))),
              promise.future.flatMap { case (dctx, v) ⇒
                val uc = resolveUc(updateCtx, v)

                resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc) match {
                  case r: Result ⇒ dctx.resolveResult(r)
                  case er: DeferredResult ⇒ dctx.resolveDeferredResult(uc, er)
                }
              }
              .recover{
                case e ⇒ Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.position), None)
              })
        }

        val simpleRes = resolvedValues.collect {case (af, r: Result) ⇒ af → r}

        val resSoFar = simpleRes.foldLeft(Result(errors, Some(marshaller.emptyMapNode(fieldsNamesOrdered)))) {
          case (res, (astField, other)) ⇒ res addToMap (other, astField.outputName, isOptional(tpe, astField.name), path + astField, astField.position)
        }

        val complexRes = resolvedValues.collect{case (af, r: DeferredResult) ⇒ af → r}

        if (complexRes.isEmpty) resSoFar.buildValue
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future.sequence(complexRes.map {case (astField, DeferredResult(_, future)) ⇒  future map (astField → _)}) map { results ⇒
            results.foldLeft(resSoFar) {
              case (res, (astField, other)) ⇒ res addToMap (other, astField.outputName, isOptional(tpe, astField.name), path + astField, astField.position)
            }.buildValue
          }

          DeferredResult(allDeferred, finalValue)
        }
    }
  }

  private def resolveDeferred(uc: Ctx, toResolve: Vector[Defer]) =
    if (toResolve.nonEmpty) {
      def findActualDeferred(deferred: Deferred[_]): Deferred[_] = deferred match {
        case MappingDeferred(d, _) ⇒ findActualDeferred(d)
        case d ⇒ d
      }

      def mapAllDeferred(deferred: Deferred[_], value: Future[Any]): Future[Any] = deferred match {
        case MappingDeferred(d, fn) ⇒ mapAllDeferred(d, value) map fn
        case d ⇒ value
      }

      try {
        val resolved = deferredResolver.resolve(toResolve map (d ⇒ findActualDeferred(d.deferred)), uc, deferredResolverState)

        if (toResolve.size == resolved.size) {
          val dctx = ParentDeferredContext(uc, toResolve.size)

          for (i ← toResolve.indices) {
            val toRes = toResolve(i)

            toRes.promise tryCompleteWith mapAllDeferred(toRes.deferred, resolved(i)).map(dctx.children(i) → _).recover {
              case NonFatal(e) ⇒
                dctx.children(i).resolveError(e)
                throw e
            }
          }

          dctx.init()
        } else {
          toResolve foreach (_.promise.failure(
            new IllegalStateException(s"Deferred resolver returned ${resolved.size} elements, but it got ${toResolve.size} deferred values. This violates the contract. You can find more information in the documentation: http://sangria-graphql.org/learn/#deferred-values-and-resolver")))
        }
      } catch {
        case NonFatal(error) ⇒ toResolve foreach (_.promise.failure(error))
      }
    }

  def resolveValue(
      path: ExecutionPath,
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
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else {
          val actualValue = value match {
            case seq: Seq[_] ⇒ seq
            case other ⇒ Seq(other)
          }

          val res = actualValue.zipWithIndex map {
            case (v, idx) ⇒ resolveValue(path withIndex idx, astFields, listTpe, field, v, userCtx)
          }

          val simpleRes = res.collect { case r: Result ⇒ r}
          val optional = isOptional(listTpe)

          if (simpleRes.size == res.size)
            resolveSimpleListValue(simpleRes, path, optional, astFields.head.position)
          else {
            // this is very hot place, so resorting to mutability to minimize the footprint
            val deferredBuilder = new VectorBuilder[Future[Vector[Defer]]]
            val resultFutures = new VectorBuilder[Future[Result]]

            val resIt = res.iterator

            while(resIt.hasNext) {
              resIt.next() match {
                case r: Result ⇒
                  resultFutures += Future.successful(r)
                case dr: DeferredResult ⇒
                  resultFutures += dr.futureValue
                  deferredBuilder ++= dr.deferred
              }
            }

            DeferredResult(
              deferred = deferredBuilder.result(),
              futureValue = Future.sequence(resultFutures.result()) map (
                resolveSimpleListValue(_, path, optional, astFields.head.position))
            )
          }
        }
      case scalar: ScalarType[Any @unchecked] ⇒
        try {
          Result(ErrorRegistry.empty,
            if (isUndefinedValue(value))
              None
            else
              Some(marshalScalarValue(scalar.coerceOutput(value, marshaller.capabilities), marshaller, scalar.name, scalar.scalarInfo)))
        } catch {
          case NonFatal(e) ⇒ Result(ErrorRegistry(path, e), None)
        }
      case enum: EnumType[Any @unchecked] ⇒
        try {
          Result(ErrorRegistry.empty, if (isUndefinedValue(value)) None else Some(marshalEnumValue(enum.coerceOutput(value), marshaller, enum.name)))
        } catch {
          case NonFatal(e) ⇒ Result(ErrorRegistry(path, e), None)
        }
      case obj: ObjectType[Ctx, _] ⇒
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else
          fieldCollector.collectFields(path, obj, astFields) match {
            case Success(fields) ⇒
              val actions = collectActionsPar(path, obj, value, fields, ErrorRegistry.empty, userCtx)

              resolveActionsPar(path, obj, actions, userCtx, fields.namesOrdered)
            case Failure(error) ⇒ Result(ErrorRegistry(path, error), None)
          }
      case abst: AbstractType ⇒
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else
          abst.typeOf(value, schema) match {
            case Some(obj) ⇒ resolveValue(path, astFields, obj, field, value, userCtx)
            case None ⇒ Result(ErrorRegistry(path,
              new ExecutionError(s"Can't find appropriate subtype for field at path $path", exceptionHandler, sourceMapper, astFields.head.position.toList)), None)
          }
    }

  def isUndefinedValue(value: Any) =
    value == null || value == None

  def resolveSimpleListValue(simpleRes: Seq[Result], path: ExecutionPath, optional: Boolean, astPosition: Option[Position]): Result = {
    // this is very hot place, so resorting to mutability to minimize the footprint

    var errorReg = ErrorRegistry.empty
    var listBuilder = new VectorBuilder[marshaller.Node]
    var canceled = false
    val resIt = simpleRes.iterator

    while (resIt.hasNext && !canceled) {
      val res = resIt.next()

      if (!optional && res.value.isEmpty && res.errors.isEmpty)
        errorReg = errorReg.add(path, nullForNotNullTypeError(astPosition))
      else if (res.errors.nonEmpty)
        errorReg = errorReg.add(res.errors)

      res.nodeValue match {
        case node if optional ⇒
          listBuilder += marshaller.optionalArrayNodeValue(node)
        case Some(other) ⇒
          listBuilder += other
        case None ⇒
          canceled = true
      }
    }

    Result(errorReg, if (canceled) None else Some(marshaller.arrayNode(listBuilder.result())))
  }

  def resolveField(
      userCtx: Ctx,
      tpe: ObjectType[Ctx, _],
      path: ExecutionPath,
      value: Any,
      errors: ErrorRegistry,
      name: String,
      astFields: Vector[ast.Field]): (ErrorRegistry, Option[LeafAction[Ctx, Any]], Option[MappedCtxUpdate[Ctx, Any, Any]]) = {
    val astField = astFields.head
    val allFields = tpe.getField(schema, astField.name).asInstanceOf[Vector[Field[Ctx, Any]]]
    val field = allFields.head

    maxQueryDepth match {
      case Some(max) if path.size > max ⇒ (errors.add(path, new ExecutionError(s"Max query depth $max is reached.", exceptionHandler), astField.position), None, None)
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
              path,
              deferredResolverState)

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
                  // these specific cases are important for time measuring middleware and eager values
                  case resolved: Value[Ctx, Any @unchecked] ⇒
                    (errors,
                      if (mAfter.nonEmpty)
                        Some(Value(doAfterMiddleware(resolved.value)))
                      else
                        Some(resolved),
                      None)

                  case resolved: PartialValue[Ctx, Any @unchecked] ⇒
                    (errors,
                      if (mAfter.nonEmpty)
                        Some(Value(doAfterMiddleware(resolved.value)))
                      else
                        Some(resolved),
                      if (mError.nonEmpty)
                        Some(MappedCtxUpdate(
                          _ ⇒ userCtx,
                          identity,
                          doErrorMiddleware))
                      else None)

                  case resolved: TryValue[Ctx, Any @unchecked] ⇒
                    (errors,
                      if (mAfter.nonEmpty && resolved.value.isSuccess)
                        Some(Value(doAfterMiddleware(resolved.value.get)))
                      else
                        Some(resolved),
                      if (mError.nonEmpty)
                        Some(MappedCtxUpdate(
                          _ ⇒ userCtx,
                          identity,
                          doErrorMiddleware))
                      else None)

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

  def collectProjections(path: ExecutionPath, field: Field[Ctx, _], astFields: Vector[ast.Field], maxLevel: Int): Vector[ProjectedName] = {
    def loop(path: ExecutionPath, tpe: OutputType[_], astFields: Vector[ast.Field], currLevel: Int): Vector[ProjectedName] =
      if (currLevel > maxLevel) Vector.empty
      else tpe match {
        case OptionType(ofType) ⇒ loop(path, ofType, astFields, currLevel)
        case ListType(ofType) ⇒ loop(path, ofType, astFields, currLevel)
        case objTpe: ObjectType[Ctx, _] ⇒
          fieldCollector.collectFields(path, objTpe, astFields) match {
            case Success(ff) ⇒
              ff.fields
                .collect {
                  case CollectedField(_, _, Success(fields)) if objTpe.getField(schema, fields.head.name).nonEmpty && !objTpe.getField(schema, fields.head.name).head.tags.contains(ProjectionExclude) ⇒
                    val astField = fields.head
                    val field = objTpe.getField(schema, astField.name).head
                    val projectionNames = field.tags collect {case ProjectionName(name) ⇒ name}

                    val projectedName =
                      if (projectionNames.nonEmpty) projectionNames.toVector
                      else Vector(field.name)

                    projectedName.map (name ⇒
                      ProjectedName(name, loop(path + name, field.fieldType, fields, currLevel + 1)))
                }
                .flatten
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

  def nullForNotNullTypeError(position: Option[Position]) =
    new ExecutionError("Cannot return null for non-nullable type", exceptionHandler, sourceMapper, position.toList)

  trait Resolve {
    def appendErrors(path: ExecutionPath, errors: Vector[Throwable], position: Option[Position]): Resolve
  }

  case class DeferredResult(deferred: Vector[Future[Vector[Defer]]], futureValue: Future[Result]) extends Resolve {
    def appendErrors(path: ExecutionPath, errors: Vector[Throwable], position: Option[Position]) = copy(futureValue = futureValue map (_.appendErrors(path, errors, position)))
  }

  case class Defer(promise: Promise[(ChildDeferredContext, Any)], deferred: Deferred[Any], complexity: Double, field: Field[_, _], astFields: Vector[ast.Field], args: Args) extends DeferredWithInfo
  case class Result(errors: ErrorRegistry, value: Option[Any /* Either marshaller.Node or marshaller.MapBuilder */], userContext: Option[Ctx] = None) extends Resolve {
    def addToMap(other: Result, key: String, optional: Boolean, path: ExecutionPath, position: Option[Position]) =
      copy(
        errors =
            if (!optional && other.value.isEmpty && other.errors.isEmpty)
              errors.add(other.errors).add(path, nullForNotNullTypeError(position))
            else
              errors.add(other.errors),
        value =
            if (optional && other.value.isEmpty)
              value map (v ⇒ marshaller.addMapNodeElem(v.asInstanceOf[marshaller.MapBuilder], key, marshaller.nullNode, optional = false))
            else
              for {myVal ← value; otherVal ← other.value} yield marshaller.addMapNodeElem(myVal.asInstanceOf[marshaller.MapBuilder], key, otherVal.asInstanceOf[marshaller.Node], optional = false))

    def nodeValue = value.asInstanceOf[Option[marshaller.Node]]
    def builderValue = value.asInstanceOf[Option[marshaller.MapBuilder]]
    def buildValue = copy(value = builderValue map marshaller.mapNode)

    def appendErrors(path: ExecutionPath, e: Vector[Throwable], position: Option[Position]) = copy(errors = errors.append(path, e, position))
  }

  case class ParentDeferredContext(uc: Ctx, expectedBranches: Int) {
    val children = Vector.fill(expectedBranches)(
      ChildDeferredContext(Promise[Vector[Future[Vector[Defer]]]]()))

    def init(): Unit =
      Future.sequence(children.map(_.promise.future)).onComplete { res ⇒
        val allDeferred = res.get.flatten

        if (allDeferred.nonEmpty)
          resolveDeferredWithGrouping(allDeferred).foreach(groups ⇒
            groups.foreach(group ⇒ resolveDeferred(uc, group)))
      }
  }

  case class ChildDeferredContext(promise: Promise[Vector[Future[Vector[Defer]]]]) {
    def resolveDeferredResult(uc: Ctx, res: DeferredResult): Future[Result] = {
      promise.success(res.deferred)
      res.futureValue
    }

    def resolveResult(res: Result): Future[Result] = {
      promise.success(Vector.empty)
      Future.successful(res)
    }

    def resolveError(e: Throwable): Unit = {
      promise.success(Vector.empty)
    }
  }
}

case class MappedCtxUpdate[Ctx, Val, NewVal](ctxFn: Val ⇒ Ctx, mapFn: Val ⇒ NewVal, onError: Throwable ⇒ Unit)

object Resolver {
  val DefaultComplexity = 1.0D

  def marshalEnumValue(value: String, marshaller: ResultMarshaller, typeName: String): marshaller.Node =
    marshaller.enumNode(value, typeName)

  def marshalScalarValue(value: Any, marshaller: ResultMarshaller, typeName: String, scalarInfo: Set[ScalarValueInfo]): marshaller.Node =
    value match {
      case astValue: ast.Value ⇒ marshalAstValue(astValue, marshaller, typeName, scalarInfo)
      case v ⇒ marshaller.scalarNode(value, typeName, scalarInfo)
    }

  def marshalAstValue(value: ast.Value, marshaller: ResultMarshaller, typeName: String, scalarInfo: Set[ScalarValueInfo]): marshaller.Node = value match {
    case ast.StringValue(str, _, _) ⇒ marshaller.scalarNode(str, typeName, scalarInfo)
    case ast.IntValue(i, _, _) ⇒ marshaller.scalarNode(i, typeName, scalarInfo)
    case ast.BigIntValue(i, _, _) ⇒ marshaller.scalarNode(i, typeName, scalarInfo)
    case ast.FloatValue(f, _, _) ⇒ marshaller.scalarNode(f, typeName, scalarInfo)
    case ast.BigDecimalValue(f, _, _) ⇒ marshaller.scalarNode(f, typeName, scalarInfo)
    case ast.BooleanValue(b, _, _) ⇒ marshaller.scalarNode(b, typeName, scalarInfo)
    case ast.NullValue(_, _) ⇒ marshaller.nullNode
    case ast.EnumValue(enum, _, _) ⇒ marshaller.enumNode(enum, typeName)
    case ast.ListValue(values, _, _) ⇒ marshaller.arrayNode(values.toVector map (marshalAstValue(_, marshaller, typeName, scalarInfo)))
    case ast.ObjectValue(values, _, _) ⇒ marshaller.mapNode(values map (v ⇒ v.name → marshalAstValue(v.value, marshaller, typeName, scalarInfo)))
    case ast.VariableValue(name, _, _) ⇒ marshaller.enumNode(name, typeName)
  }
}

trait DeferredWithInfo {
  def deferred: Deferred[Any]
  def complexity: Double
  def field: Field[_, _]
  def astFields: Vector[ast.Field]
  def args: Args
}