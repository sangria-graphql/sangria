package sangria.execution

import sangria.ast
import sangria.ast.{AstLocation, Document, SourceMapper}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.marshalling.ResultMarshaller
import sangria.schema._
import sangria.streaming.SubscriptionStream

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

private[execution] object FutureResolverBuilder extends ResolverBuilder {
  override def build[Ctx](
      marshaller: ResultMarshaller,
      middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
      schema: Schema[Ctx, _],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      fieldCollector: FieldCollector[Ctx, _],
      userContext: Ctx,
      exceptionHandler: ExceptionHandler,
      deferredResolver: DeferredResolver[Ctx],
      sourceMapper: Option[SourceMapper],
      deprecationTracker: Option[DeprecationTracker],
      middleware: List[(Any, Middleware[Ctx])],
      beforeFieldMiddlewares: List[(Any, MiddlewareBeforeField[Ctx])],
      maxQueryDepth: Option[Int],
      deferredResolverState: Any,
      preserveOriginalErrors: Boolean,
      validationTiming: TimeMeasurement,
      queryReducerTiming: TimeMeasurement,
      queryAst: Document)(implicit executionContext: ExecutionContext): Resolver[Ctx] =
    new FutureResolver[Ctx](
      marshaller,
      middlewareCtx,
      schema,
      valueCollector,
      variables,
      fieldCollector,
      userContext,
      exceptionHandler,
      deferredResolver,
      sourceMapper,
      deprecationTracker,
      middleware,
      beforeFieldMiddlewares,
      maxQueryDepth,
      deferredResolverState,
      preserveOriginalErrors,
      validationTiming,
      queryReducerTiming,
      queryAst
    )
}

/** [[Resolver]] using [[scala.concurrent.Future]] and [[scala.concurrent.Promise]] as base
  * asynchronous primitives
  */
private[execution] class FutureResolver[Ctx](
    val marshaller: ResultMarshaller,
    val middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
    val schema: Schema[Ctx, _],
    val valueCollector: ValueCollector[Ctx, _],
    val variables: Map[String, VariableValue],
    val fieldCollector: FieldCollector[Ctx, _],
    userContext: Ctx,
    val exceptionHandler: ExceptionHandler,
    deferredResolver: DeferredResolver[Ctx],
    val sourceMapper: Option[SourceMapper],
    val deprecationTracker: Option[DeprecationTracker],
    middlewares: List[(Any, Middleware[Ctx])],
    val beforeFieldMiddlewares: List[(Any, MiddlewareBeforeField[Ctx])],
    val maxQueryDepth: Option[Int],
    val deferredResolverState: Any,
    preserveOriginalErrors: Boolean,
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement,
    val queryAst: ast.Document
)(implicit executionContext: ExecutionContext)
    extends Resolver[Ctx]
    with FieldResolutionSupport[Ctx] {
  protected val resultResolver =
    new ResultResolver(marshaller, exceptionHandler, preserveOriginalErrors)
  private val toScalarMiddleware =
    Middleware.composeToScalarMiddleware(middlewares.map(_._2), userContext)

  import Resolver._
  import resultResolver._

  override def resolveFieldsPar(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val actions =
      collectActionsPar(ExecutionPath.empty, tpe, value, fields, ErrorRegistry.empty, userContext)

    handleScheme(
      processFinalResolve(
        resolveActionsPar(ExecutionPath.empty, tpe, actions, userContext, fields.namesOrdered))
        .map(_ -> userContext),
      scheme)
  }

  override def resolveFieldsSeq(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val result = resolveSeq(ExecutionPath.empty, tpe, value, fields)

    handleScheme(result.flatMap(res => processFinalResolve(res._1).map(_ -> res._2)), scheme)
  }

  override def resolveFieldsSubs(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    scheme match {
      case ExecutionScheme.Default =>
        val (s, res) = resolveSubs[({ type X[_] })#X](
          ExecutionPath.empty,
          tpe,
          value,
          fields,
          ErrorRegistry.empty,
          None)

        s.first(res).map(_._2).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

      case ExecutionScheme.Extended =>
        val (s, res) = resolveSubs[({ type X[_] })#X](
          ExecutionPath.empty,
          tpe,
          value,
          fields,
          ErrorRegistry.empty,
          None)

        s.first(res)
          .map { case (errors, res) =>
            ExecutionResult(
              userContext,
              res,
              errors,
              middlewares,
              validationTiming,
              queryReducerTiming)
          }
          .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

      case es: ExecutionScheme.StreamBasedExecutionScheme[
            ({ type X[_] })#X @unchecked] @unchecked =>
        val (_, res) = resolveSubs(
          ExecutionPath.empty,
          tpe,
          value,
          fields,
          ErrorRegistry.empty,
          Some(es.subscriptionStream))

        es.subscriptionStream
          .map(res) {
            case (errors, r) if es.extended =>
              ExecutionResult(
                userContext,
                r,
                errors,
                middlewares,
                validationTiming,
                queryReducerTiming)
            case (_, r) => r
          }
          .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

      case s =>
        throw new IllegalStateException(s"Unsupported execution scheme: $s")
    }

  protected def handleScheme(
      result: Future[((Vector[RegisteredError], marshaller.Node), Ctx)],
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = scheme match {
    case ExecutionScheme.Default =>
      result.map { case ((_, res), _) => res }.asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case ExecutionScheme.Extended =>
      result
        .map { case ((errors, res), uc) =>
          ExecutionResult(uc, res, errors, middlewares, validationTiming, queryReducerTiming)
        }
        .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case s: ExecutionScheme.StreamBasedExecutionScheme[_] @unchecked =>
      s.subscriptionStream
        .singleFuture(result.map {
          case ((errors, res), uc) if s.extended =>
            ExecutionResult(uc, res, errors, middlewares, validationTiming, queryReducerTiming)
          case ((_, res), _) => res
        })
        .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

    case s =>
      throw new IllegalStateException(s"Unsupported execution scheme: $s")
  }

  private def processFinalResolve(resolve: Resolve) = resolve match {
    case Result(errors, data, _) =>
      Future.successful(
        errors.originalErrors ->
          marshalResult(
            data.asInstanceOf[Option[resultResolver.marshaller.Node]],
            marshalErrors(errors),
            marshallExtensions.asInstanceOf[Option[resultResolver.marshaller.Node]],
            beforeExecution = false
          ).asInstanceOf[marshaller.Node])

    case dr: DeferredResult =>
      immediatelyResolveDeferred(
        userContext,
        dr,
        _.map { case Result(errors, data, _) =>
          errors.originalErrors ->
            marshalResult(
              data.asInstanceOf[Option[resultResolver.marshaller.Node]],
              marshalErrors(errors),
              marshallExtensions.asInstanceOf[Option[resultResolver.marshaller.Node]],
              beforeExecution = false
            ).asInstanceOf[marshaller.Node]
        }
      )
  }

  private def marshallExtensions: Option[marshaller.Node] = {
    val extensions =
      middlewares.flatMap {
        case (v, m: MiddlewareExtension[Ctx @unchecked]) =>
          m.afterQueryExtensions(v.asInstanceOf[m.QueryVal], middlewareCtx)
        case _ => Nil
      }

    if (extensions.nonEmpty) ResultResolver.marshalExtensions(marshaller, extensions)
    else None
  }

  private def immediatelyResolveDeferred[T](
      uc: Ctx,
      dr: DeferredResult,
      fn: Future[Result] => Future[T]): Future[T] = {
    val res = fn(dr.futureValue)

    resolveDeferredWithGrouping(dr.deferred).foreach(groups =>
      groups.foreach(group => resolveDeferred(uc, group)))

    res
  }

  private def resolveDeferredWithGrouping(deferred: Vector[Future[Vector[Defer]]]) =
    Future.sequence(deferred).map(listOfDef => deferredResolver.groupDeferred(listOfDef.flatten))

  private def resolveSubs[S[_]](
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields,
      errorReg: ErrorRegistry,
      requestedStream: Option[SubscriptionStream[S]])
      : (SubscriptionStream[S], S[(Vector[RegisteredError], marshaller.Node)]) = {
    val firstStream = tpe.uniqueFields.head.tags
      .collectFirst { case SubscriptionField(s) => s }
      .get
      .asInstanceOf[SubscriptionStream[S]]
    val stream = requestedStream.fold(firstStream) { s =>
      if (s.supported(firstStream)) s
      else
        throw new IllegalStateException(
          "Subscription type field stream implementation is incompatible with requested stream implementation")
    }

    def marshallResult(result: Result): Any =
      stream.single(result)

    val fieldStreams = fields.fields.flatMap {
      case CollectedField(_, origField, _) if tpe.getField(schema, origField.name).isEmpty =>
        None
      case CollectedField(_, origField, Failure(error)) =>
        val resMap = marshaller.emptyMapNode(Seq(origField.outputName))

        Some(
          marshallResult(Result(
            errorReg.add(path.add(origField, tpe), error),
            if (isOptional(tpe, origField.name))
              Some(marshaller
                .addMapNodeElem(resMap, origField.outputName, marshaller.nullNode, optional = true))
            else None
          )))
      case CollectedField(_, origField, Success(fields)) =>
        resolveField(
          userContext,
          tpe,
          path.add(origField, tpe),
          value,
          ErrorRegistry.empty,
          fields) match {
          case _: StandardFieldResolution =>
            throw new IllegalStateException(
              "StandardFieldResolution is not supposed to appear here")
          case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
            val resMap = marshaller.emptyMapNode(Seq(origField.outputName))

            Some(
              marshallResult(
                Result(
                  updatedErrors,
                  Some(
                    marshaller.addMapNodeElem(
                      resMap,
                      fields.head.outputName,
                      marshaller.nullNode,
                      optional = isOptional(tpe, origField.name)))
                )))
          case ErrorFieldResolution(updatedErrors) =>
            Some(marshallResult(Result(updatedErrors, None)))
          case StreamFieldResolution(updatedErrors, svalue, standardFn) =>
            val s = svalue.stream.mapFuture[Any, Result](svalue.source) { action =>
              val res =
                Result(updatedErrors, Some(marshaller.emptyMapNode(Seq(origField.outputName))))
              val standardAction = standardFn(action)

              resolveStandardFieldResolutionSeq(
                path,
                userContext,
                tpe,
                origField,
                fields,
                res,
                standardAction)
                .map(_._1)
            }

            val recovered = svalue.stream.recover(s) { e =>
              val resMap = marshaller.emptyMapNode(Seq(origField.outputName))

              Result(
                updatedErrors.add(path.add(origField, tpe), e),
                if (isOptional(tpe, origField.name))
                  Some(
                    marshaller.addMapNodeElem(
                      resMap,
                      origField.outputName,
                      marshaller.nullNode,
                      optional = true))
                else None
              )
            }

            Some(recovered)
        }
    }

    stream -> stream.mapFuture(stream.merge(fieldStreams.asInstanceOf[Vector[S[Result]]]))(r =>
      processFinalResolve(r.buildValue))
  }

  private def resolveSeq(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields): Future[(Result, Ctx)] =
    fields.fields
      .foldLeft(
        Future.successful(
          (
            Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode(fields.namesOrdered))),
            userContext))) { case (future, elem) =>
        future.flatMap { resAndCtx =>
          (resAndCtx, elem) match {
            case (acc @ (Result(_, None, _), _), _) => Future.successful(acc)
            case (acc, CollectedField(_, origField, _))
                if tpe.getField(schema, origField.name).isEmpty =>
              Future.successful(acc)
            case (
                  (Result(errors, Some(acc), _), uc),
                  CollectedField(_, origField, Failure(error))) =>
              Future.successful(Result(
                errors.add(path.add(origField, tpe), error),
                if (isOptional(tpe, origField.name))
                  Some(
                    marshaller.addMapNodeElem(
                      acc.asInstanceOf[marshaller.MapBuilder],
                      origField.outputName,
                      marshaller.nullNode,
                      optional = true))
                else None
              ) -> uc)
            case (
                  (accRes @ Result(errors, Some(acc), _), uc),
                  CollectedField(_, origField, Success(fields))) =>
              resolveSingleFieldSeq(path, uc, tpe, value, errors, origField, fields, accRes, acc)
          }
        }
      }
      .map { case (res, ctx) =>
        res.buildValue -> ctx
      }

  private def resolveSingleFieldSeq(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      value: Any,
      errors: ErrorRegistry,
      origField: ast.Field,
      fields: Vector[ast.Field],
      accRes: Result,
      acc: Any // from `accRes`
  ): Future[(Result, Ctx)] =
    resolveField(uc, tpe, path.add(origField, tpe), value, errors, fields) match {
      case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
        Future.successful(
          Result(
            updatedErrors,
            Some(
              marshaller.addMapNodeElem(
                acc.asInstanceOf[marshaller.MapBuilder],
                fields.head.outputName,
                marshaller.nullNode,
                optional = isOptional(tpe, origField.name)))
          ) -> uc)
      case ErrorFieldResolution(updatedErrors) =>
        Future.successful(Result(updatedErrors, None) -> uc)
      case resolution: StandardFieldResolution =>
        resolveStandardFieldResolutionSeq(path, uc, tpe, origField, fields, accRes, resolution)
      case _: StreamFieldResolution[_, _] =>
        Future.failed(
          new IllegalStateException("StreamFieldResolution is not supposed to happen here"))
    }

  private def resolveStandardFieldResolutionSeq(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      origField: ast.Field,
      fields: Vector[ast.Field],
      accRes: Result,
      resolution: StandardFieldResolution
  ): Future[(Result, Ctx)] = {
    val StandardFieldResolution(updatedErrors, result, newUc) = resolution
    val sfield = tpe.getField(schema, origField.name).head
    val fieldPath = path.add(fields.head, tpe)

    def resolveUc(v: Any) = newUc.fold(uc)(_.ctxFn(v))

    def resolveError(e: Throwable) = {
      try newUc.foreach(_.onError(e))
      catch {
        case NonFatal(ee) => ee.printStackTrace()
      }

      e
    }

    def resolveVal(v: Any) = newUc match {
      case Some(MappedCtxUpdate(_, mapFn, _)) => mapFn(v)
      case None => v
    }

    val resolve =
      try
        resolveStandardFieldResolutionSeqInner(
          path,
          uc,
          tpe,
          origField,
          fields,
          result,
          sfield,
          fieldPath,
          resolveUc,
          resolveError,
          resolveVal
        )
      catch {
        case NonFatal(e) =>
          Future.successful(
            Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None) -> uc)
      }

    resolve.flatMap {
      case (r: Result, newUc) =>
        Future.successful(
          accRes.addToMap(
            r,
            fields.head.outputName,
            isOptional(tpe, fields.head.name),
            fieldPath,
            fields.head.location,
            updatedErrors) -> newUc)
      case (dr: DeferredResult, newUc) =>
        immediatelyResolveDeferred(
          newUc,
          dr,
          _.map(
            accRes.addToMap(
              _,
              fields.head.outputName,
              isOptional(tpe, fields.head.name),
              fieldPath,
              fields.head.location,
              updatedErrors) -> newUc))
    }
  }

  protected def resolveStandardFieldResolutionSeqInner(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      origField: ast.Field,
      fields: Vector[ast.Field],
      result: LeafAction[Ctx, Any],
      sfield: Field[Ctx, _],
      fieldPath: ExecutionPath,
      resolveUc: Any => Ctx,
      resolveError: Throwable => Throwable,
      resolveVal: Any => Any): Future[(Resolve, Ctx)] =
    result match {
      case Value(v) =>
        val updatedUc = resolveUc(v)

        Future.successful(
          resolveValue(
            fieldPath,
            fields,
            sfield.fieldType,
            sfield,
            resolveVal(v),
            updatedUc) -> updatedUc)

      case SequenceLeafAction(actions) =>
        val values = resolveActionSequenceValues(fieldPath, fields, sfield, actions)
        val future = Future.sequence(values.map(_.value))

        val resolved = future
          .flatMap { vs =>
            val errors = vs.iterator.flatMap(_.errors).toVector
            val successfulValues = vs.collect { case SeqFutRes(v, _, _) if v != null => v }
            val dctx = vs.collect { case SeqFutRes(_, _, d) if d != null => d }

            def resolveDctx(resolve: Resolve) = {
              val last = dctx.lastOption
              val init = if (dctx.isEmpty) dctx else dctx.init

              resolve match {
                case res: Result =>
                  dctx.foreach(_.promise.success(Vector.empty))
                  Future.successful(res)
                case res: DeferredResult =>
                  init.foreach(_.promise.success(Vector.empty))
                  last.foreach(_.promise.success(res.deferred))
                  res.futureValue
              }
            }

            errors.foreach(resolveError)

            if (successfulValues.size == vs.size)
              resolveDctx(
                resolveValue(
                  fieldPath,
                  fields,
                  sfield.fieldType,
                  sfield,
                  resolveVal(successfulValues),
                  resolveUc(successfulValues))
                  .appendErrors(fieldPath, errors, fields.head.location))
            else
              resolveDctx(
                Result(ErrorRegistry.empty.append(fieldPath, errors, fields.head.location), None))
          }
          .recover { case e =>
            Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None)
          }

        val deferred = values.iterator.collect {
          case SeqRes(_, d, _) if d != null => d
        }.toVector
        val deferredFut = values.iterator.collect {
          case SeqRes(_, _, d) if d != null => d
        }.toVector

        immediatelyResolveDeferred(
          uc,
          DeferredResult(Future.successful(deferred) +: deferredFut, resolved),
          _.map(r => r -> r.userContext.getOrElse(uc)))

      case PartialValue(v, es) =>
        val updatedUc = resolveUc(v)

        es.foreach(resolveError)

        Future.successful(
          resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
            .appendErrors(fieldPath, es, fields.head.location) -> updatedUc)

      case TryValue(v) =>
        Future.successful(v match {
          case Success(success) =>
            val updatedUc = resolveUc(success)

            resolveValue(
              fieldPath,
              fields,
              sfield.fieldType,
              sfield,
              resolveVal(success),
              updatedUc) -> updatedUc
          case Failure(e) =>
            Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None) -> uc
        })

      case DeferredValue(d) =>
        val p = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
        val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
        val defer = Defer(p, d, complexity, sfield, fields, args)

        immediatelyResolveDeferred(
          uc,
          DeferredResult(
            Vector(Future.successful(Vector(defer))),
            p.future
              .flatMap { case (dctx, v, es) =>
                val updatedUc = resolveUc(v)

                es.foreach(resolveError)

                resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
                  .appendErrors(fieldPath, es, fields.head.location) match {
                  case r: Result => dctx.resolveResult(r.copy(userContext = Some(updatedUc)))
                  case er: DeferredResult =>
                    dctx
                      .resolveDeferredResult(er)
                      .map(_.copy(userContext = Some(updatedUc)))
                }
              }
              .recover { case e =>
                Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None)
              }
          ),
          _.map(r => r -> r.userContext.getOrElse(uc))
        )

      case FutureValue(f) =>
        f.map { v =>
          val updatedUc = resolveUc(v)

          resolveValue(
            fieldPath,
            fields,
            sfield.fieldType,
            sfield,
            resolveVal(v),
            updatedUc) -> updatedUc
        }.recover { case e =>
          Result(
            ErrorRegistry(path.add(origField, tpe), resolveError(e), fields.head.location),
            None) -> uc
        }

      case PartialFutureValue(f) =>
        f.map { case PartialValue(v, es) =>
          val updatedUc = resolveUc(v)

          es.foreach(resolveError)

          resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
            .appendErrors(fieldPath, es, fields.head.location) -> updatedUc
        }.recover { case e =>
          Result(
            ErrorRegistry(path.add(origField, tpe), resolveError(e), fields.head.location),
            None) -> uc
        }

      case DeferredFutureValue(df) =>
        val p = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
        def defer(d: Deferred[Any]) = {
          val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
          Defer(p, d, complexity, sfield, fields, args)
        }

        val actualDeferred = df
          .map(d => Vector(defer(d)))
          .recover { case NonFatal(e) =>
            p.failure(e)
            Vector.empty
          }

        immediatelyResolveDeferred(
          uc,
          DeferredResult(
            Vector(actualDeferred),
            p.future
              .flatMap { case (dctx, v, es) =>
                val updatedUc = resolveUc(v)

                es.foreach(resolveError)

                resolveValue(fieldPath, fields, sfield.fieldType, sfield, resolveVal(v), updatedUc)
                  .appendErrors(fieldPath, es, fields.head.location) match {
                  case r: Result => dctx.resolveResult(r.copy(userContext = Some(updatedUc)))
                  case er: DeferredResult =>
                    dctx
                      .resolveDeferredResult(er)
                      .map(_.copy(userContext = Some(updatedUc)))
                }
              }
              .recover { case e =>
                Result(ErrorRegistry(fieldPath, resolveError(e), fields.head.location), None)
              }
          ),
          _.map(r => r -> r.userContext.getOrElse(uc))
        )

      case SubscriptionValue(_, _) =>
        Future.failed(
          new IllegalStateException("Subscription values are not supported for normal operations"))

      case e =>
        Future.failed(
          new IllegalStateException(s"${e.getClass.toString} is not supposed to appear here"))
    }

  private def resolveActionSequenceValues(
      fieldsPath: ExecutionPath,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      actions: Seq[LeafAction[Any, Any]]): Seq[SeqRes] =
    actions.map {
      case Value(v) => SeqRes(SeqFutRes(v))
      case TryValue(Success(v)) => SeqRes(SeqFutRes(v))
      case TryValue(Failure(e)) => SeqRes(SeqFutRes(errors = Vector(e)))
      case PartialValue(v, es) => SeqRes(SeqFutRes(v, es))
      case FutureValue(future) =>
        SeqRes(future.map(v => SeqFutRes(v)).recover { case e => SeqFutRes(errors = Vector(e)) })
      case PartialFutureValue(future) =>
        SeqRes(future.map { case PartialValue(v, es) => SeqFutRes(v, es) }.recover { case e =>
          SeqFutRes(errors = Vector(e))
        })
      case DeferredValue(deferred) =>
        val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
        val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
        val defer = Defer(promise, deferred, complexity, field, astFields, args)

        SeqRes(
          promise.future.map { case (dctx, v, es) => SeqFutRes(v, es, dctx) }.recover { case e =>
            SeqFutRes(errors = Vector(e))
          },
          defer)
      case DeferredFutureValue(deferredValue) =>
        val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()

        def defer(d: Deferred[Any]) = {
          val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
          Defer(promise, d, complexity, field, astFields, args)
        }

        val actualDeferred = deferredValue
          .map(d => Vector(defer(d)))
          .recover { case NonFatal(e) =>
            promise.failure(e)
            Vector.empty
          }

        SeqRes(
          promise.future.map { case (dctx, v, es) => SeqFutRes(v, es, dctx) }.recover { case e =>
            SeqFutRes(errors = Vector(e))
          },
          actualDeferred)
      case SequenceLeafAction(_) | _: MappedSequenceLeafAction[_, _, _] =>
        SeqRes(SeqFutRes(errors = Vector(new IllegalStateException(
          "Nested `SequenceLeafAction` is not yet supported inside of another `SequenceLeafAction`"))))
      case SubscriptionValue(_, _) =>
        SeqRes(
          SeqFutRes(errors = Vector(new IllegalStateException(
            "Subscription values are not supported for normal operations"))))
      case e =>
        SeqRes(
          SeqFutRes(errors = Vector(
            new IllegalStateException(s"${e.getClass.toString} is not supposed to appear here"))))
    }

  private def resolveActionsPar(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      actions: Actions,
      userCtx: Ctx,
      fieldsNamesOrdered: Vector[String]): Resolve =
    actions.actions match {
      case None => Result(actions.errorRegistry, None)
      case Some(results) =>
        val complexResBuilder: VectorBuilder[(ast.Field, DeferredResult)] = new VectorBuilder

        val resSoFar =
          results.iterator
            .map { a =>
              val (field, result) = a.result match {
                case None => a.fields.head -> Result(ErrorRegistry.empty, None)
                case Some(r) =>
                  resolveLeafAction(path, tpe, userCtx, a.fields, r.field, r.updateCtx)(r.action)
              }
              result match {
                case r: Result => Some((field, r))
                case r: DeferredResult =>
                  complexResBuilder += ((field, r))
                  None
              }
            }
            .collect { case Some(f) => f }
            .foldLeft(
              Result(actions.errorRegistry, Some(marshaller.emptyMapNode(fieldsNamesOrdered)))) {
              case (acc, (astField, other)) =>
                acc.addToMap(
                  other,
                  astField.outputName,
                  isOptional(tpe, astField.name),
                  path.add(astField, tpe),
                  astField.location,
                  acc.errors)
            }

        val complexRes = complexResBuilder.result()

        if (complexRes.isEmpty) resSoFar.buildValue
        else {
          val allDeferred = complexRes.flatMap(_._2.deferred)
          val finalValue = Future
            .sequence(complexRes.iterator.map { case (astField, DeferredResult(_, future)) =>
              future.map(astField -> _)
            })
            .map { results =>
              results
                .foldLeft(resSoFar) { case (acc, (astField, other)) =>
                  acc.addToMap(
                    other,
                    astField.outputName,
                    isOptional(tpe, astField.name),
                    path.add(astField, tpe),
                    astField.location,
                    acc.errors)
                }
                .buildValue
            }

          DeferredResult(allDeferred, finalValue)
        }
    }

  protected def resolveLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: LeafAction[Ctx, Any]): (ast.Field, Resolve) =
    action match {
      case a: StandardLeafAction[Ctx, Any] =>
        resolveStandardLeafAction(path, tpe, userCtx, astFields, field, updateCtx)(a)
      case other => unresolvableLeafAction(path, tpe, astFields, updateCtx)(other)
    }

  protected def unresolvableLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      astFields: Vector[ast.Field],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: LeafAction[Ctx, Any]): (ast.Field, Resolve) =
    illegalActionException(path, tpe, astFields, updateCtx)(
      s"Action of type '${action.getClass.toString}' is not supported by this resolver")

  protected def resolveStandardLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: StandardLeafAction[Ctx, Any]): (ast.Field, Resolve) =
    action match {
      case v: Value[Ctx, Any] => resolveValue(path, tpe, userCtx, astFields, field, updateCtx)(v)
      case t: TryValue[Ctx, Any] =>
        resolveTryValue(path, tpe, userCtx, astFields, field, updateCtx)(t)
      case p: PartialValue[Ctx, Any] =>
        resolvePartialValue(path, tpe, userCtx, astFields, field, updateCtx)(p)
      case f: FutureValue[Ctx, Any] =>
        resolveFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(f)
      case p: PartialFutureValue[Ctx, Any] =>
        resolvePartialFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(p)
      case d: DeferredValue[Ctx, Any] =>
        resolveDeferredValue(path, tpe, userCtx, astFields, field, updateCtx)(d)
      case d: DeferredFutureValue[Ctx, Any] =>
        resolveDeferredFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(d)
      case s: SequenceLeafAction[Ctx, Any] @unchecked =>
        resolveSequenceLeafAction(path, tpe, userCtx, astFields, field, updateCtx)(s)
      case _: MappedSequenceLeafAction[_, _, _] =>
        illegalActionException(path, tpe, astFields, updateCtx)(
          "MappedSequenceLeafAction is not supposed to appear here")
      case _: SubscriptionValue[_, _, _] =>
        illegalActionException(path, tpe, astFields, updateCtx)(
          "Subscription values are not supported for normal operations")
    }

  private def resolveValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      value: Value[Ctx, Any]): (ast.Field, Resolve) = {
    val v = value.value
    val fieldsPath = path.add(astFields.head, tpe)

    try
      astFields.head -> resolveValue(
        fieldsPath,
        astFields,
        field.fieldType,
        field,
        resolveVal(updateCtx, v),
        resolveUc(updateCtx, v, userCtx))
    catch {
      case NonFatal(e) =>
        astFields.head -> Result(
          ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
          None)
    }
  }

  private def resolveSequenceLeafAction(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: SequenceLeafAction[Ctx, Any]): (ast.Field, Resolve) = {
    val actions = action.value
    val fieldsPath = path.add(astFields.head, tpe)
    val values = resolveActionSequenceValues(fieldsPath, astFields, field, actions)
    val future = Future.sequence(values.map(_.value))

    val resolved = future
      .flatMap { vs =>
        val errors = vs.iterator.flatMap(_.errors).toVector
        val successfulValues = vs.collect { case SeqFutRes(v, _, _) if v != null => v }
        val dctx = vs.collect { case SeqFutRes(_, _, d) if d != null => d }

        def resolveDctx(resolve: Resolve) = {
          val last = dctx.lastOption
          val init = if (dctx.isEmpty) dctx else dctx.init

          resolve match {
            case res: Result =>
              dctx.foreach(_.promise.success(Vector.empty))
              Future.successful(res)
            case res: DeferredResult =>
              init.foreach(_.promise.success(Vector.empty))
              last.foreach(_.promise.success(res.deferred))
              res.futureValue
          }
        }

        errors.foreach(resolveError(updateCtx, _))

        if (successfulValues.size == vs.size)
          resolveDctx(
            resolveValue(
              fieldsPath,
              astFields,
              field.fieldType,
              field,
              resolveVal(updateCtx, successfulValues),
              resolveUc(updateCtx, successfulValues, userCtx))
              .appendErrors(fieldsPath, errors, astFields.head.location))
        else
          resolveDctx(
            Result(ErrorRegistry.empty.append(fieldsPath, errors, astFields.head.location), None))
      }
      .recover { case e =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      }

    val deferred = values.iterator.collect {
      case SeqRes(_, d, _) if d != null => d
    }.toVector
    val deferredFut = values.iterator.collect {
      case SeqRes(_, _, d) if d != null => d
    }.toVector

    astFields.head -> DeferredResult(Future.successful(deferred) +: deferredFut, resolved)
  }

  private def resolvePartialValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      p: PartialValue[Ctx, Any]): (ast.Field, Resolve) = {
    val v = p.value
    val es = p.errors

    val fieldsPath = path.add(astFields.head, tpe)

    es.foreach(resolveError(updateCtx, _))

    try
      astFields.head ->
        resolveValue(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx))
          .appendErrors(fieldsPath, es, astFields.head.location)
    catch {
      case NonFatal(e) =>
        astFields.head -> Result(
          ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location)
            .append(fieldsPath, es, astFields.head.location),
          None)
    }
  }

  private def resolveTryValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      t: TryValue[Ctx, Any]): (ast.Field, Resolve) = {
    val v = t.value
    val fieldsPath = path.add(astFields.head, tpe)

    v match {
      case Success(success) =>
        try
          astFields.head -> resolveValue(
            fieldsPath,
            astFields,
            field.fieldType,
            field,
            resolveVal(updateCtx, success),
            resolveUc(updateCtx, success, userCtx))
        catch {
          case NonFatal(e) =>
            astFields.head -> Result(
              ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
              None)
        }
      case Failure(e) =>
        astFields.head -> Result(
          ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
          None)
    }
  }

  private def resolveDeferredValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      d: DeferredValue[Ctx, Any]): (ast.Field, Resolve) = {
    val deferred = d.value
    val fieldsPath = path.add(astFields.head, tpe)
    val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()
    val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
    val defer = Defer(promise, deferred, complexity, field, astFields, args)

    astFields.head -> DeferredResult(
      Vector(Future.successful(Vector(defer))),
      promise.future
        .flatMap { case (dctx, v, es) =>
          val uc = resolveUc(updateCtx, v, userCtx)

          es.foreach(resolveError(updateCtx, _))

          resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc)
            .appendErrors(fieldsPath, es, astFields.head.location) match {
            case r: Result => dctx.resolveResult(r)
            case er: DeferredResult => dctx.resolveDeferredResult(er)
          }
        }
        .recover { case e =>
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None)
        }
    )
  }

  protected def resolveFutureValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      f: FutureValue[Ctx, Any]): (ast.Field, Resolve) = {
    val future = f.value
    val fieldsPath = path.add(astFields.head, tpe)

    val resolved = future
      .map(v =>
        resolveValue(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx)))
      .recover { case e =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      }

    def process() = {
      val deferred = resolved.flatMap {
        case _: Result => Future.successful(Vector.empty)
        case r: DeferredResult => Future.sequence(r.deferred).map(_.flatten)
      }

      val value = resolved.flatMap {
        case r: Result => Future.successful(r)
        case dr: DeferredResult => dr.futureValue
      }

      astFields.head -> DeferredResult(Vector(deferred), value)
    }

    def processAndResolveDeferred() = {
      val value = resolved.flatMap {
        case r: Result => Future.successful(r)
        case dr: DeferredResult => immediatelyResolveDeferred(userContext, dr, identity)
      }

      astFields.head -> DeferredResult(Vector.empty, value)
    }

    deferredResolver.includeDeferredFromField match {
      case Some(fn) =>
        val (args, complexity) =
          calcComplexity(fieldsPath, astFields.head, field, userContext)

        if (fn(field, astFields, args, complexity))
          process()
        else
          processAndResolveDeferred()
      case None =>
        process()
    }
  }

  private def resolvePartialFutureValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      p: PartialFutureValue[Ctx, Any]): (ast.Field, Resolve) = {
    val future = p.value
    val fieldsPath = path.add(astFields.head, tpe)

    val resolved = future
      .map { case PartialValue(v, es) =>
        es.foreach(resolveError(updateCtx, _))

        resolveValue(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx))
          .appendErrors(fieldsPath, es, astFields.head.location)
      }
      .recover { case e =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      }

    val deferred = resolved.flatMap {
      case _: Result => Future.successful(Vector.empty)
      case r: DeferredResult => Future.sequence(r.deferred).map(_.flatten)
    }
    val value = resolved.flatMap {
      case r: Result => Future.successful(r)
      case dr: DeferredResult => dr.futureValue
    }

    astFields.head -> DeferredResult(Vector(deferred), value)
  }

  private def resolveDeferredFutureValue(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      d: DeferredFutureValue[Ctx, Any]): (ast.Field, Resolve) = {
    val deferredValue = d.value
    val fieldsPath = path.add(astFields.head, tpe)
    val promise = Promise[(ChildDeferredContext, Any, Vector[Throwable])]()

    def defer(d: Deferred[Any]) = {
      val (args, complexity) =
        calcComplexity(fieldsPath, astFields.head, field, userContext)
      Defer(promise, d, complexity, field, astFields, args)
    }

    val actualDeferred = deferredValue
      .map(d => Vector(defer(d)))
      .recover { case NonFatal(e) =>
        promise.failure(e)
        Vector.empty
      }

    astFields.head -> DeferredResult(
      Vector(actualDeferred),
      promise.future
        .flatMap { case (dctx, v, es) =>
          val uc = resolveUc(updateCtx, v, userCtx)

          es.foreach(resolveError(updateCtx, _))

          resolveValue(fieldsPath, astFields, field.fieldType, field, resolveVal(updateCtx, v), uc)
            .appendErrors(fieldsPath, es, astFields.head.location) match {
            case r: Result => dctx.resolveResult(r)
            case er: DeferredResult => dctx.resolveDeferredResult(er)
          }
        }
        .recover { case e =>
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None)
        }
    )
  }

  private def illegalActionException(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      astFields: Vector[ast.Field],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(msg: String): (ast.Field, Resolve) = {
    val fieldsPath = path.add(astFields.head, tpe)
    val error = new IllegalStateException(msg)

    astFields.head -> Result(
      ErrorRegistry(fieldsPath, resolveError(updateCtx, error), astFields.head.location),
      None)
  }

  private def resolveDeferred(uc: Ctx, toResolve: Vector[Defer]): Unit =
    if (toResolve.nonEmpty) {
      @tailrec
      def findActualDeferred(deferred: Deferred[_]): Deferred[_] = deferred match {
        case MappingDeferred(d, _) => findActualDeferred(d)
        case d => d
      }

      def mapAllDeferred(
          deferred: Deferred[_],
          value: Future[Any]): Future[(Any, Vector[Throwable])] = deferred match {
        case MappingDeferred(d, fn) =>
          mapAllDeferred(d, value).map { case (v, errors) =>
            val (mappedV, newErrors) = fn.asInstanceOf[Any => (Any, Seq[Throwable])](v)
            mappedV -> (errors ++ newErrors)
          }
        case _ => value.map(_ -> Vector.empty)
      }

      try {
        val resolved = deferredResolver.resolve(
          toResolve.map(d => findActualDeferred(d.deferred)),
          uc,
          deferredResolverState)

        if (toResolve.size == resolved.size) {
          val dctx = ParentDeferredContext(uc, toResolve.size)

          for (i <- toResolve.indices) {
            val toRes = toResolve(i)

            toRes.promise.completeWith(
              mapAllDeferred(toRes.deferred, resolved(i))
                .map(v => (dctx.children(i), v._1, v._2))
                .recover { case NonFatal(e) =>
                  dctx.children(i).resolveError(e)
                  throw e
                })
          }

          dctx.init()
        } else {
          toResolve.foreach(_.promise.failure(new IllegalStateException(
            s"Deferred resolver returned ${resolved.size} elements, but it got ${toResolve.size} deferred values. This violates the contract. You can find more information in the documentation: https://sangria-graphql.github.io/learn/#deferred-values-and-resolver")))
        }
      } catch {
        case NonFatal(error) => toResolve.foreach(_.promise.failure(error))
      }
    }

  private def resolveValue(
      path: ExecutionPath,
      astFields: Vector[ast.Field],
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any,
      userCtx: Ctx,
      actualType: Option[InputType[_]] = None): Resolve =
    tpe match {
      case OptionType(optTpe) =>
        val actualValue = value match {
          case v: Option[_] => v
          case v => Option(v)
        }

        actualValue match {
          case Some(someValue) =>
            resolveValue(path, astFields, optTpe, field, someValue, userCtx, None)
          case None => Result(ErrorRegistry.empty, None)
        }
      case ListType(listTpe) =>
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else {
          // this is very hot place, so resorting to mutability to minimize the footprint
          val simpleResBuilder: VectorBuilder[Result] = new VectorBuilder

          val actualValue = value match {
            case seq: Iterable[_] => seq.iterator
            case other => Iterator.single(other)
          }

          val res: Vector[Resolve] = actualValue.zipWithIndex.map { case (v, idx) =>
            val result =
              resolveValue(path.withIndex(idx), astFields, listTpe, field, v, userCtx, None)
            if (result.isInstanceOf[Result]) simpleResBuilder += result.asInstanceOf[Result]
            result
          }.toVector

          val simpleRes = simpleResBuilder.result()
          val optional = isOptional(listTpe)

          val resSize = res.size
          if (simpleRes.size == resSize)
            resolveSimpleListValue(simpleRes, path, optional, astFields.head.location)
          else {
            val deferredBuilder = new VectorBuilder[Future[Vector[Defer]]]
            val resultFutures = new VectorBuilder[Future[Result]]
            resultFutures.sizeHint(resSize)

            val resIt = res.iterator

            while (resIt.hasNext)
              resIt.next() match {
                case r: Result =>
                  resultFutures += Future.successful(r)
                case dr: DeferredResult =>
                  resultFutures += dr.futureValue
                  deferredBuilder ++= dr.deferred
              }

            DeferredResult(
              deferred = deferredBuilder.result(),
              futureValue = Future
                .sequence(resultFutures.result())
                .map(resolveSimpleListValue(_, path, optional, astFields.head.location))
            )
          }
        }
      case scalar: ScalarType[Any @unchecked] =>
        try
          Result(
            ErrorRegistry.empty,
            if (isUndefinedValue(value))
              None
            else {
              val coerced = scalar.coerceOutput(value, marshaller.capabilities)

              if (isUndefinedValue(coerced)) {
                None
              } else {
                val coercedWithMiddleware =
                  toScalarMiddleware match {
                    case Some(fn) => fn(coerced, actualType.getOrElse(scalar)).getOrElse(coerced)
                    case None => coerced
                  }

                Some(
                  marshalScalarValue(
                    coercedWithMiddleware,
                    marshaller,
                    scalar.name,
                    scalar.scalarInfo))
              }
            }
          )
        catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case scalar: ScalarAlias[Any @unchecked, Any @unchecked] =>
        resolveValue(
          path,
          astFields,
          scalar.aliasFor,
          field,
          scalar.toScalar(value),
          userCtx,
          Some(scalar))
      case enumT: EnumType[Any @unchecked] =>
        try
          Result(
            ErrorRegistry.empty,
            if (isUndefinedValue(value))
              None
            else {
              val coerced = enumT.coerceOutput(value)

              if (isUndefinedValue(coerced))
                None
              else
                Some(marshalEnumValue(coerced, marshaller, enumT.name))
            }
          )
        catch {
          case NonFatal(e) => Result(ErrorRegistry(path, e), None)
        }
      case obj: ObjectType[Ctx @unchecked, _] =>
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else
          fieldCollector.collectFields(path, obj, astFields) match {
            case Success(fields) =>
              val actions =
                collectActionsPar(path, obj, value, fields, ErrorRegistry.empty, userCtx)

              resolveActionsPar(path, obj, actions, userCtx, fields.namesOrdered)
            case Failure(error) => Result(ErrorRegistry(path, error), None)
          }
      case abst: AbstractType =>
        if (isUndefinedValue(value))
          Result(ErrorRegistry.empty, None)
        else {
          val actualValue =
            abst match {
              case abst: MappedAbstractType[Any @unchecked] => abst.contraMap(value)
              case _ => value
            }

          abst.typeOf(actualValue, schema) match {
            case Some(obj) =>
              resolveValue(path, astFields, obj, field, actualValue, userCtx, None)
            case None =>
              Result(
                ErrorRegistry(
                  path,
                  UndefinedConcreteTypeError(
                    path,
                    abst,
                    schema.possibleTypes.getOrElse(abst.name, Vector.empty),
                    actualValue,
                    exceptionHandler,
                    sourceMapper,
                    astFields.head.location.toList)
                ),
                None
              )
          }
        }
    }

  private case class DeferredResult(
      deferred: Vector[Future[Vector[Defer]]],
      futureValue: Future[Result])
      extends Resolve {
    def appendErrors(
        path: ExecutionPath,
        errors: Vector[Throwable],
        position: Option[AstLocation]): DeferredResult =
      if (errors.nonEmpty)
        copy(futureValue = futureValue.map(_.appendErrors(path, errors, position)))
      else this
  }

  private case class Defer(
      promise: Promise[(ChildDeferredContext, Any, Vector[Throwable])],
      deferred: Deferred[Any],
      complexity: Double,
      field: Field[_, _],
      astFields: Vector[ast.Field],
      args: Args)
      extends DeferredWithInfo

  private case class ParentDeferredContext(uc: Ctx, expectedBranches: Int) {
    val children: Vector[ChildDeferredContext] =
      Vector.fill(expectedBranches)(ChildDeferredContext(Promise[Vector[Future[Vector[Defer]]]]()))

    def init(): Unit =
      Future.sequence(children.map(_.promise.future)).onComplete { res =>
        val allDeferred = res.get.flatten

        if (allDeferred.nonEmpty)
          resolveDeferredWithGrouping(allDeferred).foreach(groups =>
            groups.foreach(group => resolveDeferred(uc, group)))
      }
  }

  private case class ChildDeferredContext(promise: Promise[Vector[Future[Vector[Defer]]]]) {
    def resolveDeferredResult(res: DeferredResult): Future[Result] = {
      promise.success(res.deferred)
      res.futureValue
    }

    def resolveResult(res: Result): Future[Result] = {
      promise.success(Vector.empty)
      Future.successful(res)
    }

    def resolveError(e: Throwable): Unit =
      promise.success(Vector.empty)
  }

  private case class SeqRes(value: Future[SeqFutRes], defer: Defer, deferFut: Future[Vector[Defer]])

  private object SeqRes {
    def apply(value: SeqFutRes): SeqRes = SeqRes(Future.successful(value), null, null)
    def apply(value: SeqFutRes, defer: Defer): SeqRes =
      SeqRes(Future.successful(value), defer, null)
    def apply(value: SeqFutRes, deferFut: Future[Vector[Defer]]): SeqRes =
      SeqRes(Future.successful(value), null, deferFut)

    def apply(value: Future[SeqFutRes]): SeqRes = SeqRes(value, null, null)
    def apply(value: Future[SeqFutRes], defer: Defer): SeqRes = SeqRes(value, defer, null)
    def apply(value: Future[SeqFutRes], deferFut: Future[Vector[Defer]]): SeqRes =
      SeqRes(value, null, deferFut)
  }

  private case class SeqFutRes(
      value: Any = null,
      errors: Vector[Throwable] = Vector.empty,
      dctx: ChildDeferredContext = null)
}
