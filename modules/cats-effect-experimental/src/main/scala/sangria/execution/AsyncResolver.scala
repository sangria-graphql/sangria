package sangria.execution

import cats.effect.Async
import cats.implicits._
import sangria.ast
import sangria.ast.{AstLocation, Document, SourceMapper}
import sangria.catseffect.schema.AsyncValue
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.marshalling.ResultMarshaller
import sangria.schema._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

private[execution] class AsyncResolverBuilder[F[_]: Async] extends ResolverBuilder {
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
    new AsyncResolver[Ctx, F](
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

/** The [[Resolver]] that is used to resolve [[AsyncValue]].
  *
  * Unlike a plain `Future`/`Promise` based resolver, this implementation is expressed natively in
  * terms of `F` (via [[Async]]): the field-resolution algorithm itself is a self-contained,
  * recursive `F`-based computation (see [[Resolve]]/[[Result]]/[[Pending]]/[[runToCompletion]]),
  * and it does not construct or delegate to [[FutureResolver]] at all.
  *
  * The only place a [[scala.concurrent.Future]] is still involved is at the boundary with APIs that
  * are inherently `Future`-shaped and are not under this resolver's control:
  * [[sangria.execution.deferred.DeferredResolver#resolve]] and the `Future`-based `LeafAction`s
  * ([[FutureValue]], [[PartialFutureValue]], [[DeferredFutureValue]]) that are part of the core
  * `sangria.schema.Action` API. Those `Future`s are lifted into `F` via `Async.fromFuture` as soon
  * as they are encountered.
  *
  * Deferred-value batching is achieved without `Promise`/callback coordination: resolving a
  * (possibly deeply nested) subtree produces either a finished [[Result]] or a [[Pending]] carrying
  * every currently-known [[Defer]] together with a `resume` continuation. Siblings are merged (see
  * [[sequenceResolve]]) before anything is dispatched, and [[runToCompletion]] simply loops:
  * dispatch the current wave, resume, repeat - which naturally reproduces the original wave-by-wave
  * batching behaviour.
  */
private[execution] class AsyncResolver[Ctx, F[_]: Async](
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
    middleware: List[(Any, Middleware[Ctx])],
    val beforeFieldMiddlewares: List[(Any, MiddlewareBeforeField[Ctx])],
    val maxQueryDepth: Option[Int],
    val deferredResolverState: Any,
    preserveOriginalErrors: Boolean,
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement,
    val queryAst: ast.Document
) extends Resolver[Ctx]
    with FieldResolutionSupport[Ctx] {

  private val asyncF: Async[F] = Async[F]

  protected val resultResolver =
    new ResultResolver(marshaller, exceptionHandler, preserveOriginalErrors)
  private val toScalarMiddleware =
    Middleware.composeToScalarMiddleware(middleware.map(_._2), userContext)

  private val deferIdCounter = new java.util.concurrent.atomic.AtomicLong(0)
  private def nextDeferId(): Long = deferIdCounter.getAndIncrement()

  private val lineageIdCounter = new java.util.concurrent.atomic.AtomicLong(1)
  private def nextLineageId(): Long = lineageIdCounter.getAndIncrement()

  import Resolver._
  import resultResolver._

  private def liftFuture[A](future: => Future[A]): F[A] = asyncF.fromFuture(asyncF.delay(future))

  // ---------------------------------------------------------------------------------------------
  // Entry points
  // ---------------------------------------------------------------------------------------------

  override def resolveFieldsPar(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    scheme match {
      case _: AsyncExecutionScheme[F @unchecked] =>
        resolveFieldsParF(tpe, value, fields).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
      case other =>
        throw new IllegalStateException(s"Unsupported execution scheme: $other")
    }

  override def resolveFieldsSeq(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    scheme match {
      case _: AsyncExecutionScheme[F @unchecked] =>
        resolveFieldsSeqF(tpe, value, fields).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
      case other =>
        throw new IllegalStateException(s"Unsupported execution scheme: $other")
    }

  override def resolveFieldsSubs(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    throw new UnsupportedOperationException("Subscriptions are not supported by AsyncResolver")

  private def resolveFieldsParF(
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields): F[marshaller.Node] = {
    val actions =
      collectActionsPar(ExecutionPath.empty, tpe, value, fields, ErrorRegistry.empty, userContext)

    for {
      resolve <- resolveActionsParF(
        ExecutionPath.empty,
        tpe,
        actions,
        userContext,
        fields.namesOrdered)
      result <- runToCompletion(userContext, resolve)
      node <- processFinalResolve(result)
    } yield node
  }

  private def resolveFieldsSeqF(
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields): F[marshaller.Node] =
    resolveSeqF(ExecutionPath.empty, tpe, value, fields).flatMap { case (result, _) =>
      processFinalResolve(result)
    }

  private def processFinalResolve(result: Result): F[marshaller.Node] =
    asyncF.pure(
      marshalResult(
        result.value.asInstanceOf[Option[resultResolver.marshaller.Node]],
        marshalErrors(result.errors),
        marshallExtensions.asInstanceOf[Option[resultResolver.marshaller.Node]],
        beforeExecution = false
      ).asInstanceOf[marshaller.Node])

  private def marshallExtensions: Option[marshaller.Node] = {
    val extensions =
      middleware.flatMap {
        case (v, m: MiddlewareExtension[Ctx @unchecked]) =>
          m.afterQueryExtensions(v.asInstanceOf[m.QueryVal], middlewareCtx)
        case _ => Nil
      }

    if (extensions.nonEmpty) ResultResolver.marshalExtensions(marshaller, extensions)
    else None
  }

  // ---------------------------------------------------------------------------------------------
  // The batching engine: Resolve / Result / Pending / Defer, and the wave-driving loop
  // ---------------------------------------------------------------------------------------------

  private type DeferOutcome = Either[Throwable, (Any, Vector[Throwable])]

  private case class Defer(
      id: Long,
      deferred: Deferred[Any],
      complexity: Double,
      field: Field[_, _],
      astFields: Vector[ast.Field],
      args: Args)
      extends DeferredWithInfo

  /** A subtree whose value depends on one or more [[Defer]]s that still need to be dispatched
    * (batched, via the [[DeferredResolver]]) before `resume` can produce the next step - which may
    * itself be another [[Pending]] (the next wave).
    *
    * `lineages` tags each defer with the identity of the dispatch "cascade" it belongs to (see
    * [[dispatchAll]]): once [[DeferredResolver#groupDeferred]] has split a set of defers into
    * several groups, each group is assigned a fresh lineage id, and everything discovered further
    * downstream of that group keeps that same id. This is what lets [[dispatchAll]] keep two
    * dispatch groups permanently separate even after they bubble back up and get merged together
    * with unrelated siblings by [[sequenceResolve]] - matching the original resolver's behaviour of
    * scoping a dispatch's follow-up wave to that exact dispatch, regardless of what else is
    * happening elsewhere in the tree at the same time.
    */
  private case class Pending(
      defers: Vector[Defer],
      lineages: Vector[Long],
      resume: Vector[(DeferOutcome, Long)] => F[Resolve])
      extends Resolve {
    def appendErrors(
        path: ExecutionPath,
        e: Vector[Throwable],
        position: Option[AstLocation]): Resolve =
      if (e.isEmpty) this
      else
        Pending(defers, lineages, values => resume(values).map(_.appendErrors(path, e, position)))
  }

  /** A freshly minted, single-defer [[Pending]] doesn't belong to any dispatch cascade yet, so it
    * gets a placeholder lineage - [[dispatchAll]] assigns it a real one the moment it is
    * dispatched.
    */
  private val NoLineage = 0L

  /** Overwrites a freshly-produced [[Resolve]]'s lineage with the one just assigned to the defer
    * that produced it, so that everything discovered downstream of one dispatch group keeps tracing
    * back to that same group.
    */
  private def retag(resolve: Resolve, lineage: Long): Resolve = resolve match {
    case r: Result => r
    case p: Pending => p.copy(lineages = Vector.fill(p.defers.size)(lineage))
  }

  private def tagUserContext(resolve: Resolve, ctx: Ctx): Resolve = resolve match {
    case r: Result => r.copy(userContext = Some(ctx))
    case p: Pending =>
      p.copy(resume = values => p.resume(values).map(tagUserContext(_, ctx)))
  }

  /** Merges sibling [[Resolve]]s: if all of them are already [[Result]]s, `combine` is applied
    * immediately; otherwise every currently pending [[Defer]] across all siblings is merged into
    * one combined [[Pending]], so that they get dispatched together as a single wave (unless they
    * already carry different lineages from an earlier split, in which case [[dispatchAll]] keeps
    * them apart regardless of this merge).
    */
  private def sequenceResolve[A](items: Vector[(A, Resolve)])(
      combine: Vector[(A, Result)] => Result): Resolve =
    if (items.forall(_._2.isInstanceOf[Result]))
      combine(items.asInstanceOf[Vector[(A, Result)]])
    else {
      val sizes = items.map {
        case (_, p: Pending) => p.defers.size
        case _ => 0
      }
      val offsets = sizes.scanLeft(0)(_ + _)
      val allDefers = items.collect { case (_, p: Pending) => p.defers }.flatten
      val allLineages = items.collect { case (_, p: Pending) => p.lineages }.flatten

      def resume(values: Vector[(DeferOutcome, Long)]): F[Resolve] =
        items.zipWithIndex.toList
          .traverse {
            case ((a, r: Result), _) => asyncF.pure(a -> (r: Resolve))
            case ((a, p: Pending), i) =>
              val slice = values.slice(offsets(i), offsets(i) + p.defers.size)
              p.resume(slice).map(a -> _)
          }
          .map(next => sequenceResolve(next.toVector)(combine))

      Pending(allDefers, allLineages, resume)
    }

  private def sequenceResolveUnlabeled(items: Vector[Resolve])(
      combine: Vector[Result] => Result): Resolve =
    sequenceResolve(items.map(() -> _))(labeled => combine(labeled.map(_._2)))

  /** Drives a [[Resolve]] to completion: as long as it is [[Pending]], dispatch the current wave
    * and resume; repeat until a [[Result]] is produced. `ctx` is the context passed to the
    * [[DeferredResolver]] for every dispatch in this cascade - matching the original resolver's
    * behaviour of always using a single, unchanging context for a given cascade.
    */
  private def runToCompletion(ctx: Ctx, resolve: Resolve): F[Result] = resolve match {
    case r: Result => asyncF.pure(r)
    case p: Pending =>
      dispatchAll(ctx, p.defers, p.lineages).flatMap(p.resume).flatMap(runToCompletion(ctx, _))
  }

  @tailrec
  private def findActualDeferred(deferred: Deferred[_]): Deferred[_] = deferred match {
    case MappingDeferred(d, _) => findActualDeferred(d)
    case d => d
  }

  private def mapAllDeferredF(deferred: Deferred[_], value: F[Any]): F[(Any, Vector[Throwable])] =
    deferred match {
      case MappingDeferred(d, fn) =>
        mapAllDeferredF(d, value).map { case (v, errors) =>
          val (mappedV, newErrors) = fn.asInstanceOf[Any => (Any, Seq[Throwable])](v)
          mappedV -> (errors ++ newErrors)
        }
      case _ => value.map(_ -> Vector.empty)
    }

  /** Dispatches every defer in `defers`, keeping pre-existing lineages (see [[Pending]]) apart:
    * defers are first partitioned by their current lineage, and only *within* each of those
    * partitions is [[DeferredResolver#groupDeferred]] consulted to decide how many dispatch calls
    * are needed and whether that lineage itself now splits further. Every resulting dispatch group
    * is assigned a fresh lineage id, inherited by whatever each of its defers resolves to next.
    */
  private def dispatchAll(
      ctx: Ctx,
      defers: Vector[Defer],
      lineages: Vector[Long]): F[Vector[(DeferOutcome, Long)]] = {
    val byLineage: Map[Long, Vector[Defer]] =
      defers.zip(lineages).groupBy(_._2).map { case (lineage, pairs) => lineage -> pairs.map(_._1) }

    byLineage.toList
      .traverse { case (_, lineageDefers) =>
        deferredResolver
          .groupDeferred(lineageDefers)
          .toList
          .traverse { subGroup =>
            val newLineage = nextLineageId()
            dispatchGroup(ctx, subGroup).map(_.map { case (id, outcome) =>
              id -> (outcome, newLineage)
            })
          }
      }
      .map { nested =>
        val byId: Map[Long, (DeferOutcome, Long)] = nested.flatten.flatten.toMap
        defers.map(d => byId(d.id))
      }
  }

  private def dispatchGroup(ctx: Ctx, group: Vector[Defer]): F[Vector[(Long, DeferOutcome)]] =
    if (group.isEmpty)
      asyncF.pure(Vector.empty)
    else
      asyncF.executionContext
        .flatMap { implicit ec =>
          asyncF.attempt(asyncF.delay(deferredResolver
            .resolve(group.map(d => findActualDeferred(d.deferred)), ctx, deferredResolverState)))
        }
        .flatMap {
          case Left(error) =>
            asyncF.pure(group.map(d => d.id -> (Left(error): DeferOutcome)))
          case Right(resolved) if resolved.size != group.size =>
            val error = new IllegalStateException(
              s"Deferred resolver returned ${resolved.size} elements, but it got ${group.size} deferred values. This violates the contract. You can find more information in the documentation: https://sangria-graphql.github.io/learn/#deferred-values-and-resolver")
            asyncF.pure(group.map(d => d.id -> (Left(error): DeferOutcome)))
          case Right(resolved) =>
            group
              .zip(resolved)
              .toList
              .traverse { case (d, future) =>
                asyncF
                  .attempt(mapAllDeferredF(d.deferred, liftFuture(future)))
                  .map(outcome => d.id -> outcome)
              }
              .map(_.toVector)
        }

  // ---------------------------------------------------------------------------------------------
  // Parallel ("par") resolution - used for queries and for resolving the sub-structure of any
  // resolved value (lists, nested object types, ...)
  // ---------------------------------------------------------------------------------------------

  private def resolveActionsParF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      actions: Actions,
      userCtx: Ctx,
      fieldsNamesOrdered: Vector[String]): F[Resolve] =
    actions.actions match {
      case None => asyncF.pure(Result(actions.errorRegistry, None))
      case Some(results) =>
        results.toList
          .traverse { a =>
            val fieldLabel = a.fields.head
            a.result match {
              case None => asyncF.pure(fieldLabel -> (Result(ErrorRegistry.empty, None): Resolve))
              case Some(r) =>
                resolveLeafActionF(path, tpe, userCtx, a.fields, r.field, r.updateCtx)(r.action)
                  .map(fieldLabel -> _)
            }
          }
          .map { labeled =>
            sequenceResolve(labeled.toVector) { finished =>
              finished
                .foldLeft(
                  Result(
                    actions.errorRegistry,
                    Some(marshaller.emptyMapNode(fieldsNamesOrdered)))) {
                  case (acc, (astField, other)) =>
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
          }
    }

  private def resolveLeafActionF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(action: LeafAction[Ctx, Any]): F[Resolve] =
    action match {
      case v: Value[Ctx, Any] =>
        resolveValueActionF(path, tpe, userCtx, astFields, field, updateCtx)(v)
      case t: TryValue[Ctx, Any] =>
        resolveTryValueF(path, tpe, userCtx, astFields, field, updateCtx)(t)
      case p: PartialValue[Ctx, Any] =>
        resolvePartialValueF(path, tpe, userCtx, astFields, field, updateCtx)(p)
      case a: AsyncValue[Ctx, Any, F] =>
        resolveFutureLikeF(path, tpe, userCtx, astFields, field, updateCtx)(a.value)
      case f: FutureValue[Ctx, Any] =>
        resolveFutureLikeF(path, tpe, userCtx, astFields, field, updateCtx)(liftFuture(f.value))
      case p: PartialFutureValue[Ctx, Any] =>
        resolvePartialFutureValueF(path, tpe, userCtx, astFields, field, updateCtx)(p)
      case d: DeferredValue[Ctx, Any] =>
        resolveDeferredValueF(path, tpe, userCtx, astFields, field, updateCtx)(d)
      case d: DeferredFutureValue[Ctx, Any] =>
        resolveDeferredFutureValueF(path, tpe, userCtx, astFields, field, updateCtx)(d)
      case s: SequenceLeafAction[Ctx, Any] @unchecked =>
        resolveSequenceLeafActionF(path, tpe, userCtx, astFields, field, updateCtx)(s)
      case _: MappedSequenceLeafAction[_, _, _] =>
        asyncF.pure(
          illegalActionResolve(path, tpe, astFields, updateCtx)(
            "MappedSequenceLeafAction is not supposed to appear here"))
      case _: SubscriptionValue[_, _, _] =>
        asyncF.pure(
          illegalActionResolve(path, tpe, astFields, updateCtx)(
            "Subscription values are not supported for normal operations"))
      case other =>
        asyncF.pure(
          illegalActionResolve(path, tpe, astFields, updateCtx)(
            s"Action of type '${other.getClass.toString}' is not supported by this resolver"))
    }

  private def illegalActionResolve(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      astFields: Vector[ast.Field],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(msg: String): Result = {
    val fieldsPath = path.add(astFields.head, tpe)
    Result(
      ErrorRegistry(
        fieldsPath,
        resolveError(updateCtx, new IllegalStateException(msg)),
        astFields.head.location),
      None)
  }

  private def resolveValueActionF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(v: Value[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)
    try
      resolveValueTypeF(
        fieldsPath,
        astFields,
        field.fieldType,
        field,
        resolveVal(updateCtx, v.value),
        resolveUc(updateCtx, v.value, userCtx))
    catch {
      case NonFatal(e) =>
        asyncF.pure(
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None))
    }
  }

  private def resolveTryValueF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(t: TryValue[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)
    t.value match {
      case Success(success) =>
        try
          resolveValueTypeF(
            fieldsPath,
            astFields,
            field.fieldType,
            field,
            resolveVal(updateCtx, success),
            resolveUc(updateCtx, success, userCtx))
        catch {
          case NonFatal(e) =>
            asyncF.pure(
              Result(
                ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
                None))
        }
      case Failure(e) =>
        asyncF.pure(
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None))
    }
  }

  private def resolvePartialValueF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(p: PartialValue[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)
    p.errors.foreach(resolveError(updateCtx, _))

    try
      resolveValueTypeF(
        fieldsPath,
        astFields,
        field.fieldType,
        field,
        resolveVal(updateCtx, p.value),
        resolveUc(updateCtx, p.value, userCtx))
        .map(_.appendErrors(fieldsPath, p.errors, astFields.head.location))
    catch {
      case NonFatal(e) =>
        asyncF.pure(
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location)
              .append(fieldsPath, p.errors, astFields.head.location),
            None))
    }
  }

  /** Shared by plain [[FutureValue]] (lifted into `F`) and [[AsyncValue]] (already `F`): both
    * respect [[DeferredResolver#includeDeferredFromField]] the same way the original resolver does,
    * deciding whether any deferred values discovered while resolving this value should be batched
    * together with the rest of the tree, or resolved immediately on their own.
    */
  private def resolveFutureLikeF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(lifted: F[Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)

    val resolvedF: F[Resolve] =
      asyncF.attempt(lifted).flatMap {
        case Left(e) =>
          asyncF.pure(
            Result(
              ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
              None))
        case Right(v) =>
          resolveValueTypeF(
            fieldsPath,
            astFields,
            field.fieldType,
            field,
            resolveVal(updateCtx, v),
            resolveUc(updateCtx, v, userCtx))
      }

    deferredResolver.includeDeferredFromField match {
      case Some(fn) =>
        val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)

        if (fn(field, astFields, args, complexity)) resolvedF
        else resolvedF.flatMap(r => runToCompletion(userContext, r).map(x => x: Resolve))
      case None => resolvedF
    }
  }

  private def resolvePartialFutureValueF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      p: PartialFutureValue[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)

    asyncF.attempt(liftFuture(p.value)).flatMap {
      case Left(e) =>
        asyncF.pure(
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None))
      case Right(PartialValue(v, es)) =>
        es.foreach(resolveError(updateCtx, _))
        resolveValueTypeF(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx))
          .map(_.appendErrors(fieldsPath, es, astFields.head.location))
    }
  }

  private def resumeSingle(
      fieldsPath: ExecutionPath,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]],
      userCtx: Ctx)(outcomes: Vector[(DeferOutcome, Long)]): F[Resolve] =
    outcomes match {
      case Vector((Left(e), _)) =>
        asyncF.pure(
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None))
      case Vector((Right((v, es)), newLineage)) =>
        es.foreach(resolveError(updateCtx, _))
        resolveValueTypeF(
          fieldsPath,
          astFields,
          field.fieldType,
          field,
          resolveVal(updateCtx, v),
          resolveUc(updateCtx, v, userCtx))
          .map(_.appendErrors(fieldsPath, es, astFields.head.location))
          .map(retag(_, newLineage))
      case other =>
        asyncF.raiseError(
          new IllegalStateException(s"Expected exactly one deferred outcome, got ${other.size}"))
    }

  private def resolveDeferredValueF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(d: DeferredValue[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)
    val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
    val defer = Defer(nextDeferId(), d.value, complexity, field, astFields, args)

    asyncF.pure(
      Pending(
        Vector(defer),
        Vector(NoLineage),
        resumeSingle(fieldsPath, astFields, field, updateCtx, userCtx)))
  }

  private def resolveDeferredFutureValueF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      d: DeferredFutureValue[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)

    asyncF.attempt(liftFuture(d.value)).map {
      case Left(e) =>
        Result(ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location), None)
      case Right(deferredValue) =>
        val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
        val defer = Defer(nextDeferId(), deferredValue, complexity, field, astFields, args)

        Pending(
          Vector(defer),
          Vector(NoLineage),
          resumeSingle(fieldsPath, astFields, field, updateCtx, userCtx))
    }
  }

  // ---------------------------------------------------------------------------------------------
  // `SequenceLeafAction` (i.e. `Action.sequence(...)`): shared between the "par" and "seq" paths
  // ---------------------------------------------------------------------------------------------

  /** Resolves a single item of a `SequenceLeafAction` to its raw (not yet marshalled) outcome.
    * `DeferredValue`/`DeferredFutureValue` items are resolved via their own dedicated dispatch (not
    * batched together with the rest of the tree) - this keeps the sequence-item machinery
    * self-contained, at the cost of one extra `DeferredResolver` call for such items.
    */
  private def resolveSeqItemF(
      fieldsPath: ExecutionPath,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _])(action: LeafAction[Any, Any]): F[(Option[Any], Vector[Throwable])] =
    action match {
      case Value(v) => asyncF.pure(Some(v) -> Vector.empty)
      case TryValue(Success(v)) => asyncF.pure(Some(v) -> Vector.empty)
      case TryValue(Failure(e)) => asyncF.pure(None -> Vector(e))
      case PartialValue(v, es) => asyncF.pure(Some(v) -> es)
      case FutureValue(future) =>
        asyncF.attempt(liftFuture(future)).map {
          case Right(v) => Some(v) -> Vector.empty
          case Left(e) => None -> Vector(e)
        }
      case PartialFutureValue(future) =>
        asyncF.attempt(liftFuture(future)).map {
          case Right(PartialValue(v, es)) => Some(v) -> es
          case Left(e) => None -> Vector(e)
        }
      case a: AsyncValue[Any, Any, F] =>
        asyncF.attempt(a.value).map {
          case Right(v) => Some(v) -> Vector.empty
          case Left(e) => None -> Vector(e)
        }
      case DeferredValue(deferred) =>
        val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
        val defer = Defer(nextDeferId(), deferred, complexity, field, astFields, args)
        dispatchAll(userContext, Vector(defer), Vector(NoLineage)).map(_.head).map {
          case (Left(e), _) => None -> Vector(e)
          case (Right((v, es)), _) => Some(v) -> es
        }
      case DeferredFutureValue(deferredValue) =>
        asyncF.attempt(liftFuture(deferredValue)).flatMap {
          case Left(e) => asyncF.pure(None -> Vector(e))
          case Right(d) =>
            val (args, complexity) = calcComplexity(fieldsPath, astFields.head, field, userContext)
            val defer = Defer(nextDeferId(), d, complexity, field, astFields, args)
            dispatchAll(userContext, Vector(defer), Vector(NoLineage)).map(_.head).map {
              case (Left(e2), _) => None -> Vector(e2)
              case (Right((v, es)), _) => Some(v) -> es
            }
        }
      case SequenceLeafAction(_) | _: MappedSequenceLeafAction[_, _, _] =>
        asyncF.pure(None -> Vector(new IllegalStateException(
          "Nested `SequenceLeafAction` is not yet supported inside of another `SequenceLeafAction`")))
      case _: SubscriptionValue[_, _, _] =>
        asyncF.pure(None -> Vector(
          new IllegalStateException("Subscription values are not supported for normal operations")))
      case other =>
        asyncF.pure(None -> Vector(
          new IllegalStateException(s"${other.getClass.toString} is not supposed to appear here")))
    }

  private def resolveSeqSequenceF(
      fieldsPath: ExecutionPath,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      actions: Seq[LeafAction[Any, Any]]): F[(Vector[Throwable], Option[Vector[Any]])] =
    actions.toVector.traverse(resolveSeqItemF(fieldsPath, astFields, field)).map { outcomes =>
      val errors = outcomes.flatMap(_._2)
      val successfulValues = outcomes.collect { case (Some(v), _) => v }

      if (successfulValues.size == outcomes.size) errors -> Some(successfulValues)
      else errors -> None
    }

  private def resolveSequenceLeafActionF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      userCtx: Ctx,
      astFields: Vector[ast.Field],
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
      action: SequenceLeafAction[Ctx, Any]): F[Resolve] = {
    val fieldsPath = path.add(astFields.head, tpe)

    resolveSeqSequenceF(fieldsPath, astFields, field, action.value)
      .flatMap { case (errors, successfulValuesOpt) =>
        errors.foreach(resolveError(updateCtx, _))

        (successfulValuesOpt match {
          case Some(successfulValues) =>
            resolveValueTypeF(
              fieldsPath,
              astFields,
              field.fieldType,
              field,
              resolveVal(updateCtx, successfulValues),
              resolveUc(updateCtx, successfulValues, userCtx))
              .map(_.appendErrors(fieldsPath, errors, astFields.head.location))
          case None =>
            asyncF.pure(
              Result(ErrorRegistry.empty.append(fieldsPath, errors, astFields.head.location), None))
        }): F[Resolve]
      }
      .handleErrorWith(e =>
        asyncF.pure(
          Result(
            ErrorRegistry(fieldsPath, resolveError(updateCtx, e), astFields.head.location),
            None)))
  }

  // ---------------------------------------------------------------------------------------------
  // Sequential ("seq") resolution - used for mutations: fields are resolved one at a time, fully
  // (including any deferred-value dispatch cascade of their own), in declaration order.
  // ---------------------------------------------------------------------------------------------

  private def resolveSeqF(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields): F[(Result, Ctx)] = {
    def step(acc: F[(Result, Ctx)], elem: CollectedField): F[(Result, Ctx)] =
      acc.flatMap { resAndCtx =>
        (resAndCtx, elem) match {
          case (a @ (Result(_, None, _), _), _) => asyncF.pure(a)
          case (a, CollectedField(_, origField, _))
              if tpe.getField(schema, origField.name).isEmpty =>
            asyncF.pure(a)
          case (
                (Result(errors, Some(accVal), _), uc),
                CollectedField(_, origField, Failure(error))) =>
            asyncF.pure(
              Result(
                errors.add(path.add(origField, tpe), error),
                if (isOptional(tpe, origField.name))
                  Some(
                    marshaller.addMapNodeElem(
                      accVal.asInstanceOf[marshaller.MapBuilder],
                      origField.outputName,
                      marshaller.nullNode,
                      optional = true))
                else None
              ) -> uc)
          case (
                (accRes @ Result(_, Some(accVal), _), uc),
                CollectedField(_, origField, Success(fieldsX))) =>
            resolveSingleFieldSeqF(
              path,
              uc,
              tpe,
              value,
              accRes.errors,
              origField,
              fieldsX,
              accRes,
              accVal)
        }
      }

    fields.fields
      .foldLeft(
        asyncF.pure(
          (
            Result(ErrorRegistry.empty, Some(marshaller.emptyMapNode(fields.namesOrdered))),
            userContext)
        ): F[(Result, Ctx)])(step)
      .map { case (res, ctx) => res.buildValue -> ctx }
  }

  private def resolveSingleFieldSeqF(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      value: Any,
      errors: ErrorRegistry,
      origField: ast.Field,
      fields: Vector[ast.Field],
      accRes: Result,
      acc: Any // from `accRes`
  ): F[(Result, Ctx)] =
    resolveField(uc, tpe, path.add(origField, tpe), value, errors, fields) match {
      case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
        asyncF.pure(
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
        asyncF.pure(Result(updatedErrors, None) -> uc)
      case resolution: StandardFieldResolution =>
        resolveStandardFieldResolutionSeqF(path, uc, tpe, origField, fields, accRes, resolution)
      case _: StreamFieldResolution[_, _] =>
        asyncF.raiseError(
          new IllegalStateException("StreamFieldResolution is not supposed to happen here"))
    }

  private def resolveStandardFieldResolutionSeqF(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      origField: ast.Field,
      fields: Vector[ast.Field],
      accRes: Result,
      resolution: StandardFieldResolution): F[(Result, Ctx)] = {
    val StandardFieldResolution(updatedErrors, result, newUc) = resolution
    val sfield = tpe.getField(schema, origField.name).head
    val fieldPath = path.add(fields.head, tpe)

    def resolveUcFn(v: Any): Ctx = newUc.fold(uc)(_.ctxFn(v))

    def resolveErrorFn(e: Throwable): Throwable = {
      try newUc.foreach(_.onError(e))
      catch {
        case NonFatal(ee) => ee.printStackTrace()
      }
      e
    }

    def resolveValFn(v: Any): Any = newUc match {
      case Some(MappedCtxUpdate(_, mapFn, _)) => mapFn(v)
      case None => v
    }

    val resolveF: F[Result] =
      try
        resolveStandardFieldResolutionSeqInnerF(
          path,
          uc,
          tpe,
          origField,
          fields,
          result,
          sfield,
          fieldPath,
          resolveUcFn,
          resolveErrorFn,
          resolveValFn
        )
      catch {
        case NonFatal(e) =>
          asyncF.pure(
            Result(
              ErrorRegistry(fieldPath, resolveErrorFn(e), fields.head.location),
              None,
              Some(uc)))
      }

    resolveF.map { r =>
      accRes.addToMap(
        r,
        fields.head.outputName,
        isOptional(tpe, fields.head.name),
        fieldPath,
        fields.head.location,
        updatedErrors) -> r.userContext.getOrElse(uc)
    }
  }

  private def resolveStandardFieldResolutionSeqInnerF(
      path: ExecutionPath,
      uc: Ctx,
      tpe: ObjectType[Ctx, _],
      origField: ast.Field,
      fields: Vector[ast.Field],
      result: LeafAction[Ctx, Any],
      sfield: Field[Ctx, _],
      fieldPath: ExecutionPath,
      resolveUcFn: Any => Ctx,
      resolveErrorFn: Throwable => Throwable,
      resolveValFn: Any => Any): F[Result] = {

    def resolveAndTag(rawValue: Any): F[Result] = {
      val updatedUc = resolveUcFn(rawValue)
      resolveValueTypeF(
        fieldPath,
        fields,
        sfield.fieldType,
        sfield,
        resolveValFn(rawValue),
        updatedUc)
        .flatMap(r => runToCompletion(uc, tagUserContext(r, updatedUc)))
    }

    result match {
      case Value(v) => resolveAndTag(v)

      case SequenceLeafAction(actions) =>
        resolveSeqSequenceF(fieldPath, fields, sfield, actions)
          .flatMap { case (errors, successfulValuesOpt) =>
            errors.foreach(resolveErrorFn)
            successfulValuesOpt match {
              case Some(successfulValues) =>
                val updatedUc = resolveUcFn(successfulValues)
                resolveValueTypeF(
                  fieldPath,
                  fields,
                  sfield.fieldType,
                  sfield,
                  resolveValFn(successfulValues),
                  updatedUc)
                  .map(_.appendErrors(fieldPath, errors, fields.head.location))
                  .flatMap(r => runToCompletion(uc, tagUserContext(r, updatedUc)))
              case None =>
                asyncF.pure(
                  Result(
                    ErrorRegistry.empty.append(fieldPath, errors, fields.head.location),
                    None,
                    Some(uc)))
            }
          }
          .handleErrorWith(e =>
            asyncF.pure(
              Result(
                ErrorRegistry(fieldPath, resolveErrorFn(e), fields.head.location),
                None,
                Some(uc))))

      case PartialValue(v, es) =>
        es.foreach(resolveErrorFn)
        resolveAndTag(v).map(_.appendErrors(fieldPath, es, fields.head.location))

      case TryValue(Success(v)) => resolveAndTag(v)
      case TryValue(Failure(e)) =>
        asyncF.pure(
          Result(ErrorRegistry(fieldPath, resolveErrorFn(e), fields.head.location), None, Some(uc)))

      case DeferredValue(d) =>
        val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
        val defer = Defer(nextDeferId(), d, complexity, sfield, fields, args)

        def resume(outcomes: Vector[(DeferOutcome, Long)]): F[Resolve] = outcomes match {
          case Vector((Left(e), _)) =>
            asyncF.pure(
              Result(ErrorRegistry(fieldPath, resolveErrorFn(e), fields.head.location), None))
          case Vector((Right((v, es)), newLineage)) =>
            val updatedUc = resolveUcFn(v)
            es.foreach(resolveErrorFn)
            resolveValueTypeF(
              fieldPath,
              fields,
              sfield.fieldType,
              sfield,
              resolveValFn(v),
              updatedUc)
              .map(_.appendErrors(fieldPath, es, fields.head.location))
              .map(tagUserContext(_, updatedUc))
              .map(retag(_, newLineage))
        }

        runToCompletion(uc, Pending(Vector(defer), Vector(NoLineage), resume))

      case FutureValue(f) =>
        asyncF.attempt(liftFuture(f)).flatMap {
          case Left(e) =>
            asyncF.pure(
              Result(
                ErrorRegistry(path.add(origField, tpe), resolveErrorFn(e), fields.head.location),
                None,
                Some(uc)))
          case Right(v) => resolveAndTag(v)
        }

      case a: AsyncValue[Ctx, Any, F] =>
        asyncF.attempt(a.value).flatMap {
          case Left(e) =>
            asyncF.pure(
              Result(
                ErrorRegistry(path.add(origField, tpe), resolveErrorFn(e), fields.head.location),
                None,
                Some(uc)))
          case Right(v) => resolveAndTag(v)
        }

      case PartialFutureValue(f) =>
        asyncF.attempt(liftFuture(f)).flatMap {
          case Left(e) =>
            asyncF.pure(
              Result(
                ErrorRegistry(path.add(origField, tpe), resolveErrorFn(e), fields.head.location),
                None,
                Some(uc)))
          case Right(PartialValue(v, es)) =>
            es.foreach(resolveErrorFn)
            resolveAndTag(v).map(_.appendErrors(fieldPath, es, fields.head.location))
        }

      case DeferredFutureValue(df) =>
        asyncF.attempt(liftFuture(df)).flatMap {
          case Left(e) =>
            asyncF.pure(
              Result(
                ErrorRegistry(fieldPath, resolveErrorFn(e), fields.head.location),
                None,
                Some(uc)))
          case Right(deferredValue) =>
            val (args, complexity) = calcComplexity(fieldPath, origField, sfield, userContext)
            val defer = Defer(nextDeferId(), deferredValue, complexity, sfield, fields, args)

            def resume(outcomes: Vector[(DeferOutcome, Long)]): F[Resolve] = outcomes match {
              case Vector((Left(e2), _)) =>
                asyncF.pure(
                  Result(ErrorRegistry(fieldPath, resolveErrorFn(e2), fields.head.location), None))
              case Vector((Right((v, es)), newLineage)) =>
                val updatedUc = resolveUcFn(v)
                es.foreach(resolveErrorFn)
                resolveValueTypeF(
                  fieldPath,
                  fields,
                  sfield.fieldType,
                  sfield,
                  resolveValFn(v),
                  updatedUc)
                  .map(_.appendErrors(fieldPath, es, fields.head.location))
                  .map(tagUserContext(_, updatedUc))
                  .map(retag(_, newLineage))
            }

            runToCompletion(uc, Pending(Vector(defer), Vector(NoLineage), resume))
        }

      case _: SubscriptionValue[_, _, _] =>
        asyncF.raiseError(
          new IllegalStateException("Subscription values are not supported for normal operations"))

      case e =>
        asyncF.raiseError(
          new IllegalStateException(s"${e.getClass.toString} is not supposed to appear here"))
    }
  }

  // ---------------------------------------------------------------------------------------------
  // Type-directed marshalling (lists, objects, scalars, enums, abstract types, ...)
  // ---------------------------------------------------------------------------------------------

  private def resolveValueTypeF(
      path: ExecutionPath,
      astFields: Vector[ast.Field],
      tpe: OutputType[_],
      field: Field[Ctx, _],
      value: Any,
      userCtx: Ctx,
      actualType: Option[InputType[_]] = None): F[Resolve] =
    tpe match {
      case OptionType(optTpe) =>
        val actualValue = value match {
          case v: Option[_] => v
          case v => Option(v)
        }

        actualValue match {
          case Some(someValue) =>
            resolveValueTypeF(path, astFields, optTpe, field, someValue, userCtx, None)
          case None => asyncF.pure(Result(ErrorRegistry.empty, None))
        }

      case ListType(listTpe) =>
        if (isUndefinedValue(value)) asyncF.pure(Result(ErrorRegistry.empty, None))
        else {
          val actualValue = value match {
            case seq: Iterable[_] => seq.iterator
            case other => Iterator.single(other)
          }
          val optional = isOptional(listTpe)

          actualValue.zipWithIndex.toVector
            .traverse { case (v, idx) =>
              resolveValueTypeF(path.withIndex(idx), astFields, listTpe, field, v, userCtx, None)
            }
            .map(items =>
              sequenceResolveUnlabeled(items)(
                resolveSimpleListValue(_, path, optional, astFields.head.location)))
        }

      case scalar: ScalarType[Any @unchecked] =>
        asyncF.pure(
          try
            Result(
              ErrorRegistry.empty,
              if (isUndefinedValue(value))
                None
              else {
                val coerced = scalar.coerceOutput(value, marshaller.capabilities)

                if (isUndefinedValue(coerced))
                  None
                else {
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
          })

      case scalar: ScalarAlias[Any @unchecked, Any @unchecked] =>
        resolveValueTypeF(
          path,
          astFields,
          scalar.aliasFor,
          field,
          scalar.toScalar(value),
          userCtx,
          Some(scalar))

      case enumT: EnumType[Any @unchecked] =>
        asyncF.pure(
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
          })

      case obj: ObjectType[Ctx @unchecked, _] =>
        if (isUndefinedValue(value)) asyncF.pure(Result(ErrorRegistry.empty, None))
        else
          fieldCollector.collectFields(path, obj, astFields) match {
            case Success(fields) =>
              val actions =
                collectActionsPar(path, obj, value, fields, ErrorRegistry.empty, userCtx)
              resolveActionsParF(path, obj, actions, userCtx, fields.namesOrdered)
            case Failure(error) => asyncF.pure(Result(ErrorRegistry(path, error), None))
          }

      case abst: AbstractType =>
        if (isUndefinedValue(value)) asyncF.pure(Result(ErrorRegistry.empty, None))
        else {
          val actualValue =
            abst match {
              case abst: MappedAbstractType[Any @unchecked] => abst.contraMap(value)
              case _ => value
            }

          abst.typeOf(actualValue, schema) match {
            case Some(obj) =>
              resolveValueTypeF(path, astFields, obj, field, actualValue, userCtx, None)
            case None =>
              asyncF.pure(
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
                ))
          }
        }
    }

}
