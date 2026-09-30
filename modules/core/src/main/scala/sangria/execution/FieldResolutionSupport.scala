package sangria.execution

import sangria.ast
import sangria.ast.{AstLocation, SourceMapper}
import sangria.marshalling.ResultMarshaller
import sangria.schema._

import scala.annotation.tailrec
import scala.collection.immutable.{ListMap, VectorBuilder}
import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** Field resolution logic shared between [[FutureResolver]] and any other [[Resolver]]
  * implementation (e.g. the cats-effect module's `AsyncResolver`) that resolves the same
  * `sangria.schema.Action`/`LeafAction` tree but drives the resulting asynchronous values with a
  * different primitive (`Future`, a `cats.effect.Async` instance, ...).
  *
  * Everything here is pure with respect to that primitive: it calls the user's `resolve` function,
  * runs before/after/error middleware, tracks deprecations, computes complexity, and collects
  * `@skip`/`@include`-aware projections - none of which cares how the resulting `LeafAction` (a
  * `Value`, `FutureValue`, `DeferredValue`, ...) is ultimately turned into a value. Turning that
  * `LeafAction` into a value (and, in particular, batching [[sangria.execution.deferred.Deferred]]
  * values) is exactly the part each concrete [[Resolver]] still implements on its own.
  */
private[execution] trait FieldResolutionSupport[Ctx] {
  protected val marshaller: ResultMarshaller
  protected def middlewareCtx: MiddlewareQueryContext[Ctx, _, _]
  protected def schema: Schema[Ctx, _]
  protected def valueCollector: ValueCollector[Ctx, _]
  protected def variables: Map[String, VariableValue]
  protected def fieldCollector: FieldCollector[Ctx, _]
  protected def exceptionHandler: ExceptionHandler
  protected def sourceMapper: Option[SourceMapper]
  protected def deprecationTracker: Option[DeprecationTracker]
  protected def beforeFieldMiddlewares: List[(Any, MiddlewareBeforeField[Ctx])]
  protected def maxQueryDepth: Option[Int]
  protected def deferredResolverState: Any
  protected def queryAst: ast.Document
  protected val resultResolver: ResultResolver

  import Resolver._
  import resultResolver._

  protected case class Actions(errorRegistry: ErrorRegistry, actions: Option[Vector[ActionsItem]])
  protected case class ActionsItem(fields: Vector[ast.Field], result: Option[ActionsItemResult])
  protected case class ActionsItemResult(
      field: Field[Ctx, _],
      updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]],
      action: LeafAction[Ctx, _])

  protected sealed trait FieldResolution
  protected case class ErrorFieldResolution(errors: ErrorRegistry) extends FieldResolution
  protected case class StandardFieldResolution(
      errors: ErrorRegistry,
      action: LeafAction[Ctx, Any],
      ctxUpdate: Option[MappedCtxUpdate[Ctx, Any, Any]])
      extends FieldResolution
  protected case class StreamFieldResolution[Val, S[_]](
      errors: ErrorRegistry,
      value: SubscriptionValue[Ctx, Val, S],
      standardResolution: Any => StandardFieldResolution)
      extends FieldResolution

  protected def collectActionsPar(
      path: ExecutionPath,
      tpe: ObjectType[Ctx, _],
      value: Any,
      fields: CollectedFields,
      errorReg: ErrorRegistry,
      userCtx: Ctx): Actions = {
    var errorRegistry = errorReg
    val actions: VectorBuilder[ActionsItem] = new VectorBuilder
    val it = fields.fields.iterator
    while (it.hasNext) {
      val f = it.next()
      val origField = f.field
      val origFieldName = origField.name
      val fieldDefs = tpe.getField(schema, origFieldName)
      if (fieldDefs.nonEmpty) {
        f.allFields match {
          case Failure(error) =>
            errorRegistry = errorRegistry.add(path.add(origField, tpe), error)
            if (isOptional(fieldDefs.head.fieldType))
              actions += ActionsItem(Vector(origField), None)
            else return Actions(errorRegistry, None) // short-circuit

          case Success(allFields) =>
            val resolution =
              resolveField(userCtx, tpe, path.add(origField, tpe), value, errorRegistry, allFields)
            resolution match {
              case StandardFieldResolution(updatedErrors, result, updateCtx) =>
                errorRegistry = updatedErrors
                actions += ActionsItem(
                  allFields,
                  Some(ActionsItemResult(fieldDefs.head, updateCtx, result)))
              case ErrorFieldResolution(updatedErrors) if isOptional(tpe, origField.name) =>
                errorRegistry = updatedErrors
                actions += ActionsItem(Vector(origField), None)
              case ErrorFieldResolution(updatedErrors) =>
                errorRegistry = updatedErrors
                return Actions(errorRegistry, None) // short-circuit
              case _: StreamFieldResolution[_, _] =>
                throw new IllegalStateException(
                  "IllegalStateException is not supposed to happen here")
            }
        }
      }
    }
    Actions(errorRegistry, Some(actions.result()))
  }

  protected def calcComplexity(
      path: ExecutionPath,
      astField: ast.Field,
      field: Field[Ctx, _],
      uc: Ctx): (Args, Double) = {
    val args = valueCollector.getFieldArgumentValues(
      path,
      Some(astField),
      field.arguments,
      astField.arguments,
      variables)

    args match {
      case Success(a) => a -> field.complexity.fold(DefaultComplexity)(_(uc, a, DefaultComplexity))
      case _ => Args.empty -> DefaultComplexity
    }
  }

  protected def resolveUc(
      newUc: Option[MappedCtxUpdate[Ctx, Any, Any]],
      v: Any,
      userCtx: Ctx): Ctx =
    newUc.fold(userCtx)(_.ctxFn(v))

  protected def resolveVal(newUc: Option[MappedCtxUpdate[Ctx, Any, Any]], v: Any): Any =
    newUc match {
      case Some(MappedCtxUpdate(_, mapFn, _)) => mapFn(v)
      case None => v
    }

  protected def resolveError(
      newUc: Option[MappedCtxUpdate[Ctx, Any, Any]],
      e: Throwable): Throwable = {
    try newUc.foreach(_.onError(e))
    catch {
      case NonFatal(ee) => ee.printStackTrace()
    }
    e
  }

  private def trackDeprecation(
      deprecationTracker: DeprecationTracker,
      ctx: Context[Ctx, _]): Unit = {
    val fieldArgs = ctx.args
    val visitedDirectives = mutable.Set[String]()

    def getArgValue(name: String, args: Args): Option[_] =
      if (args.argDefinedInQuery(name)) {
        if (args.optionalArgs.contains(name)) {
          args.argOpt(name)
        } else {
          Some(args.arg(name))
        }
      } else {
        None
      }

    def deprecatedArgsUsed(argDefs: List[Argument[_]], argValues: Args): List[Argument[_]] =
      argDefs.filter { argDef =>
        val argValue = getArgValue(argDef.name, argValues)
        argDef.deprecationReason.isDefined && argValue.isDefined
      }

    def trackDeprecatedDirectiveArgs(astDirective: ast.Directive): Unit = {
      // prevent infinite loop from directiveA -> arg -> directiveA -> arg ...
      if (visitedDirectives.contains(astDirective.name)) {
        return
      }
      visitedDirectives.add(astDirective.name)

      ctx.schema.directives.find(_.name == astDirective.name) match {
        case Some(directive) =>
          val directiveArgs = valueCollector
            .getArgumentValues(
              Some(astDirective),
              directive.arguments,
              astDirective.arguments,
              variables)

          directiveArgs match {
            case Success(directiveArgs) =>
              deprecatedArgsUsed(directive.arguments, directiveArgs).foreach { arg =>
                deprecationTracker.deprecatedDirectiveArgUsed(directive, arg, ctx)
              }
            case _ => // if we fail to get args, the query should fail elsewhere
          }

          // nested argument directives
          directive.arguments.foreach { nestedArg =>
            nestedArg.astDirectives.foreach(trackDeprecatedDirectiveArgs)
          }
        case _ => // do nothing
      }
    }

    def trackDeprecatedInputObjectFields(inputType: InputType[_], ioArg: Any): Unit =
      inputType match {
        case ioDef: InputObjectType[_] =>
          ioDef.fields.foreach { field =>
            // field deprecation
            val fieldVal: Option[_] = (ioArg match {
              case lm: ListMap[String, _] @unchecked => lm.get(field.name)
              case _ => None
            }) match {
              case Some(Some(nested)) => Some(nested)
              case value => value
            }

            if (field.deprecationReason.isDefined && fieldVal.isDefined) {
              deprecationTracker.deprecatedInputObjectFieldUsed(ioDef, field, ctx)
            }

            // for nested input objects
            if (fieldVal.isDefined) trackDeprecatedInputObjectFields(field.fieldType, fieldVal.get)

            // field directive args deprecation
            field.astDirectives.foreach(trackDeprecatedDirectiveArgs)
          }
        case OptionInputType(ofType) =>
          ioArg match {
            case Some(ioArg) => trackDeprecatedInputObjectFields(ofType, ioArg)
            case _ => trackDeprecatedInputObjectFields(ofType, ioArg)
          }
        case ListInputType(ofType) =>
          ioArg match {
            case seq: Seq[_] => seq.foreach(trackDeprecatedInputObjectFields(ofType, _))
            case _ => // do nothing
          }
        case _ => // do nothing
      }

    val field = ctx.field
    val astField = ctx.astFields.head

    // field deprecation
    val allFields =
      ctx.parentType.getField(ctx.schema, astField.name).asInstanceOf[Vector[Field[Ctx, Any]]]
    if (allFields.exists(_.deprecationReason.isDefined))
      deprecationTracker.deprecatedFieldUsed(ctx)

    // directive argument deprecation
    field.astDirectives.foreach(trackDeprecatedDirectiveArgs)

    // field argument deprecation
    deprecatedArgsUsed(field.arguments, fieldArgs).foreach { arg =>
      deprecationTracker.deprecatedFieldArgUsed(arg, ctx)
    }

    field.arguments.foreach { argDef =>
      // argument directives args deprecation
      argDef.astDirectives.foreach(trackDeprecatedDirectiveArgs)

      // input object field deprecation
      getArgValue(argDef.name, fieldArgs) match {
        case Some(ioArg) => trackDeprecatedInputObjectFields(argDef.argumentType, ioArg)
        case _ => // do nothing
      }
    }
  }

  protected def resolveField(
      userCtx: Ctx,
      tpe: ObjectType[Ctx, _],
      path: ExecutionPath,
      value: Any,
      errors: ErrorRegistry,
      astFields: Vector[ast.Field]): FieldResolution = {
    val astField = astFields.head
    val allFields = tpe.getField(schema, astField.name).asInstanceOf[Vector[Field[Ctx, Any]]]
    val field = allFields.head

    maxQueryDepth match {
      case Some(max) if path.size > max =>
        ErrorFieldResolution(errors.add(path, MaxQueryDepthReachedError(max), astField.location))
      case _ =>
        valueCollector.getFieldArgumentValues(
          path,
          Some(astField),
          field.arguments,
          astField.arguments,
          variables) match {
          case Success(args) =>
            val ctx = Context[Ctx, Any](
              value = value,
              ctx = userCtx,
              args = args,
              schema = schema.asInstanceOf[Schema[Ctx, Any]],
              field = field,
              parentType = tpe.asInstanceOf[ObjectType[Ctx, Any]],
              marshaller = marshaller,
              query = queryAst,
              sourceMapper = sourceMapper,
              deprecationTracker = deprecationTracker,
              astFields = astFields,
              path = path,
              deferredResolverState = deferredResolverState
            )

            deprecationTracker.foreach(trackDeprecation(_, ctx))

            try {
              var beforeAction: Option[Action[Ctx, _]] = None
              val beforeAttachmentsBuilder: VectorBuilder[MiddlewareAttachment] =
                new VectorBuilder()

              val mAfterBuilder
                  : VectorBuilder[(BeforeFieldResult[Ctx, _], Any, MiddlewareAfterField[Ctx])] =
                new VectorBuilder()

              val mErrorBuilder
                  : VectorBuilder[(BeforeFieldResult[Ctx, _], Any, MiddlewareErrorField[Ctx])] =
                new VectorBuilder()

              val it = beforeFieldMiddlewares.iterator
              while (it.hasNext) {
                val (mv, m) = it.next()
                val beforeFieldResult = m
                  .beforeField(mv.asInstanceOf[m.QueryVal], middlewareCtx, ctx)

                if (beforeFieldResult.actionOverride.nonEmpty) {
                  beforeAction = beforeFieldResult.actionOverride
                }
                beforeFieldResult.attachment match {
                  case Some(att) => beforeAttachmentsBuilder += att
                  case None =>
                }

                if (m.isInstanceOf[MiddlewareAfterField[Ctx]]) {
                  mAfterBuilder += ((
                    beforeFieldResult,
                    mv,
                    m.asInstanceOf[MiddlewareAfterField[Ctx]]))
                }
                if (m.isInstanceOf[MiddlewareErrorField[Ctx]]) {
                  mErrorBuilder += ((
                    beforeFieldResult,
                    mv,
                    m.asInstanceOf[MiddlewareErrorField[Ctx]]))
                }
              }

              val beforeAttachments = beforeAttachmentsBuilder.result()
              val updatedCtx =
                if (beforeAttachments.nonEmpty) ctx.copy(middlewareAttachments = beforeAttachments)
                else ctx

              val mAfter: Vector[(BeforeFieldResult[Ctx, _], Any, MiddlewareAfterField[Ctx])] =
                mAfterBuilder.result().reverse
              val mError: Vector[(BeforeFieldResult[Ctx, _], Any, MiddlewareErrorField[Ctx])] =
                mErrorBuilder.result()

              def doAfterMiddleware[Val](v: Val): Val =
                mAfter.foldLeft(v) { case (acc, (beforeFieldResult, mv, m)) =>
                  m.afterField(
                    mv.asInstanceOf[m.QueryVal],
                    beforeFieldResult.fieldVal.asInstanceOf[m.FieldVal],
                    acc,
                    middlewareCtx,
                    updatedCtx)
                    .asInstanceOf[Option[Val]]
                    .getOrElse(acc)
                }

              def doErrorMiddleware(error: Throwable): Unit = {
                val it = mError.iterator
                while (it.hasNext) {
                  val r = it.next()
                  val m = r._3
                  m.fieldError(
                    r._2.asInstanceOf[m.QueryVal],
                    r._1.fieldVal.asInstanceOf[m.FieldVal],
                    error,
                    middlewareCtx,
                    updatedCtx)
                }
              }

              def doAfterMiddlewareWithMap[Val, NewVal](fn: Val => NewVal)(v: Val): NewVal =
                mAfter.foldLeft(fn(v)) { case (acc, (beforeFieldResult, mv, m)) =>
                  m.afterField(
                    mv.asInstanceOf[m.QueryVal],
                    beforeFieldResult.fieldVal.asInstanceOf[m.FieldVal],
                    acc,
                    middlewareCtx,
                    updatedCtx)
                    .asInstanceOf[Option[NewVal]]
                    .getOrElse(acc)
                }

              try {
                def createResolution(result: Any): StandardFieldResolution =
                  result match {
                    // these specific cases are important for time measuring middleware and eager values
                    case resolved: Value[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        if (mAfter.nonEmpty)
                          Value(doAfterMiddleware(resolved.value))
                        else
                          resolved,
                        None)

                    case resolved: PartialValue[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        if (mAfter.nonEmpty)
                          PartialValue(doAfterMiddleware(resolved.value), resolved.errors)
                        else
                          resolved,
                        if (mError.nonEmpty)
                          Some(MappedCtxUpdate(_ => userCtx, identity, doErrorMiddleware))
                        else None
                      )

                    case resolved: TryValue[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        if (mAfter.nonEmpty && resolved.value.isSuccess)
                          Value(doAfterMiddleware(resolved.value.get))
                        else
                          resolved,
                        if (mError.nonEmpty)
                          Some(MappedCtxUpdate(_ => userCtx, identity, doErrorMiddleware))
                        else None
                      )

                    case res: SequenceLeafAction[Ctx @unchecked, _] =>
                      StandardFieldResolution(
                        errors,
                        res,
                        Some(
                          MappedCtxUpdate(
                            _ => userCtx,
                            if (mAfter.nonEmpty) doAfterMiddleware else identity,
                            if (mError.nonEmpty) doErrorMiddleware else identity)))

                    case res: MappedSequenceLeafAction[
                          Ctx @unchecked,
                          Any @unchecked,
                          Any @unchecked] =>
                      val mapFn = res.mapFn.asInstanceOf[Any => Any]

                      StandardFieldResolution(
                        errors,
                        res.action,
                        Some(
                          MappedCtxUpdate(
                            _ => userCtx,
                            if (mAfter.nonEmpty) doAfterMiddlewareWithMap(mapFn) else mapFn,
                            if (mError.nonEmpty) doErrorMiddleware else identity))
                      )

                    case resolved: LeafAction[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        resolved,
                        if (mAfter.nonEmpty || mError.nonEmpty)
                          Some(
                            MappedCtxUpdate(
                              _ => userCtx,
                              if (mAfter.nonEmpty) doAfterMiddleware else identity,
                              if (mError.nonEmpty) doErrorMiddleware else identity))
                        else None
                      )

                    case res: UpdateCtx[Ctx @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        res.action,
                        Some(
                          MappedCtxUpdate(
                            res.nextCtx,
                            if (mAfter.nonEmpty) doAfterMiddleware else identity,
                            if (mError.nonEmpty) doErrorMiddleware else identity))
                      )

                    case res: MappedUpdateCtx[Ctx @unchecked, Any @unchecked, Any @unchecked] =>
                      StandardFieldResolution(
                        errors,
                        res.action,
                        Some(
                          MappedCtxUpdate(
                            res.nextCtx,
                            if (mAfter.nonEmpty) doAfterMiddlewareWithMap(res.mapFn) else res.mapFn,
                            if (mError.nonEmpty) doErrorMiddleware else identity))
                      )

                    case e => throw new IllegalStateException(s"Unsupported action: $e")
                  }

                val result =
                  beforeAction match {
                    case Some(action) => action
                    case None =>
                      field.resolve match {
                        case pfn: Projector[Ctx, Any, _] =>
                          pfn(updatedCtx, collectProjections(path, field, astFields, pfn.maxLevel))
                        case fn =>
                          fn(updatedCtx)
                      }
                  }
                result match {
                  case s: SubscriptionValue[Ctx @unchecked, _, _] =>
                    StreamFieldResolution(errors, s, createResolution)
                  case _ => createResolution(result)
                }
              } catch {
                case NonFatal(e) =>
                  try {
                    if (mError.nonEmpty) doErrorMiddleware(e)

                    ErrorFieldResolution(errors.add(path, e, astField.location))
                  } catch {
                    case NonFatal(me) =>
                      ErrorFieldResolution(
                        errors.add(path, e, astField.location).add(path, me, astField.location))
                  }
              }
            } catch {
              case NonFatal(e) => ErrorFieldResolution(errors.add(path, e, astField.location))
            }
          case Failure(error) => ErrorFieldResolution(errors.add(path, error))
        }
    }
  }

  private def collectProjections(
      path: ExecutionPath,
      field: Field[Ctx, _],
      astFields: Vector[ast.Field],
      maxLevel: Int): Vector[ProjectedName] = {

    def collectProjectionsInternal(
        path: ExecutionPath,
        tpe: OutputType[_],
        astFields: Vector[ast.Field],
        currLevel: Int): Vector[ProjectedName] =
      loop(path, tpe, astFields, currLevel)

    @tailrec
    def loop(
        path: ExecutionPath,
        tpe: OutputType[_],
        astFields: Vector[ast.Field],
        currLevel: Int): Vector[ProjectedName] =
      if (currLevel > maxLevel) Vector.empty
      else
        tpe match {
          case OptionType(ofType) => loop(path, ofType, astFields, currLevel)
          case ListType(ofType) => loop(path, ofType, astFields, currLevel)
          case objTpe: ObjectType[Ctx @unchecked, _] =>
            fieldCollector.collectFields(path, objTpe, astFields) match {
              case Success(ff) =>
                ff.fields.collect { case CollectedField(_, _, Success(astFields2)) =>
                  val astField = astFields2.head
                  val fields = objTpe.getField(schema, astField.name)
                  if (fields.isEmpty) Vector.empty
                  else {
                    val field = fields.head
                    if (field.tags.contains(ProjectionExclude)) Vector.empty
                    else {
                      val projectionNames = field.tags.iterator.collect {
                        case ProjectionName(name) => name
                      }

                      val projectedName =
                        if (projectionNames.nonEmpty) projectionNames
                        else Iterator.single(field.name)

                      projectedName.map { name =>
                        val children =
                          collectProjectionsInternal(
                            path.add(astField, objTpe),
                            field.fieldType,
                            astFields2,
                            currLevel + 1)
                        ProjectedName(name, children, Args(field, astField, variables))
                      }
                    }
                  }
                }.flatten
              case Failure(_) => Vector.empty
            }
          case abst: AbstractType =>
            schema.possibleTypes
              .get(abst.name)
              .map(
                _.flatMap(collectProjectionsInternal(path, _, astFields, currLevel + 1))
                  .groupBy(_.name)
                  .iterator
                  .map(_._2.head)
                  .toVector)
              .getOrElse(Vector.empty)
          case _ => Vector.empty
        }

    collectProjectionsInternal(path, field.fieldType, astFields, 1)
  }

  protected def isOptional(tpe: ObjectType[_, _], fieldName: String): Boolean =
    isOptional(tpe.getField(schema, fieldName).head.fieldType)

  protected def isOptional(tpe: OutputType[_]): Boolean =
    tpe.isInstanceOf[OptionType[_]]

  protected def isUndefinedValue(value: Any): Boolean =
    value == null || value == None

  private def nullForNotNullTypeError(position: Option[AstLocation]) =
    new ExecutionError(
      "Cannot return null for non-nullable type",
      exceptionHandler,
      sourceMapper,
      position.toList)

  /** The outcome of resolving a subtree: either a finished [[Result]], or (in concrete
    * [[Resolver]]s that support deferred/asynchronous values) something still pending on more work -
    * e.g. `FutureResolver`'s `DeferredResult` or the cats-effect module's `Pending`.
    *
    * This is intentionally not `sealed`: its non-[[Result]] subtypes are primitive-specific and
    * defined alongside each concrete [[Resolver]], not here.
    */
  protected trait Resolve {
    def appendErrors(
        path: ExecutionPath,
        errors: Vector[Throwable],
        position: Option[AstLocation]): Resolve
  }

  protected case class Result(
      errors: ErrorRegistry,
      value: Option[Any] /* Either marshaller.Node or marshaller.MapBuilder */,
      userContext: Option[Ctx] = None)
      extends Resolve {
    def addToMap(
        other: Result,
        key: String,
        optional: Boolean,
        path: ExecutionPath,
        position: Option[AstLocation],
        updatedErrors: ErrorRegistry): Result =
      copy(
        errors =
          if (!optional && other.value.isEmpty && other.errors.isEmpty)
            updatedErrors.add(other.errors).add(path, nullForNotNullTypeError(position))
          else
            updatedErrors.add(other.errors),
        value =
          if (optional && other.value.isEmpty)
            value.map(v =>
              marshaller.addMapNodeElem(
                v.asInstanceOf[marshaller.MapBuilder],
                key,
                marshaller.nullNode,
                optional = false))
          else
            for {
              myVal <- value
              otherVal <- other.value
            } yield marshaller.addMapNodeElem(
              myVal.asInstanceOf[marshaller.MapBuilder],
              key,
              otherVal.asInstanceOf[marshaller.Node],
              optional = false)
      )

    def nodeValue: Option[marshaller.Node] = value.asInstanceOf[Option[marshaller.Node]]
    private def builderValue = value.asInstanceOf[Option[marshaller.MapBuilder]]
    def buildValue: Result = copy(value = builderValue.map(marshaller.mapNode))

    def appendErrors(
        path: ExecutionPath,
        e: Vector[Throwable],
        position: Option[AstLocation]): Result =
      if (e.nonEmpty) copy(errors = errors.append(path, e, position))
      else this
  }

  protected def resolveSimpleListValue(
      simpleRes: Iterable[Result],
      path: ExecutionPath,
      optional: Boolean,
      astPosition: Option[AstLocation]): Result = {
    // this is very hot place, so resorting to mutability to minimize the footprint

    var errorReg = ErrorRegistry.empty
    val listBuilder = new VectorBuilder[marshaller.Node]
    listBuilder.sizeHint(simpleRes.size)
    var canceled = false
    val resIt = simpleRes.iterator

    while (resIt.hasNext && !canceled) {
      val res = resIt.next()

      if (!optional && res.value.isEmpty && res.errors.isEmpty)
        errorReg = errorReg.add(path, nullForNotNullTypeError(astPosition))
      else if (res.errors.nonEmpty)
        errorReg = errorReg.add(res.errors)

      res.nodeValue match {
        case node if optional =>
          listBuilder += marshaller.optionalArrayNodeValue(node)
        case Some(other) =>
          listBuilder += other
        case None =>
          canceled = true
      }
    }

    Result(errorReg, if (canceled) None else Some(marshaller.arrayNode(listBuilder.result())))
  }
}
