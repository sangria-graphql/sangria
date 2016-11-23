package sangria.execution

import sangria.ast
import sangria.marshalling.{InputUnmarshaller, ResultMarshaller}
import sangria.parser.SourceMapper
import sangria.schema._
import sangria.validation.QueryValidator
import InputUnmarshaller.emptyMapVars
import sangria.execution.deferred.DeferredResolver

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

case class Executor[Ctx, Root](
    schema: Schema[Ctx, Root],
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
    exceptionHandler: Executor.ExceptionHandler = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware[Ctx]] = Nil,
    maxQueryDepth: Option[Int] = None,
    queryReducers: List[QueryReducer[Ctx, _]] = Nil)(implicit executionContext: ExecutionContext) {

  def prepare[Input](
      queryAst: ast.Document,
      userContext: Ctx,
      root: Root,
      operationName: Option[String] = None,
      variables: Input = emptyMapVars)(implicit um: InputUnmarshaller[Input]): Future[PreparedQuery[Ctx, Root, Input]] = {
    val (violations, validationTiming) = TimeMeasurement.measure(queryValidator.validateQuery(schema, queryAst))

    if (violations.nonEmpty)
      Future.failed(ValidationError(violations, exceptionHandler))
    else {
      val valueCollector = new ValueCollector[Ctx, Input](schema, variables, queryAst.sourceMapper, deprecationTracker, userContext, exceptionHandler)(um)

      val executionResult = for {
        operation ← getOperation(queryAst, operationName)
        unmarshalledVariables ← valueCollector.getVariableValues(operation.variables)
        fieldCollector = new FieldCollector[Ctx, Root](schema, queryAst, unmarshalledVariables, queryAst.sourceMapper, valueCollector, exceptionHandler)
        tpe ← getOperationRootType(operation, queryAst.sourceMapper)
        fields ← fieldCollector.collectFields(ExecutionPath.empty, tpe, Vector(operation))
      } yield {
        val preparedFields = fields.fields.flatMap {
          case CollectedField(_, astField, Success(_)) ⇒
            val allFields = tpe.getField(schema, astField.name).asInstanceOf[Vector[Field[Ctx, Root]]]
            val field = allFields.head
            val args = valueCollector.getFieldArgumentValues(ExecutionPath.empty.add(astField, tpe), field.arguments, astField.arguments, unmarshalledVariables)

            args.toOption.map(PreparedField(field, _))
          case _ ⇒ None
        }

        reduceQuerySafe(fieldCollector, valueCollector, unmarshalledVariables, tpe, fields, userContext) match {
          case fut: Future[(Ctx, TimeMeasurement) @unchecked] ⇒
            fut.map(newCtx ⇒
              new PreparedQuery[Ctx, Root, Input](queryAst, operation, tpe, newCtx._1, root, preparedFields,
                (c: Ctx, r: Root, m: ResultMarshaller, scheme: ExecutionScheme) ⇒
                  executeOperation(queryAst, operationName, variables, um, operation, queryAst.sourceMapper, valueCollector,
                    fieldCollector, m, unmarshalledVariables, tpe, fields, c, r, scheme, validationTiming, newCtx._2)))
          case (newCtx: Ctx @unchecked, timing: TimeMeasurement) ⇒
            Future.successful(new PreparedQuery[Ctx, Root, Input](queryAst, operation, tpe, newCtx, root, preparedFields,
              (c: Ctx, r: Root, m: ResultMarshaller, scheme: ExecutionScheme) ⇒
                executeOperation(queryAst, operationName, variables, um, operation, queryAst.sourceMapper, valueCollector,
                  fieldCollector, m, unmarshalledVariables, tpe, fields, c, r, scheme, validationTiming, timing)))
        }
      }

      executionResult match {
        case Success(future) ⇒ future
        case Failure(error) ⇒ Future.failed(error)
      }
    }
  }

  def execute[Input](
      queryAst: ast.Document,
      userContext: Ctx,
      root: Root,
      operationName: Option[String] = None,
      variables: Input = emptyMapVars)(implicit marshaller: ResultMarshaller, um: InputUnmarshaller[Input], scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] = {
    val (violations, validationTiming) = TimeMeasurement.measure(queryValidator.validateQuery(schema, queryAst))

    if (violations.nonEmpty)
      scheme.failed(ValidationError(violations, exceptionHandler))
    else {
      val valueCollector = new ValueCollector[Ctx, Input](schema, variables, queryAst.sourceMapper, deprecationTracker, userContext, exceptionHandler)(um)

      val executionResult = for {
        operation ← getOperation(queryAst, operationName)
        unmarshalledVariables ← valueCollector.getVariableValues(operation.variables)
        fieldCollector = new FieldCollector[Ctx, Root](schema, queryAst, unmarshalledVariables, queryAst.sourceMapper, valueCollector, exceptionHandler)
        tpe ← getOperationRootType(operation, queryAst.sourceMapper)
        fields ← fieldCollector.collectFields(ExecutionPath.empty, tpe, Vector(operation))
      } yield reduceQuerySafe(fieldCollector, valueCollector, unmarshalledVariables, tpe, fields, userContext) match {
        case fut: Future[(Ctx, TimeMeasurement) @unchecked] ⇒
          scheme.flatMapFuture(fut)(c ⇒ executeOperation(queryAst, operationName, variables, um, operation, queryAst.sourceMapper, valueCollector,
            fieldCollector, marshaller, unmarshalledVariables, tpe, fields, c._1, root, scheme, validationTiming, c._2))
        case (ctx: Ctx @unchecked, timing: TimeMeasurement) ⇒
          executeOperation(queryAst, operationName, variables, um, operation, queryAst.sourceMapper, valueCollector,
            fieldCollector, marshaller, unmarshalledVariables, tpe, fields, ctx, root, scheme, validationTiming, timing)
      }

      executionResult match {
        case Success(result) ⇒ result
        case Failure(error) ⇒ scheme.failed(error)
      }
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(OperationSelectionError("Must provide operation name if query contains multiple operations", exceptionHandler))
    else {
      val unexpectedDefinition = document.definitions.find(d ⇒ !(d.isInstanceOf[ast.OperationDefinition] || d.isInstanceOf[ast.FragmentDefinition]))

      unexpectedDefinition match {
        case Some(unexpected) ⇒
          Failure(new ExecutionError(s"GraphQL cannot execute a request containing a ${unexpected.getClass.getSimpleName}.", exceptionHandler))
        case None ⇒
          operationName match {
            case Some(opName) ⇒
              document.operations get Some(opName) map (Success(_)) getOrElse
                Failure(OperationSelectionError(s"Unknown operation name '$opName'", exceptionHandler))
            case None ⇒
              Success(document.operations.values.head)
          }
      }
    }

  def executeOperation[Input](
        queryAst: ast.Document,
        operationName: Option[String] = None,
        inputVariables: Input,
        inputUnmarshaller: InputUnmarshaller[Input],
        operation: ast.OperationDefinition,
        sourceMapper: Option[SourceMapper],
        valueCollector: ValueCollector[Ctx, _],
        fieldCollector: FieldCollector[Ctx, Root],
        marshaller: ResultMarshaller,
        variables: Map[String, VariableValue],
        tpe: ObjectType[Ctx, Root],
        fields: CollectedFields,
        ctx: Ctx,
        root: Root,
        scheme: ExecutionScheme,
        validationTiming: TimeMeasurement,
        queryReducerTiming: TimeMeasurement): scheme.Result[Ctx, marshaller.Node] = {
      val middlewareCtx = MiddlewareQueryContext(ctx, this, queryAst, operationName, inputVariables, inputUnmarshaller, validationTiming, queryReducerTiming)

      try {
        val middlewareVal = middleware map (m ⇒ m.beforeQuery(middlewareCtx) → m)
        val deferredResolverState = deferredResolver.initialQueryState

        val resolver = new Resolver[Ctx](
          marshaller,
          middlewareCtx,
          schema,
          valueCollector,
          variables,
          fieldCollector,
          ctx,
          exceptionHandler,
          deferredResolver,
          sourceMapper,
          deprecationTracker,
          middlewareVal,
          maxQueryDepth,
          deferredResolverState,
          scheme.extended,
          validationTiming,
          queryReducerTiming)

        val result =
          operation.operationType match {
            case ast.OperationType.Query ⇒ resolver.resolveFieldsPar(tpe, root, fields)(scheme).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
            case ast.OperationType.Mutation ⇒ resolver.resolveFieldsSeq(tpe, root, fields)(scheme).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
            case ast.OperationType.Subscription ⇒
              tpe.uniqueFields.head.tags.collectFirst{case SubscriptionField(s) ⇒ s} match {
                case Some(stream) ⇒
                  // Streaming is supported - resolve as a real subscription
                  resolver.resolveFieldsSubs(tpe, root, fields)(scheme).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
                case None ⇒
                  // No streaming is supported - resolve as a normal "query" operation
                  resolver.resolveFieldsPar(tpe, root, fields)(scheme).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
              }

          }

        if (middlewareVal.nonEmpty)
          scheme.onComplete(result)(
            middlewareVal foreach { case (v, m) ⇒ m.afterQuery(v.asInstanceOf[m.QueryVal], middlewareCtx)})
        else result
      } catch {
        case NonFatal(error) ⇒
          scheme.failed(error)
      }
    }

  def getOperationRootType(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query ⇒
      Success(schema.query)
    case ast.OperationType.Mutation ⇒
      schema.mutation map (Success(_)) getOrElse
        Failure(OperationSelectionError("Schema is not configured for mutations", exceptionHandler, sourceMapper, operation.position.toList))
    case ast.OperationType.Subscription ⇒
      schema.subscription map (Success(_)) getOrElse
        Failure(OperationSelectionError("Schema is not configured for subscriptions", exceptionHandler, sourceMapper, operation.position.toList))
  }

  // returns either new Ctx or future of it (with time measurement)
  private def reduceQuerySafe[Val](
      fieldCollector: FieldCollector[Ctx, Root],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      rootTpe: ObjectType[Ctx, Root],
      fields: CollectedFields,
      userContext: Ctx): Any =
    if (queryReducers.nonEmpty) {
      val startTime = System.currentTimeMillis()
      val start = System.nanoTime()

      def timeMeasurement[T](res: T) = {
        val end = System.nanoTime()
        val endTime = System.currentTimeMillis()

        res → TimeMeasurement(startTime, endTime, end - start)
      }

      reduceQuery(fieldCollector, valueCollector, variables, rootTpe, fields, queryReducers.toVector, userContext) match {
        case future: Future[Ctx] ⇒
          future
            .map(newCtx ⇒ timeMeasurement(newCtx))
            .recover { case error: Throwable ⇒ throw QueryReducingError(error, exceptionHandler) }
        case newCtx: Ctx@unchecked ⇒
          timeMeasurement(newCtx)
      }
    } else userContext → TimeMeasurement.empty

  private def reduceQuery[Val](
      fieldCollector: FieldCollector[Ctx, Val],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      rootTpe: ObjectType[_, _],
      fields: CollectedFields,
      reducers: Vector[QueryReducer[Ctx, _]],
      userContext: Ctx): Any = {
    // Using mutability here locally in order to reduce footprint
    import scala.collection.mutable.ListBuffer

    val argumentValuesFn = (path: ExecutionPath, argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument]) ⇒
      valueCollector.getFieldArgumentValues(path, argumentDefs, argumentAsts, variables)

    val initialValues: Vector[Any] = reducers map (_.initial)

    def loop(path: ExecutionPath, tpe: OutputType[_], astFields: Vector[ast.Field]): Seq[Any] =
      tpe match {
        case OptionType(ofType) ⇒ loop(path, ofType, astFields)
        case ListType(ofType) ⇒ loop(path, ofType, astFields)
        case objTpe: ObjectType[Ctx, _] ⇒
          fieldCollector.collectFields(path, objTpe, astFields) match {
            case Success(ff) ⇒
              ff.fields.foldLeft(ListBuffer(initialValues: _*)) {
                case (acc, CollectedField(_, _, Success(fields))) if objTpe.getField(schema, fields.head.name).nonEmpty ⇒
                  val astField = fields.head
                  val field = objTpe.getField(schema, astField.name).head
                  val newPath = path.add(astField, objTpe)
                  val childReduced = loop(newPath, field.fieldType, fields)

                  for (i ← reducers.indices) {
                    val reducer = reducers(i)

                    acc(i) = reducer.reduceField[Any](
                      acc(i).asInstanceOf[reducer.Acc],
                      childReduced(i).asInstanceOf[reducer.Acc],
                      newPath, userContext, fields,
                      objTpe.asInstanceOf[ObjectType[Any, Any]],
                      field.asInstanceOf[Field[Ctx, Any]], argumentValuesFn)
                  }

                  acc
                case (acc, _) ⇒ acc
              }
            case Failure(_) ⇒ initialValues
          }
        case abst: AbstractType ⇒
          schema.possibleTypes
            .get (abst.name)
            .map (types ⇒
              types.map(loop(path, _, astFields)).transpose.zipWithIndex.map{
                case (values, idx) ⇒
                  val reducer = reducers(idx)
                  reducer.reduceAlternatives(values.asInstanceOf[Seq[reducer.Acc]])
              })
            .getOrElse (initialValues)
        case s: ScalarType[_] ⇒ reducers map (_.reduceScalar(path, userContext, s))
        case e: EnumType[_] ⇒ reducers map (_.reduceEnum(path, userContext, e))
        case _ ⇒ initialValues
      }

    val reduced = fields.fields.foldLeft(ListBuffer(initialValues: _*)) {
      case (acc, CollectedField(_, _, Success(astFields))) if rootTpe.getField(schema, astFields.head.name).nonEmpty ⇒
        val astField = astFields.head
        val field = rootTpe.getField(schema, astField.name).head
        val path = ExecutionPath.empty.add(astField, rootTpe)
        val childReduced = loop(path, field.fieldType, astFields)

        for (i ← reducers.indices) {
          val reducer = reducers(i)

          acc(i) = reducer.reduceField(
            acc(i).asInstanceOf[reducer.Acc],
            childReduced(i).asInstanceOf[reducer.Acc],
            path, userContext, astFields,
            rootTpe.asInstanceOf[ObjectType[Any, Any]],
            field.asInstanceOf[Field[Ctx, Any]], argumentValuesFn)
        }

        acc
      case (acc, _) ⇒ acc
    }

    try {
      // Unsafe part to avoid addition boxing in order to reduce the footprint
      reducers.zipWithIndex.foldLeft(userContext: Any) {
        case (acc: Future[Ctx], (reducer, idx)) ⇒
          acc.flatMap(a ⇒ reducer.reduceCtx(reduced(idx).asInstanceOf[reducer.Acc], a) match {
            case FutureValue(future) ⇒ future
            case Value(value) ⇒ Future.successful(value)
            case TryValue(value) ⇒ Future.fromTry(value)
          })

        case (acc: Ctx @unchecked, (reducer, idx)) ⇒
          reducer.reduceCtx(reduced(idx).asInstanceOf[reducer.Acc], acc) match {
            case FutureValue(future) ⇒ future
            case Value(value) ⇒ value
            case TryValue(value) ⇒ value.get
          }

        case (acc, _) ⇒ Future.failed(new IllegalStateException(s"Invalid shape of the user context! $acc"))
      }
    } catch {
      case NonFatal(error) ⇒ Future.failed(error)
    }
  }
}

object Executor {
  type ExceptionHandler = PartialFunction[(ResultMarshaller, Throwable), HandledException]

  def execute[Ctx, Root, Input](
    schema: Schema[Ctx, Root],
    queryAst: ast.Document,
    userContext: Ctx = (),
    root: Root = (),
    operationName: Option[String] = None,
    variables: Input = emptyMapVars,
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
    exceptionHandler: Executor.ExceptionHandler = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware[Ctx]] = Nil,
    maxQueryDepth: Option[Int] = None,
    queryReducers: List[QueryReducer[Ctx, _]] = Nil
  )(implicit executionContext: ExecutionContext, marshaller: ResultMarshaller, um: InputUnmarshaller[Input], scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    Executor(schema, queryValidator, deferredResolver, exceptionHandler, deprecationTracker, middleware, maxQueryDepth, queryReducers)
      .execute(queryAst, userContext, root, operationName, variables)

  def prepare[Ctx, Root, Input](
    schema: Schema[Ctx, Root],
    queryAst: ast.Document,
    userContext: Ctx = (),
    root: Root = (),
    operationName: Option[String] = None,
    variables: Input = emptyMapVars,
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
    exceptionHandler: Executor.ExceptionHandler = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware[Ctx]] = Nil,
    maxQueryDepth: Option[Int] = None,
    queryReducers: List[QueryReducer[Ctx, _]] = Nil
  )(implicit executionContext: ExecutionContext, um: InputUnmarshaller[Input]): Future[PreparedQuery[Ctx, Root, Input]] =
    Executor(schema, queryValidator, deferredResolver, exceptionHandler, deprecationTracker, middleware, maxQueryDepth, queryReducers)
      .prepare(queryAst, userContext, root, operationName, variables)
}

case class HandledException(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty)

class PreparedQuery[Ctx, Root, Input] private[execution] (
    val queryAst: ast.Document,
    val operation: ast.OperationDefinition,
    val tpe: ObjectType[Ctx, Root],
    val userContext: Ctx,
    val root: Root,
    val fields: Seq[PreparedField[Ctx, Root]],
    execFn: (Ctx, Root, ResultMarshaller, ExecutionScheme) ⇒ Any) {
  def execute(userContext: Ctx = userContext, root: Root = root)(implicit marshaller: ResultMarshaller, scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    execFn(userContext, root, marshaller, scheme).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
}

case class PreparedField[Ctx, Root](field: Field[Ctx, Root], args: Args)