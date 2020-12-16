package sangria.execution

import sangria.ast
import sangria.execution._
import sangria.marshalling.{InputUnmarshaller, ScalaInput}
import sangria.schema._
import sangria.util.tag.@@
import sangria.validation.QueryValidator
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object QueryReducerExecutor {
  def reduceQueryWithoutVariables[Ctx, Root](
      schema: Schema[Ctx, Root],
      queryAst: ast.Document,
      userContext: Ctx,
      queryReducers: List[QueryReducer[Ctx, _]],
      operationName: Option[String] = None,
      queryValidator: QueryValidator = QueryValidator.default,
      exceptionHandler: ExceptionHandler = ExceptionHandler.empty,
      deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
      middleware: List[Middleware[Ctx]] = Nil
  )(implicit executionContext: ExecutionContext): Future[(Ctx, TimeMeasurement)] = {
    val violations = queryValidator.validateQuery(schema, queryAst)

    if (violations.nonEmpty)
      Future.failed(ValidationError(violations, exceptionHandler))
    else {
      val scalarMiddleware = Middleware.composeFromScalarMiddleware(middleware, userContext)
      val valueCollector = new ValueCollector[Ctx, _ @@ ScalaInput](
        schema,
        InputUnmarshaller.emptyMapVars,
        queryAst.sourceMapper,
        deprecationTracker,
        userContext,
        exceptionHandler,
        scalarMiddleware,
        true)(InputUnmarshaller.scalaInputUnmarshaller[_ @@ ScalaInput])

      val executionResult = for {
        operation <- Executor.getOperation(exceptionHandler, queryAst, operationName)
        fieldCollector = new FieldCollector[Ctx, Root](
          schema,
          queryAst,
          Map.empty,
          queryAst.sourceMapper,
          valueCollector,
          exceptionHandler)
        tpe <- Executor.getOperationRootType(
          schema,
          exceptionHandler,
          operation,
          queryAst.sourceMapper)
        fields <- fieldCollector.collectFields(ExecutionPath.empty, tpe, Vector(operation))
      } yield QueryReducerExecutor.reduceQuery(
        schema,
        queryReducers,
        exceptionHandler,
        fieldCollector,
        valueCollector,
        Map.empty,
        tpe,
        fields,
        userContext)

      executionResult match {
        case Success(future) => future
        case Failure(error) => Future.failed(error)
      }
    }
  }

  /** Returns either new Ctx or future of it (with time measurement) */
  def reduceQuery[Ctx, Root, Val](
      schema: Schema[Ctx, Root],
      queryReducers: List[QueryReducer[Ctx, _]],
      exceptionHandler: ExceptionHandler,
      fieldCollector: FieldCollector[Ctx, Root],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      rootTpe: ObjectType[Ctx, Root],
      fields: CollectedFields,
      userContext: Ctx)(implicit
      executionContext: ExecutionContext): Future[(Ctx, TimeMeasurement)] =
    if (queryReducers.nonEmpty) {
      val sw = StopWatch.start()
      reduceQueryUnsafe(
        schema,
        fieldCollector,
        valueCollector,
        variables,
        rootTpe,
        fields,
        queryReducers.toVector,
        userContext)
        .map(_ -> sw.stop)
        .recover { case error: Throwable => throw QueryReducingError(error, exceptionHandler) }
    } else Future.successful(userContext -> TimeMeasurement.empty)

  private def reduceQueryUnsafe[Ctx, Val](
      schema: Schema[Ctx, _],
      fieldCollector: FieldCollector[Ctx, Val],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      rootTpe: ObjectType[Ctx, _],
      fields: CollectedFields,
      reducers: Vector[QueryReducer[Ctx, _]],
      userContext: Ctx)(implicit executionContext: ExecutionContext): Future[Ctx] = {
    val argumentValuesFn: QueryReducer.ArgumentValuesFn =
      (path: ExecutionPath, argumentDefs: List[Argument[_]], argumentAsts: Vector[ast.Argument]) =>
        valueCollector.getFieldArgumentValues(path, None, argumentDefs, argumentAsts, variables)

    val initialValues: Vector[Any] = reducers.map(_.initial)

    def loop(path: ExecutionPath, tpe: OutputType[_], astFields: Vector[ast.Field]): Seq[Any] =
      tpe match {
        case OptionType(ofType) => loop(path, ofType, astFields)
        case ListType(ofType) => loop(path, ofType, astFields)
        case objTpe: ObjectType[Ctx @unchecked, _] =>
          fieldCollector.collectFields(path, objTpe, astFields) match {
            case Success(ff) =>
              // Using mutability here locally in order to reduce footprint
              ff.fields.foldLeft(Array(initialValues: _*)) {
                case (acc, CollectedField(_, _, Success(fields)))
                    if objTpe.getField(schema, fields.head.name).nonEmpty =>
                  val astField = fields.head
                  val field = objTpe.getField(schema, astField.name).head
                  val newPath = path.add(astField, objTpe)
                  val childReduced = loop(newPath, field.fieldType, fields)

                  for (i <- reducers.indices) {
                    val reducer = reducers(i)

                    acc(i) = reducer.reduceField[Any](
                      acc(i).asInstanceOf[reducer.Acc],
                      childReduced(i).asInstanceOf[reducer.Acc],
                      newPath,
                      userContext,
                      fields,
                      objTpe.asInstanceOf[ObjectType[Any, Any]],
                      field.asInstanceOf[Field[Ctx, Any]],
                      argumentValuesFn
                    )
                  }

                  acc
                case (acc, _) => acc
              }
            case Failure(_) => initialValues
          }
        case abst: AbstractType =>
          schema.possibleTypes
            .get(abst.name)
            .map(types =>
              types.map(loop(path, _, astFields)).transpose.zipWithIndex.map { case (values, idx) =>
                val reducer = reducers(idx)
                reducer.reduceAlternatives(values.asInstanceOf[Seq[reducer.Acc]])
              })
            .getOrElse(initialValues)
        case s: ScalarType[_] => reducers.map(_.reduceScalar(path, userContext, s))
        case ScalarAlias(aliasFor, _, _) =>
          reducers.map(_.reduceScalar(path, userContext, aliasFor))
        case e: EnumType[_] => reducers.map(_.reduceEnum(path, userContext, e))
        case _ => initialValues
      }

    val reduced = fields.fields.foldLeft(Array(initialValues: _*)) {
      case (acc, CollectedField(_, _, Success(astFields)))
          if rootTpe.getField(schema, astFields.head.name).nonEmpty =>
        val astField = astFields.head
        val field = rootTpe.getField(schema, astField.name).head
        val path = ExecutionPath.empty.add(astField, rootTpe)
        val childReduced = loop(path, field.fieldType, astFields)

        for (i <- reducers.indices) {
          val reducer = reducers(i)

          acc(i) = reducer.reduceField(
            acc(i).asInstanceOf[reducer.Acc],
            childReduced(i).asInstanceOf[reducer.Acc],
            path,
            userContext,
            astFields,
            rootTpe.asInstanceOf[ObjectType[Any, Any]],
            field.asInstanceOf[Field[Ctx, Any]],
            argumentValuesFn
          )
        }

        acc
      case (acc, _) => acc
    }

    val newContext =
      try
      // Unsafe part to avoid additional boxing in order to reduce the footprint
      reducers.zipWithIndex.foldLeft(userContext: Any) {
        case (acc: Future[Ctx @unchecked], (reducer, idx)) =>
          acc.flatMap(a =>
            reducer.reduceCtx(reduced(idx).asInstanceOf[reducer.Acc], a) match {
              case FutureValue(future) => future
              case Value(value) => Future.successful(value)
              case TryValue(value) => Future.fromTry(value)
            })

        case (acc: Ctx @unchecked, (reducer, idx)) =>
          reducer.reduceCtx(reduced(idx).asInstanceOf[reducer.Acc], acc) match {
            case FutureValue(future) => future
            case Value(value) => value
            case TryValue(value) => value.get
          }

        case (acc, _) =>
          Future.failed(new IllegalStateException(s"Invalid shape of the user context! $acc"))
      } catch {
        case NonFatal(error) => Future.failed(error)
      }

    newContext match {
      case fut: Future[Ctx @unchecked] => fut
      case ctx => Future.successful(ctx.asInstanceOf[Ctx])
    }
  }

}
