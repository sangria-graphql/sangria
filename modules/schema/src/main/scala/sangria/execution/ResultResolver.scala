package sangria.execution

import sangria.ast.AstLocation
import sangria.marshalling.{ResultMarshaller, SimpleResultMarshallerForType}
import sangria.validation.{AstNodeLocation, Violation}

class ResultResolver(
    val marshaller: ResultMarshaller,
    exceptionHandler: ExceptionHandler,
    preserveOriginalErrors: Boolean) {
  def marshalErrors(errors: ErrorRegistry) =
    if (errors.isEmpty) None else Some(marshaller.arrayNode(errors.errorList))

  def marshalResult(
      data: Option[marshaller.Node],
      errors: Option[marshaller.Node],
      extensions: Option[marshaller.Node]) = {
    val names =
      if (errors.isDefined & extensions.isDefined) Vector("data", "errors", "extensions")
      else if (errors.isDefined) Vector("data", "errors")
      else if (extensions.isDefined) Vector("data", "extensions")
      else Vector("data")

    val empty = marshaller.emptyMapNode(names)

    val withData = data match {
      case Some(d) => marshaller.addMapNodeElem(empty, "data", d, optional = false)
      case None => marshaller.addMapNodeElem(empty, "data", marshaller.nullNode, optional = false)
    }

    val withErrors = errors match {
      case Some(e) => marshaller.addMapNodeElem(withData, "errors", e, optional = false)
      case None => withData
    }

    val withExtensions = extensions match {
      case Some(e) => marshaller.addMapNodeElem(withErrors, "extensions", e, optional = true)
      case None => withErrors
    }

    marshaller.mapNode(withExtensions)
  }

  def resolveError(error: Throwable) =
    marshalResult(None, marshalErrors(ErrorRegistry(ExecutionPath.empty, error)), None)

  def handleSupportedError(
      path: ExecutionPath,
      handledException: HandledException,
      locations: List[AstLocation]) =
    handledException match {
      case SingleHandledException(
            message,
            additionalFields,
            newLocations,
            addFieldsInExtensions,
            addFieldsInError) =>
        val msg = if (message == null) "" else message
        val af = additionalFields.toSeq.asInstanceOf[Seq[(String, marshaller.Node)]]

        Vector(
          errorNode(
            msg,
            path,
            locations ++ newLocations,
            if (addFieldsInError) af else Seq.empty,
            if (addFieldsInExtensions) af else Seq.empty))
      case MultipleHandledExceptions(messages, addFieldsInExtensions, addFieldsInError) =>
        messages.map { case (message, additionalFields, newLocations) =>
          val msg = if (message == null) "" else message
          val af = additionalFields.toSeq.asInstanceOf[Seq[(String, marshaller.Node)]]

          errorNode(
            msg,
            path,
            locations ++ newLocations,
            if (addFieldsInError) af else Seq.empty,
            if (addFieldsInExtensions) af else Seq.empty)
        }
    }

  private def getLocations(violation: Violation): List[AstLocation] =
    violation match {
      case v: AstNodeLocation if v.locations.nonEmpty => v.locations
      case _ => Nil
    }

  private def getLocations(error: Throwable): List[AstLocation] =
    error match {
      case error: AstNodeLocation if error.locations.nonEmpty => error.locations
      case _ => Nil
    }

  private def createLocation(loc: AstLocation) = marshaller.mapNode(
    Seq(
      "line" -> marshaller.scalarNode(loc.line, "Int", Set.empty),
      "column" -> marshaller.scalarNode(loc.column, "Int", Set.empty)))

  private def errorNode(
      message: String,
      path: ExecutionPath,
      positions: List[AstLocation],
      additionalFields: Seq[(String, marshaller.Node)] = Nil,
      additionalExtensionFields: Seq[(String, marshaller.Node)] = Nil): marshaller.Node =
    mapNode(
      messageFields(message) ++
        pathFields(path) ++
        positionFields(positions) ++
        additionalFields ++
        (if (additionalExtensionFields.nonEmpty)
           Seq("extensions" -> mapNode(additionalExtensionFields))
         else Seq.empty))

  private def mapNode(fields: Seq[(String, marshaller.Node)]): marshaller.Node =
    marshaller.mapNode(fields.foldLeft(marshaller.emptyMapNode(fields.map(_._1))) {
      case (acc, (name, value)) => marshaller.addMapNodeElem(acc, name, value, optional = false)
    })

  private def messageFields(message: String): Seq[(String, marshaller.Node)] =
    Seq("message" -> marshaller.scalarNode(message, "String", Set.empty))

  private def pathFields(path: ExecutionPath): Seq[(String, marshaller.Node)] =
    if (path.nonEmpty)
      Seq("path" -> path.marshal(marshaller))
    else
      Seq.empty

  private def positionFields(positions: List[AstLocation]): Seq[(String, marshaller.Node)] =
    if (positions.nonEmpty)
      Seq("locations" -> marshallPositions(positions))
    else
      Seq.empty

  private def marshallPositions(px: List[AstLocation]) =
    marshaller.mapAndMarshal(px, createLocation)

  case class ErrorRegistry(
      errorList: Vector[marshaller.Node],
      originalErrors: Vector[RegisteredError]) {
    def nonEmpty = errorList.nonEmpty
    def isEmpty = errorList.isEmpty

    def add(path: ExecutionPath, error: Throwable) =
      copy(
        errorList ++ createErrorPaths(path, error, None),
        if (preserveOriginalErrors) originalErrors :+ RegisteredError(path, error, None)
        else originalErrors)

    def append(path: ExecutionPath, errors: Vector[Throwable], position: Option[AstLocation]) =
      copy(
        errors.flatMap(e => createErrorPaths(path, e, position)) ++ errorList,
        if (preserveOriginalErrors)
          errors.map(e => RegisteredError(path, e, position)) ++ originalErrors
        else originalErrors
      )

    def add(path: ExecutionPath, error: Throwable, position: Option[AstLocation]) =
      copy(
        errorList ++ createErrorPaths(path, error, position),
        if (preserveOriginalErrors) originalErrors :+ RegisteredError(path, error, position)
        else originalErrors)

    def add(other: ErrorRegistry) =
      ErrorRegistry(
        errorList ++ other.errorList,
        if (preserveOriginalErrors) originalErrors ++ other.originalErrors else originalErrors)

    private def createErrorPaths(
        path: ExecutionPath,
        error: Throwable,
        position: Option[AstLocation]) =
      error match {
        case e: WithViolations if e.violations.nonEmpty =>
          e.violations.flatMap {
            case v if exceptionHandler.onViolation.isDefinedAt(marshaller -> v) =>
              handleSupportedError(
                path,
                exceptionHandler.onViolation(marshaller -> v),
                position.toList ++ getLocations(v))
            case v =>
              Vector(errorNode(v.errorMessage, path, position.toList ++ getLocations(v)))
          }

        case e: UserFacingError
            if exceptionHandler.onUserFacingError.isDefinedAt(marshaller -> e) =>
          handleSupportedError(
            path,
            exceptionHandler.onUserFacingError(marshaller -> e),
            position.toList ++ getLocations(e))

        case e: UserFacingError =>
          Vector(errorNode(e.getMessage, path, position.toList ++ getLocations(e)))

        case e if exceptionHandler.onException.isDefinedAt(marshaller -> e) =>
          handleSupportedError(
            path,
            exceptionHandler.onException(marshaller -> e),
            position.toList ++ getLocations(e))

        case QueryReducingError(cause, _)
            if exceptionHandler.onException.isDefinedAt(marshaller -> cause) =>
          handleSupportedError(
            path,
            exceptionHandler.onException(marshaller -> cause),
            position.toList ++ getLocations(cause))

        case e =>
          e.printStackTrace()

          Vector(errorNode("Internal server error", path, position.toList ++ getLocations(e)))
      }
  }

  object ErrorRegistry {
    val empty = ErrorRegistry(Vector.empty, Vector.empty)

    def apply(path: ExecutionPath, error: Throwable): ErrorRegistry = empty.add(path, error)
    def apply(path: ExecutionPath, error: Throwable, pos: Option[AstLocation]): ErrorRegistry =
      empty.add(path, error, pos)
  }
}

object ResultResolver {
  def marshalExtensions(
      marshaller: ResultMarshaller,
      extensions: Seq[Extension[_]]): Option[marshaller.Node] = {
    import scala.collection.mutable
    import sangria.marshalling.MarshallingUtil._

    implicit val m = SimpleResultMarshallerForType[marshaller.Node](marshaller)
    val res = new mutable.LinkedHashMap[String, marshaller.Node]

    extensions.foreach { e =>
      val eAny = e.asInstanceOf[Extension[Any]]

      implicit val iu = eAny.iu

      if (iu.isMapNode(eAny.data)) {
        iu.getMapKeys(e.data).map { key =>
          iu.getMapValue(e.data, key).foreach { value =>
            res(key) = value.convertMarshaled[marshaller.Node]
          }
        }
      }
    }

    if (res.nonEmpty) Some(marshaller.mapNode(res.toVector))
    else None
  }
}

case class RegisteredError(path: ExecutionPath, error: Throwable, position: Option[AstLocation])
