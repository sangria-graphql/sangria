package sangria.execution

import org.parboiled2.Position
import sangria.marshalling.ResultMarshaller
import sangria.validation.{Violation, AstNodeLocation}

class ResultResolver(val marshaller: ResultMarshaller, exceptionHandler: Executor.ExceptionHandler) {
  def marshalErrors(errors: ErrorRegistry) =
    if (errors.errorList.isEmpty) None else Some(marshaller.arrayNode(errors.errorList))

  def marshalResult(data: Option[marshaller.Node], errors: Option[marshaller.Node]) = {
    val empty = marshaller.emptyMapNode(if (errors.isDefined) Vector("data", "errors") else Vector("data"))

    val withData = data match {
      case Some(d) ⇒ marshaller.addMapNodeElem(empty, "data", d, optional = false)
      case None ⇒ marshaller.addMapNodeElem(empty, "data", marshaller.nullNode, optional = false)
    }

    errors match {
      case Some(e) ⇒ marshaller.mapNode(marshaller.addMapNodeElem(withData, "errors", e, optional = false))
      case None ⇒ marshaller.mapNode(withData)
    }
  }

  def resolveError(error: Throwable) =
    marshalResult(None, marshalErrors(ErrorRegistry(Vector.empty, error)))

  def handleSupportedError(e: Throwable) = {
    val handeled = exceptionHandler(marshaller → e)

    Seq("message" → marshaller.stringNode(handeled.message)) ++ handeled.additionalFields.toSeq.asInstanceOf[Seq[(String, marshaller.Node)]]
  }

  def handleException(exception: Throwable) = exception match {
    case e: UserFacingError ⇒
      Seq("message" → marshaller.stringNode(e.getMessage))
    case e if exceptionHandler isDefinedAt (marshaller → e) ⇒
      handleSupportedError(e)
    case QueryReducingError(cause, _) if exceptionHandler isDefinedAt (marshaller → cause) ⇒
      handleSupportedError(cause)
    case e ⇒
      e.printStackTrace() // todo proper logging?
      Seq("message" → marshaller.stringNode("Internal server error"))
  }

  case class ErrorRegistry(errorList: Vector[marshaller.Node]) {
    def add(path: Vector[String], error: String) =
      copy(errorList:+ errorNode(path, None, Seq("message" → marshaller.stringNode(error))))

    def add(path: Vector[String], error: Throwable) =
      copy(errorList ++ createErrorPaths(path, error))

    def add(path: Vector[String], error: Throwable, position: Option[Position]) =
      copy(errorList :+ errorNode(path, position map singleLocation, handleException(error)))

    def add(other: ErrorRegistry) =
      ErrorRegistry(errorList ++ other.errorList)

    def createErrorPaths(path: Vector[String], e: Throwable) = e match {
      case e: WithViolations if e.violations.nonEmpty ⇒
        e.violations map { v ⇒
          errorNode(path, getLocations(v), Seq("message" → marshaller.stringNode(v.errorMessage)))
        }
      case other ⇒
        errorNode(path, getLocations(other), handleException(other)) :: Nil
    }

    def getLocations(violation: Violation) = violation match {
      case v: AstNodeLocation if v.positions.nonEmpty ⇒
        Some(marshallPositions(v.positions))
      case v ⇒
        None
    }

    def getLocations(error: Throwable) =
      error match {
        case error: AstNodeLocation if error.positions.nonEmpty ⇒ Some(marshallPositions(error.positions))
        case _ ⇒ None
      }

    def marshallPositions(px: List[Position]) =
      marshaller.mapAndMarshal(px, createLocation)


    def createLocation(pos: Position) = marshaller.mapNode(Seq(
      "line" → marshaller.intNode(pos.line),
      "column" → marshaller.intNode(pos.column)))

    def singleLocation(pos: Position) = marshaller.arrayNode(Vector(createLocation(pos)))
  }

  object ErrorRegistry {
    val empty = ErrorRegistry(Vector.empty)

    def apply(path: Vector[String], error: Throwable): ErrorRegistry = empty.add(path, error)
    def apply(path: Vector[String], error: Throwable, pos: Option[Position]): ErrorRegistry = empty.add(path, error, pos)
  }


  def errorNode(path: Vector[String], location: Option[marshaller.Node], errorFields: Seq[(String, marshaller.Node)]) = {
    val names = errorFields.map(_._1)
    val namesWithPath = if (path.nonEmpty) names :+ "field" else names
    val namesWithLoc = if (location.isDefined) namesWithPath :+ "locations" else namesWithPath

    val builder = errorFields.foldLeft(marshaller.emptyMapNode(namesWithLoc)) {
      case (acc, (name, value)) ⇒ marshaller.addMapNodeElem(acc, name, value, optional = false)
    }

    val builderWithPath =
      if (path.nonEmpty)
        marshaller.addMapNodeElem(builder, "field", marshaller.stringNode(path mkString "."), optional = false)
      else
        builder

    location match {
      case Some(node) ⇒ marshaller.mapNode(marshaller.addMapNodeElem(builderWithPath, "locations", node, optional = false))
      case None ⇒ marshaller.mapNode(builderWithPath)
    }
  }
}
