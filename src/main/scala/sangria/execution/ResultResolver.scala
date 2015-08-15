package sangria.execution

import org.parboiled2.Position
import sangria.ast.AstNode
import sangria.validation.{Violation, AstNodeLocation}

class ResultResolver(val marshaller: ResultMarshaller, exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException]) {
  def marshalErrors(errors: ErrorRegistry) = {
    val marshalled = errors.errorList.foldLeft(marshaller.emptyArrayNode) {
      case (acc, ErrorPath(path, error, location)) =>
        val withPath =
          if (path.nonEmpty)
            marshaller.addMapNodeElem(error, "field", marshaller.stringNode(path mkString "."))
          else
            error

        val withLocations = location match {
          case Some(node) => marshaller.addMapNodeElem(withPath, "locations", node)
          case None => withPath
        }

        marshaller.addArrayNodeElem(acc, withLocations)
    }

    if (marshaller.isEmptyArrayNode(marshalled)) None else Some(marshalled)
  }


  def marshalResult(data: Option[marshaller.Node], errors: Option[marshaller.Node]) = {
    val empty = marshaller.emptyMapNode

    val withData = data match {
      case Some(d) => marshaller.addMapNodeElem(empty, "data", d)
      case None => marshaller.addMapNodeElem(empty, "data", marshaller.nullNode)
    }

    errors match {
      case Some(e) => marshaller.addMapNodeElem(withData, "errors", e)
      case None => withData
    }
  }

  def resolveError(error: Throwable) =
    marshalResult(None, marshalErrors(ErrorRegistry(Nil, error)))

  def handleException(exception: Throwable): marshaller.Node = exception match {
    case e: UserFacingError =>
      marshaller.mapNode(Seq("message" -> marshaller.stringNode(e.getMessage)))
    case e if exceptionHandler isDefinedAt (marshaller -> e) =>
      val handeled = exceptionHandler(marshaller -> e)

      marshaller.mapNode(Seq("message" -> marshaller.stringNode(handeled.message)) ++ handeled.additionalFields.toSeq.asInstanceOf[Seq[(String, marshaller.Node)]])
    case e =>
      e.printStackTrace() // todo proper logging?
      marshaller.mapNode(Seq("message" -> marshaller.stringNode("Internal server error")))
  }

  case class ErrorRegistry(errorList: List[ErrorPath]) {
    def add(path: List[String], error: String) =
      copy(errorList:+ ErrorPath(path, marshaller.mapNode(Seq("message" -> marshaller.stringNode(error))), None))

    def add(path: List[String], error: Throwable) =
      copy(errorList ++ createErrorPaths(path, error))

    def add(path: List[String], error: Throwable, position: Option[Position]) =
      copy(errorList :+ ErrorPath(path, handleException(error), position map singleLocation))

    def add(other: ErrorRegistry) =
      ErrorRegistry(errorList ++ other.errorList)

    def createErrorPaths(path: List[String], e: Throwable) = e match {
      case e: WithViolations if e.violations.nonEmpty =>
        e.violations map { v =>
          ErrorPath(path, marshaller.mapNode(Seq("message" -> marshaller.stringNode(v.errorMessage))), getLocations(v))
        }
      case other =>
        ErrorPath(path, handleException(other), getLocations(other)) :: Nil
    }

    def getLocations(violation: Violation) = violation match {
      case v: AstNodeLocation =>
        val violationLocations = marshallPositions(v.positions)

        if (marshaller.isEmptyArrayNode(violationLocations)) None else Some(violationLocations)
      case v =>
        None
    }

    def getLocations(error: Throwable) = {
      val astNodeLocations = error match {
        case error: AstNodeLocation => marshallPositions(error.positions)
        case _ => marshaller.emptyArrayNode
      }

      if (marshaller.isEmptyArrayNode(astNodeLocations)) None else Some(astNodeLocations)
    }

    def marshallPositions(px: List[Position]) =
      px.foldLeft(marshaller.emptyArrayNode) {
        case (acc, p) => marshaller.addArrayNodeElem(acc, createLocation(p))
      }

    def createLocation(pos: Position) = marshaller.mapNode(Seq(
      "line" -> marshaller.intNode(pos.line),
      "column" -> marshaller.intNode(pos.column)))

    def singleLocation(pos: Position) = marshaller.arrayNode(Seq(createLocation(pos)))
  }

  object ErrorRegistry {
    val empty = ErrorRegistry(Nil)
    def apply(path: List[String], error: Throwable): ErrorRegistry = empty.add(path, error)
    def apply(path: List[String], error: Throwable, pos: Option[Position]): ErrorRegistry = empty.add(path, error, pos)
  }

  case class ErrorPath(path: List[String], error: marshaller.Node, location: Option[marshaller.Node])
}
