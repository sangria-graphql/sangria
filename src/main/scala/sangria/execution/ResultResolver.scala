package sangria.execution

import org.parboiled2.Position
import sangria.ast.AstNode
import sangria.validation.AstNodeLocation

class ResultResolver(val marshaller: ResultMarshaller, exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node]) {
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
    case e: UserFacingError => marshaller.mapNode(Seq("message" -> marshaller.stringNode(e.getMessage)))
    case e if exceptionHandler isDefinedAt (marshaller -> e) => exceptionHandler(marshaller -> e).asInstanceOf[marshaller.Node]
    case e =>
      e.printStackTrace() // todo proper logging?
      marshaller.mapNode(Seq("message" -> marshaller.stringNode("Internal server error")))
  }

  case class ErrorRegistry(errorList: List[ErrorPath]) {
    def add(path: List[String], error: String) = copy(errorList:+ ErrorPath(path, marshaller.mapNode(Seq("message" -> marshaller.stringNode(error))), None))
    def add(path: List[String], error: Throwable) = copy(errorList :+ ErrorPath(path, handleException(error), getLocations(error)))
    def add(path: List[String], error: Throwable, position: Option[Position]) = copy(errorList :+ ErrorPath(path, handleException(error), position map singleLocation))
    def add(other: ErrorRegistry) = ErrorRegistry(errorList ++ other.errorList)

    def getLocations(error: Throwable) = {
      val astNodeLocations = error match {
        case error: AstNodeLocation if error.position.isDefined =>
          marshaller.addArrayNodeElem(marshaller.emptyArrayNode, createLocation(error.position.get))
        case _ => marshaller.emptyArrayNode
      }

      val violatoinErrors = error match {
        case error: WithViolations if error.violations.nonEmpty =>
          error.violations.foldLeft(astNodeLocations) {
            case (acc, v: AstNodeLocation) if v.position.isDefined =>
              marshaller.addArrayNodeElem(acc, marshaller.mapNode(Seq(
                "line" -> marshaller.intNode(v.position.get.line),
                "column" -> marshaller.intNode(v.position.get.column),
                "message" -> marshaller.stringNode(v.errorMessage))))
            case (acc, _) => acc
          }
        case _ => astNodeLocations
      }

      if (marshaller.isEmptyArrayNode(violatoinErrors)) None else Some(violatoinErrors)
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
