package sangria.execution

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
      case None => empty
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
    def add(other: ErrorRegistry) = ErrorRegistry(errorList ++ other.errorList)

    def getLocations(error: Throwable) = {
      val astNodeLocations = error match {
        case error: AstNodeLocation if error.position.isDefined =>
          marshaller.addArrayNodeElem(marshaller.emptyArrayNode, marshaller.mapNode(Seq(
            "line" -> marshaller.intNode(error.position.get.line),
            "column" -> marshaller.intNode(error.position.get.column))))
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
      }

      if (marshaller.isEmptyArrayNode(violatoinErrors)) None else Some(violatoinErrors)
    }
  }

  object ErrorRegistry {
    val empty = ErrorRegistry(Nil)
    def apply(path: List[String], error: Throwable): ErrorRegistry = empty.add(path, error)
  }

  case class ErrorPath(path: List[String], error: marshaller.Node, location: Option[marshaller.Node])
}
