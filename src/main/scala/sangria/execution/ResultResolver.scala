package sangria.execution

import org.parboiled2.Position
import sangria.ast.AstNode
import sangria.integration.ResultMarshaller
import sangria.validation.{Violation, AstNodeLocation}

import scala.collection.immutable.VectorBuilder

class ResultResolver(val marshaller: ResultMarshaller, exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException]) {
  def marshalErrors(errors: ErrorRegistry) = {
    val marshalled = errors.errorList.map {
      case ErrorPath(path, error, location) ⇒
        val withPath =
          if (path.nonEmpty)
            marshaller.addMapNodeElem(error, "field", marshaller.stringNode(path mkString "."))
          else
            error

        location match {
          case Some(node) ⇒ marshaller.addMapNodeElem(withPath, "locations", node)
          case None ⇒ withPath
        }
    }

    if (marshalled.isEmpty) None else Some(marshaller.arrayNode(marshalled))
  }


  def marshalResult(data: Option[marshaller.Node], errors: Option[marshaller.Node]) = {
    val empty = marshaller.emptyMapNode

    val withData = data match {
      case Some(d) ⇒ marshaller.addMapNodeElem(empty, "data", d)
      case None ⇒ marshaller.addMapNodeElem(empty, "data", marshaller.nullNode)
    }

    errors match {
      case Some(e) ⇒ marshaller.addMapNodeElem(withData, "errors", e)
      case None ⇒ withData
    }
  }

  def resolveError(error: Throwable) =
    marshalResult(None, marshalErrors(ErrorRegistry(Vector.empty, error)))

  def handleException(exception: Throwable): marshaller.Node = exception match {
    case e: UserFacingError ⇒
      marshaller.mapNode(Seq("message" → marshaller.stringNode(e.getMessage)))
    case e if exceptionHandler isDefinedAt (marshaller → e) ⇒
      val handeled = exceptionHandler(marshaller → e)

      marshaller.mapNode(Seq("message" → marshaller.stringNode(handeled.message)) ++ handeled.additionalFields.toSeq.asInstanceOf[Seq[(String, marshaller.Node)]])
    case e ⇒
      e.printStackTrace() // todo proper logging?
      marshaller.mapNode(Seq("message" → marshaller.stringNode("Internal server error")))
  }

  case class ErrorRegistry(errorList: Vector[ErrorPath]) {
    def add(path: Vector[String], error: String) =
      copy(errorList:+ ErrorPath(path, marshaller.mapNode(Seq("message" → marshaller.stringNode(error))), None))

    def add(path: Vector[String], error: Throwable) =
      copy(errorList ++ createErrorPaths(path, error))

    def add(path: Vector[String], error: Throwable, position: Option[Position]) =
      copy(errorList :+ ErrorPath(path, handleException(error), position map singleLocation))

    def add(other: ErrorRegistry) =
      ErrorRegistry(errorList ++ other.errorList)

    def createErrorPaths(path: Vector[String], e: Throwable) = e match {
      case e: WithViolations if e.violations.nonEmpty ⇒
        e.violations map { v ⇒
          ErrorPath(path, marshaller.mapNode(Seq("message" → marshaller.stringNode(v.errorMessage))), getLocations(v))
        }
      case other ⇒
        ErrorPath(path, handleException(other), getLocations(other)) :: Nil
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

  case class ErrorPath(path: Vector[String], error: marshaller.Node, location: Option[marshaller.Node])
}
