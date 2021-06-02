package sangria.marshalling

case class InputParsingError(errors: Vector[String]) extends Exception(errors.mkString("\n"))
