package sangria.execution

case class ExecutionError(message: String) extends Exception(message)
