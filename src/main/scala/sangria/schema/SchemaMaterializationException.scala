package sangria.schema

case class SchemaMaterializationException(message: String, cause: Throwable = null) extends Exception(message, cause)
case class InputMaterializationException(message: String) extends Exception(message)