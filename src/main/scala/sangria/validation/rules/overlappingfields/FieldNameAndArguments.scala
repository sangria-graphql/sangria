package sangria.validation.rules.overlappingfields

import java.util
import java.util.Objects

import sangria.ast
import sangria.renderer.QueryRenderer

/**
  * A hashable representation of field name and arguments,
  * used to check uniqueness of them in a set of fields
  *
  * Two fields also have the same name and arguments when
  * the order of arguments differs.
  */
final class FieldNameAndArguments(private val field: ast.Field) {

  private val fieldName: String = field.name
  private val arguments: util.ArrayList[(String, String)] = argumentsKey(field.arguments)

  override val hashCode: Int = {
    Objects.hash(fieldName, arguments)
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: FieldNameAndArguments => fieldName == other.fieldName && arguments == other.arguments
      case _                            => false
    }
  }

  def conflictReason(other: FieldNameAndArguments): String = {
    if (fieldName != other.fieldName) {
      s"'$fieldName' and '${other.fieldName}' are different fields"
    } else if (arguments != other.arguments) {
      "of differing arguments"
    } else {
      throw new IllegalArgumentException("no conflict between keys")
    }
  }

  private def argumentsKey(arguments: Vector[ast.Argument]): util.ArrayList[(String, String)] = {
    val key = new util.ArrayList[(String, String)](arguments.size)
    arguments.foreach { argument =>
      key.add(argument.name -> QueryRenderer.render(argument.value, QueryRenderer.Compact))
    }
    key.sort((a: (String, String), b: (String, String)) => a._1.compareTo(b._1))
    key
  }
}
