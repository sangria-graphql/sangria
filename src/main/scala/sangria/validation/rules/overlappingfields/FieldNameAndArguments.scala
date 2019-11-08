package sangria.validation.rules.overlappingfields

import java.util.Objects

import sangria.ast
import sangria.renderer.QueryRenderer

import scala.collection.mutable

/**
  * A hashable representation of field name and arguments,
  * used to check uniqueness of them in a set of fields
  *
  * Two fields also have the same name and arguments when
  * the order of arguments differs.
  */
final class FieldNameAndArguments(private val field: ast.Field) {

  private val fieldName: String = field.name
  private val arguments: mutable.WrappedArray[(String, String)] = argumentsKey(field.arguments)

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

  private def argumentsKey(arguments: Vector[ast.Argument]): mutable.WrappedArray[(String, String)] = {
    val key: Array[(String, String)] = arguments.view.map{ argument =>
      argument.name -> QueryRenderer.render(argument.value, QueryRenderer.Compact)
    }.toArray
    util.Sorting.quickSort(key)(Ordering.by(_._1))
    key
  }
}
