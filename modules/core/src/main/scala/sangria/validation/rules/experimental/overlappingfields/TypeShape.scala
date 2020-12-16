package sangria.validation.rules.experimental.overlappingfields

import sangria.renderer.SchemaRenderer
import sangria.schema
import sangria.schema.OutputType

/** A representation of the output shape of a field
  *
  * Used to check uniqueness of the output shape in
  * an overlapping set of fields
  */
sealed trait TypeShape

object TypeShape {

  def apply(outputType: Option[OutputType[_]]): TypeShape =
    outputType match {
      case Some(tpe) => new Known(tpe)
      case None => Unknown
    }

  /** Unknown types are ignored by the validation
    */
  case object Unknown extends TypeShape

  final class Known(private val outputType: OutputType[_]) extends TypeShape {

    private val typeShape: Shape = Shape(outputType)

    override val hashCode: Int =
      typeShape.hashCode()

    override def equals(obj: Any): Boolean =
      obj match {
        case other: Known => typeShape == other.typeShape
        case _ => false
      }

    def conflictReason(other: Known): String =
      if (typeShape != other.typeShape) {
        s"they return conflicting types '${renderType()}' and '${other.renderType()}'"
      } else {
        throw new IllegalArgumentException("no conflict between keys")
      }

    private def renderType(): String =
      SchemaRenderer.renderTypeName(outputType)
  }

  sealed trait Shape

  object Shape {

    def apply(tpe: OutputType[_]): Shape =
      tpe match {
        case schema.ListType(ofType) => ListShape(Shape(ofType))
        case schema.OptionType(ofType) => OptionShape(Shape(ofType))
        case leaf: schema.LeafType => LeafShape(leaf.name)
        case _: schema.CompositeType[_] => CompositeShape
      }

    case class ListShape(tpe: Shape) extends Shape

    case class OptionShape(tpe: Shape) extends Shape

    case class LeafShape(name: String) extends Shape

    /** Composite types do not need to match, as we require that the individual selected
      * fields match recursively.
      */
    case object CompositeShape extends Shape

  }

}
