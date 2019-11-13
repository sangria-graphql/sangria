package sangria.validation.rules.overlappingfields

import sangria.schema.{CompositeType, ObjectType}

sealed trait TypeAbstractness

object TypeAbstractness {

  def apply(parentType: Option[CompositeType[_]]): TypeAbstractness = {
    parentType match {
      case Some(obt: ObjectType[_, _]) => Concrete(obt.name)
      case _                           => Abstract
    }
  }

  /**
    * For the purpose of grouping types for the validation,
    * we consider abstract types to constitute one group
    */
  case object Abstract extends TypeAbstractness

  case class Concrete(private val name: String) extends TypeAbstractness

}
