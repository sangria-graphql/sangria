package sangria.marshalling

// copy/pasted upd-to-date version from sangria-marshalling-api (TODO: remove as soon as it is released)
object ImprovedMarshallingUtil {
  def convert[In: InputUnmarshaller, Out: ResultMarshallerForType](value: In): Out = {
    val iu = implicitly[InputUnmarshaller[In]]
    val rm = implicitly[ResultMarshallerForType[Out]].marshaller

    val converted = value match {
      case nil if !iu.isDefined(nil) ⇒ rm.nullNode
      case map if iu.isMapNode(map) ⇒
        val keys = iu.getMapKeys(map)
        val builder = keys.foldLeft(rm.emptyMapNode(keys.toSeq)) {
          case (acc, key) ⇒
            iu.getMapValue(map, key) match {
              case Some(v) ⇒ rm.addMapNodeElem(acc, key, convert(v).asInstanceOf[rm.Node], optional = false)
              case None ⇒ acc
            }
        }

        rm.mapNode(builder)
      case list if iu.isListNode(list) ⇒
        rm.mapAndMarshal(iu.getListValue(list), (elem: In) ⇒ convert(elem).asInstanceOf[rm.Node])
      case enum if iu.isEnumNode(enum) && iu.getScalaScalarValue(enum).isInstanceOf[String] ⇒
        rm.enumNode(iu.getScalaScalarValue(enum).asInstanceOf[String], "Conversion")
      case scalar if iu.isScalarNode(scalar) ⇒
        rm.scalarNode(iu.getScalaScalarValue(scalar), "Conversion", Set.empty)
      case variable if iu.isVariableNode(variable) ⇒
        throw new IllegalArgumentException(s"Variable '${iu.getVariableName(value)}' found in the input, but variables are not supported in conversion!")
      case node ⇒
        throw new IllegalStateException(s"Unexpected node '$node'!")
    }

    converted.asInstanceOf[Out]
  }              

  implicit class MarshaledConverter[In : InputUnmarshaller](in: In) {
    def convertMarshaled[Out : ResultMarshallerForType] = convert(in)
  }
}
