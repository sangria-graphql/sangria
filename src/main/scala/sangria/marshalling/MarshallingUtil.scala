package sangria.marshalling

object MarshallingUtil {
  def convert[In: InputUnmarshaller, Out: ResultMarshallerForType](value: In): Out = {
    val iu = implicitly[InputUnmarshaller[In]]
    val rm = implicitly[ResultMarshallerForType[Out]].marshaller

    val converted = value match {
      case nil if !iu.isDefined(nil) ⇒ rm.nullNode
      case map if iu.isMapNode(map) ⇒
        iu.getMapKeys(map).foldLeft(rm.emptyMapNode) {
          case (acc, key) ⇒
            iu.getMapValue(map, key) match {
              case Some(v) ⇒ rm.addMapNodeElem(acc, key, convert(v).asInstanceOf[rm.Node], optional = false)
              case None ⇒ acc
            }
        }
      case list if iu.isListNode(list) ⇒
        rm.mapAndMarshal(iu.getListValue(list), (elem: In) ⇒ convert(elem).asInstanceOf[rm.Node])
      case scalar if iu.isEnumNode(scalar) || iu.isScalarNode(scalar) ⇒
        iu.getScalaScalarValue(scalar) match {
          case s: String ⇒ rm.stringNode(s)
          case i: Int ⇒ rm.intNode(i)
          case l: Long ⇒ rm.bigIntNode(BigInt(l))
          case i: BigInt ⇒ rm.bigIntNode(i)
          case f: Double ⇒ rm.floatNode(f)
          case f: BigDecimal ⇒ rm.bigDecimalNode(f)
          case b: Boolean ⇒ rm.booleanNode(b)
          case v ⇒
            throw new IllegalStateException(s"Unexpected scalar value '$v'!")
        }
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
