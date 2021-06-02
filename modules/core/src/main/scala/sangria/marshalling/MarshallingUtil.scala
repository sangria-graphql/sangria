package sangria.marshalling

object MarshallingUtil {
  def convert[In: InputUnmarshaller, Out: ResultMarshallerForType](value: In): Out = {
    val iu = implicitly[InputUnmarshaller[In]]
    val rm = implicitly[ResultMarshallerForType[Out]].marshaller

    val converted = value match {
      case nil if !iu.isDefined(nil) => rm.nullNode
      case map if iu.isMapNode(map) =>
        val keys = iu.getMapKeys(map)
        val builder = keys.foldLeft(rm.emptyMapNode(keys.toSeq)) { case (acc, key) =>
          iu.getMapValue(map, key) match {
            case Some(v) =>
              rm.addMapNodeElem(acc, key, convert(v).asInstanceOf[rm.Node], optional = false)
            case None => acc
          }
        }

        rm.mapNode(builder)
      case list if iu.isListNode(list) =>
        rm.mapAndMarshal(iu.getListValue(list), (elem: In) => convert(elem).asInstanceOf[rm.Node])
      case enums if iu.isEnumNode(enums) && iu.getScalaScalarValue(enums).isInstanceOf[String] =>
        rm.enumNode(iu.getScalaScalarValue(enums).asInstanceOf[String], "Conversion")
      case scalar if iu.isScalarNode(scalar) =>
        rm.scalarNode(iu.getScalaScalarValue(scalar), "Conversion", Set.empty)
      case variable if iu.isVariableNode(variable) =>
        throw new IllegalArgumentException(
          s"Variable '${iu.getVariableName(value)}' found in the input, but variables are not supported in conversion!")
      case node =>
        throw new IllegalStateException(s"Unexpected node '$node'!")
    }

    converted.asInstanceOf[Out]
  }

  implicit class MarshaledConverter[In: InputUnmarshaller](in: In) {
    def convertMarshaled[Out: ResultMarshallerForType] = convert(in)
  }

  implicit class ResultMarshallerOps(val m: ResultMarshaller) extends AnyVal {
    def list(elements: ResultMarshaller#Node*): m.Node =
      m.arrayNode(elements.asInstanceOf[Seq[m.Node]].toVector)

    def map(elements: (String, ResultMarshaller#Node)*): m.Node =
      m.mapNode(elements.foldLeft(m.emptyMapNode(elements.map(_._1))) { case (acc, (name, value)) =>
        m.addMapNodeElem(acc, name, value.asInstanceOf[m.Node], optional = false)
      })

    def fromString(value: String): m.Node =
      m.scalarNode(value, "String", Set.empty)

    def fromEnumString(value: String): m.Node =
      m.enumNode(value, "")

    def fromInt(value: Int): m.Node =
      m.scalarNode(value, "Int", Set.empty)

    def fromLong(value: Long): m.Node =
      m.scalarNode(value, "Long", Set.empty)

    def fromBoolean(value: Boolean): m.Node =
      m.scalarNode(value, "Long", Set.empty)

    def fromFloat(value: Float): m.Node =
      m.scalarNode(value, "Float", Set.empty)

    def fromDouble(value: Double): m.Node =
      m.scalarNode(value, "Float", Set.empty)

    def fromBigInt(value: BigInt): m.Node =
      m.scalarNode(value, "BigInt", Set.empty)

    def fromBigInt(value: BigDecimal): m.Node =
      m.scalarNode(value, "BigDecimal", Set.empty)

    def fromInput[Input: InputUnmarshaller](value: Input): m.Node =
      value.convertMarshaled(SimpleResultMarshallerForType[m.Node](m))
  }
}
