package sangria.schema

import sangria.execution.{Resolver, Trinary, ValueCoercionHelper}
import sangria.marshalling.{CoercedScalaResultMarshaller, QueryAstResultMarshaller, ToInput}

object DefaultValueRenderer {
  implicit val marshaller: QueryAstResultMarshaller =
    sangria.marshalling.queryAst.queryAstResultMarshaller

  def renderInputValueCompact[T, Ctx](
      value: (_, ToInput[_, _]),
      tpe: InputType[T],
      coercionHelper: ValueCoercionHelper[Ctx]): Option[String] =
    renderInputValue(value, tpe, coercionHelper).map(marshaller.renderCompact)

  def renderInputValuePretty[T, Ctx](
      value: (_, ToInput[_, _]),
      tpe: InputType[T],
      coercionHelper: ValueCoercionHelper[Ctx]): Option[String] =
    renderInputValue(value, tpe, coercionHelper).map(marshaller.renderPretty)

  def renderInputValue[T, Ctx](
      value: (_, ToInput[_, _]),
      tpe: InputType[T],
      coercionHelper: ValueCoercionHelper[Ctx]): Option[marshaller.Node] = {
    val (v, toInput) = value.asInstanceOf[(Any, ToInput[Any, Any])]
    val (inputValue, iu) = toInput.toInput(v)

    if (!iu.isDefined(inputValue))
      None
    else
      coercionHelper.coerceInputValue(
        tpe,
        Nil,
        inputValue,
        None,
        None,
        CoercedScalaResultMarshaller.default,
        CoercedScalaResultMarshaller.default,
        isArgument = false)(iu) match {
        case Right(Trinary.Defined(coerced)) => Some(renderCoercedInputValue(tpe, coerced))
        case _ => None
      }
  }

  def renderCoercedInputValueCompact[T](value: Any, tpe: InputType[T]): String =
    marshaller.renderCompact(renderCoercedInputValue(tpe, value))

  def renderCoercedInputValuePretty[T](value: Any, tpe: InputType[T]): String =
    marshaller.renderPretty(renderCoercedInputValue(tpe, value))

  def renderCoercedInputValue(t: InputType[_], v: Any): marshaller.Node = t match {
    case _ if v == null => marshaller.nullNode
    case s: ScalarType[Any @unchecked] =>
      Resolver.marshalScalarValue(
        s.coerceOutput(v, marshaller.capabilities),
        marshaller,
        s.name,
        s.scalarInfo)
    case s: ScalarAlias[Any @unchecked, Any @unchecked] =>
      renderCoercedInputValue(s.aliasFor, s.toScalar(v))
    case e: EnumType[Any @unchecked] =>
      Resolver.marshalEnumValue(e.coerceOutput(v), marshaller, e.name)
    case io: InputObjectType[_] =>
      val mapValue = v.asInstanceOf[Map[String, Any]]

      val builder = io.fields.foldLeft(marshaller.emptyMapNode(io.fields.map(_.name))) {
        case (acc, field) if mapValue contains field.name =>
          marshaller.addMapNodeElem(
            acc,
            field.name,
            renderCoercedInputValue(field.fieldType, mapValue(field.name)),
            optional = false)
        case (acc, _) => acc
      }

      marshaller.mapNode(builder)
    case l: ListInputType[_] =>
      val listValue = v.asInstanceOf[Seq[Any]]

      marshaller.mapAndMarshal[Any](listValue, renderCoercedInputValue(l.ofType, _))
    case o: OptionInputType[_] =>
      v match {
        case Some(optVal) => renderCoercedInputValue(o.ofType, optVal)
        case None => marshaller.nullNode
        case other => renderCoercedInputValue(o.ofType, other)
      }
  }
}
