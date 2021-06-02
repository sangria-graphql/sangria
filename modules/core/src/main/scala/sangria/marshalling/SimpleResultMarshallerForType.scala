package sangria.marshalling

case class SimpleResultMarshallerForType[T](marshaller: ResultMarshaller)
    extends ResultMarshallerForType[T]
