package sangria.marshalling

trait MarshallerCapability

sealed trait StandardMarshallerCapability extends MarshallerCapability

/** Marshaller supports `java.util.Date` natively.
  */
case object DateSupport extends StandardMarshallerCapability

/** Marshaller supports `java.util.Calendar` natively.
  */
case object CalendarSupport extends StandardMarshallerCapability

/** Marshaller supports large binary objects in form of `Array[Byte]` natively.
  */
case object BlobSupport extends StandardMarshallerCapability
