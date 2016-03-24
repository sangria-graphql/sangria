package sangria.introspection

import sangria.marshalling.InputUnmarshaller
import sangria.parser.DeliveryScheme
import sangria.schema.DirectiveLocation

object IntrospectionParser {
  def parse[In](introspectionResult: In)(implicit iu: InputUnmarshaller[In], scheme: DeliveryScheme[IntrospectionSchema]): scheme.Result =
    try {
      checkErrors(introspectionResult)

      scheme.success(parseSchema(mapField(mapField(introspectionResult, "data"), "__schema", Vector("data")), Vector("data", "__schema")))
    } catch { // exception mechanism is used intentionally in order to minimise the footprint of parsing
      case e: IllegalAccessException ⇒ scheme.failure(e)
    }

  private def parseInputValue[In : InputUnmarshaller](value: In, path: Vector[String]) =
    IntrospectionInputValue(
      name = mapStringField(value, "name", path),
      description = mapStringFieldOpt(value, "description"),
      tpe = parseTypeRef(mapField(value, "type", path), path :+ "type"),
      defaultValue = mapStringFieldOpt(value, "defaultValue"))

  private def parseField[In : InputUnmarshaller](field: In, path: Vector[String]) =
    IntrospectionField(
      name = mapStringField(field, "name", path),
      description = mapStringFieldOpt(field, "description"),
      args = mapFieldOpt(field, "args") map um.getListValue getOrElse Vector.empty map (arg ⇒ parseInputValue(arg, path :+ "args")),
      tpe = parseTypeRef(mapField(field, "type", path), path :+ "type"),
      isDeprecated = mapBooleanField(field, "isDeprecated", path),
      deprecationReason = mapStringFieldOpt(field, "deprecationReason"))

  private def parseObject[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    IntrospectionObjectType(
      name = mapStringField(tpe, "name", path),
      description = mapStringFieldOpt(tpe, "description", path),
      fields = mapFieldOpt(tpe, "fields") map um.getListValue getOrElse Vector.empty map (field ⇒ parseField(field, path :+ "fields")),
      interfaces = mapFieldOpt(tpe, "interfaces") map um.getListValue getOrElse Vector.empty  map (i ⇒ parseNamedTypeRef(i, path :+ "interfaces"))
    )

  private def parseInterface[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    IntrospectionInterfaceType(
      name = mapStringField(tpe, "name", path),
      description = mapStringFieldOpt(tpe, "description", path),
      fields = mapFieldOpt(tpe, "fields") map um.getListValue getOrElse Vector.empty map (field ⇒ parseField(field, path :+ "fields")),
      possibleTypes = mapFieldOpt(tpe, "possibleTypes") map um.getListValue getOrElse Vector.empty  map (i ⇒ parseNamedTypeRef(i, path :+ "possibleTypes"))
    )

  private def parseUnion[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    IntrospectionUnionType(
      name = mapStringField(tpe, "name", path),
      description = mapStringFieldOpt(tpe, "description", path),
      possibleTypes = mapFieldOpt(tpe, "possibleTypes") map um.getListValue getOrElse Vector.empty  map (i ⇒ parseNamedTypeRef(i, path :+ "possibleTypes"))
    )

  private def parseInputObject[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    IntrospectionInputObjectType(
      name = mapStringField(tpe, "name", path),
      description = mapStringFieldOpt(tpe, "description", path),
      inputFields = mapFieldOpt(tpe, "inputFields") map um.getListValue getOrElse Vector.empty map (arg ⇒ parseInputValue(arg, path :+ "inputFields")))

  private def parseScalar[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    IntrospectionScalarType(
      name = mapStringField(tpe, "name", path),
      description = mapStringFieldOpt(tpe, "description", path))

  private def parseEnumValue[In : InputUnmarshaller](value: In, path: Vector[String]) =
    IntrospectionEnumValue(
      name = mapStringField(value, "name", path),
      description = mapStringFieldOpt(value, "description", path),
      isDeprecated = mapBooleanField(value, "isDeprecated", path),
      deprecationReason = mapStringFieldOpt(value, "deprecationReason"))

  private def parseEnum[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    IntrospectionEnumType(
      name = mapStringField(tpe, "name", path),
      description = mapStringFieldOpt(tpe, "description", path),
      enumValues = um.getListValue(mapField(tpe, "enumValues", path)) map (parseEnumValue(_, path :+ "enumValues")))

  private def parseDirective[In : InputUnmarshaller](directive: In, path: Vector[String]) =
    IntrospectionDirective(
      name = mapStringField(directive, "name", path),
      description = mapStringFieldOpt(directive, "description"),
      locations = um.getListValue(mapField(directive, "locations")).map(v ⇒ DirectiveLocation.fromString(stringValue(v, path :+ "locations"))).toSet,
      args = mapFieldOpt(directive, "args") map um.getListValue getOrElse Vector.empty map (arg ⇒ parseInputValue(arg, path :+ "args")))

  private def parseType[In : InputUnmarshaller](tpe: In, path: Vector[String]) =
    mapStringField(tpe, "kind", path) match {
      case "OBJECT" ⇒ parseObject(tpe, path)
      case "UNION" ⇒ parseUnion(tpe, path)
      case "INTERFACE" ⇒ parseInterface(tpe, path)
      case "INPUT_OBJECT" ⇒ parseInputObject(tpe, path)
      case "SCALAR" ⇒ parseScalar(tpe, path)
      case "ENUM" ⇒ parseEnum(tpe, path)
      case kind ⇒ error(s"Unsupported kind: $kind")
    }

  private def parseSchema[In : InputUnmarshaller](schema: In, path: Vector[String]) =
    IntrospectionSchema(
      queryType = parseNamedTypeRef(mapField(schema, "queryType", path), path :+ "queryType"),
      mutationType = mapFieldOpt(schema, "mutationType") map (parseNamedTypeRef(_, path :+ "mutationType")),
      subscriptionType = mapFieldOpt(schema, "subscriptionType") map (parseNamedTypeRef(_, path :+ "subscriptionType")),
      types = um.getListValue(mapField(schema, "types", path)) map (parseType(_, path :+ "types")),
      directives = mapFieldOpt(schema, "directives") map um.getListValue getOrElse Vector.empty map (i ⇒ parseDirective(i, path :+ "directives")))

  private def parseNamedTypeRef[In : InputUnmarshaller](in: In, path: Vector[String]) =
    IntrospectionNamedTypeRef(mapStringFieldOpt(in, "kind", path) map TypeKind.fromString getOrElse TypeKind.Object, mapStringField(in, "name", path))

  private def parseTypeRef[In : InputUnmarshaller](in: In, path: Vector[String]): IntrospectionTypeRef =
    mapStringField(in, "kind", path) match {
      case "LIST" ⇒ IntrospectionListTypeRef(parseTypeRef(mapField(in, "ofType", path), path :+ "ofType"))
      case "NON_NULL" ⇒ IntrospectionNonNullTypeRef(parseTypeRef(mapField(in, "ofType", path), path :+ "ofType"))
      case _ ⇒ parseNamedTypeRef(in, path)
    }

  private def required[T](obj: Option[T], path: Vector[String]) = obj match {
    case Some(o) ⇒ o
    case None ⇒ error(s"Required property is missing at path: ${path mkString "."}")
  }

  private def checkErrors[In : InputUnmarshaller](introspectionResult: In): Unit =
    um.getRootMapValue(introspectionResult, "errors") match {
      case Some(errors) ⇒
        throw new IllegalArgumentException(
          s"Can't parse introspection results because it contains errors: ${um.render(errors)}")
      case None ⇒ // everything is fine
    }

  private def stringValue[In : InputUnmarshaller](value: In, path: Vector[String]) =
    um.getScalaScalarValue(value) match {
      case s: String ⇒ s
      case _ ⇒ error(s"Expected String but got '${um.render(value)}' at path ${path mkString "."}")
    }

  private def booleanValue[In : InputUnmarshaller](value: In, path: Vector[String]) =
    um.getScalaScalarValue(value) match {
      case b: Boolean ⇒ b
      case _ ⇒ error(s"Expected Boolean but got '${um.render(value)}' at path ${path mkString "."}")
    }

  private def mapField[In : InputUnmarshaller](map: In, name: String, path: Vector[String] = Vector.empty): In =
    required(um.getMapValue(map, name), path :+ name)

  private def mapStringField[In : InputUnmarshaller](map: In, name: String, path: Vector[String] = Vector.empty): String =
    stringValue(mapField(map, name, path), path :+ name)

  private def mapBooleanField[In : InputUnmarshaller](map: In, name: String, path: Vector[String] = Vector.empty): Boolean =
    booleanValue(mapField(map, name, path), path :+ name)

  private def mapFieldOpt[In : InputUnmarshaller](map: In, name: String): Option[In] =
    um.getMapValue(map, name) filter um.isDefined

  private def mapStringFieldOpt[In : InputUnmarshaller](map: In, name: String, path: Vector[String] = Vector.empty): Option[String] =
    mapFieldOpt(map, name) filter um.isDefined map (s ⇒ stringValue(s, path :+ name) )

  private def um[T: InputUnmarshaller] = implicitly[InputUnmarshaller[T]]

  private def error(message: String) = throw new IllegalArgumentException(message)
}
