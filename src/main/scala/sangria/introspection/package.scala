package sangria

import sangria.parser.QueryParser
import sangria.schema._

import scala.util.Success

package object introspection {
  object TypeKind extends Enumeration {
    val Scalar, Object, Interface, Union, Enum, InputObject, List, NonNull = Value
  }

  val __TypeKind = EnumType("__TypeKind", Some("An enum describing what kind of type a given __Type is."), List(
    EnumValue("SCALAR", value = TypeKind.Scalar, description = Some("Indicates this type is a scalar.")),
    EnumValue("OBJECT", value = TypeKind.Object, description = Some(
      "Indicates this type is an object. " +
      "`fields` and `interfaces` are valid fields.")),
    EnumValue("INTERFACE", value = TypeKind.Interface, description = Some(
      "Indicates this type is an interface. " +
      "`fields` and `possibleTypes` are valid fields.")),
    EnumValue("UNION", value = TypeKind.Union, description = Some(
      "Indicates this type is a union. " +
      "`possibleTypes` is a valid field.")),
    EnumValue("ENUM", value = TypeKind.Enum, description = Some(
      "Indicates this type is an enum. " +
      "`enumValues` is a valid field.")),
    EnumValue("INPUT_OBJECT", value = TypeKind.InputObject, description = Some(
      "Indicates this type is an input object. " +
      "`inputFields` is a valid field.")),
    EnumValue("LIST", value = TypeKind.List, description = Some(
      "Indicates this type is a list. " +
      "`ofType` is a valid field.")),
    EnumValue("NON_NULL", value = TypeKind.NonNull, description = Some(
      "Indicates this type is a non-null. " +
      "`ofType` is a valid field."))
  ))

  val __Field = ObjectType("__Field", () => List[Field[Unit, Field[_, _]]](
    Field("name", StringType, resolve = _.value.name),
    Field("description", OptionType(StringType), resolve = _.value.description),
    Field("args", ListType(__InputValue), resolve = _.value.arguments),
    Field("type", __Type, resolve = false -> _.value.fieldType),
    Field("isDeprecated", BooleanType, resolve = _.value.deprecationReason.isDefined),
    Field("deprecationReason", OptionType(StringType), resolve = _.value.deprecationReason)
  ))

  val includeDeprecated = Argument("includeDeprecated", OptionInputType(BooleanType), false)

  val __Type: ObjectType[Unit, (Boolean, Type)] = ObjectType("__Type", () => List[Field[Unit, (Boolean, Type)]](
    Field("kind", __TypeKind, resolve = ctx => {
      def identifyKind(t: Type, optional: Boolean): TypeKind.Value = t match {   // todo: something is not quite working here yet...
        case OptionType(ofType) => identifyKind(ofType, true)
        case OptionInputType(ofType) => identifyKind(ofType, true)
        case _ if !optional => TypeKind.NonNull
        case _: ScalarType[_] => TypeKind.Scalar
        case _: ObjectType[_, _] => TypeKind.Object
        case _: InterfaceType[_, _] => TypeKind.Interface
        case _: UnionType[_] => TypeKind.Union
        case _: EnumType[_] => TypeKind.Enum
        case _: InputObjectType[_] => TypeKind.InputObject
        case _: ListType[_] | _: ListInputType[_] => TypeKind.List
      }

      val (fromTypeList, tpe) = ctx.value

      identifyKind(tpe, fromTypeList)
    }),
    Field("name", OptionType(StringType), resolve = x => x.value._2 match {
      case named: Named => Some(named.name)
      case _ => None
    }),
    Field("description", OptionType(StringType), resolve = _.value._2 match {
      case named: Named => named.description
      case _ => None
    }),
    Field("fields", OptionType(ListType(__Field)),
      arguments = includeDeprecated :: Nil,
      resolve = ctx => {
        val incDep = ctx.arg(includeDeprecated)
        val (_, tpe) = ctx.value

        tpe match {
          case t: ObjectLikeType[_, _] if incDep => Some(t.fields.asInstanceOf[List[Field[_, _]]])
          case t: ObjectLikeType[_, _] => Some(t.fields.asInstanceOf[List[Field[_, _]]].filter(_.deprecationReason.isEmpty))
          case _ => None
        }
      }),
    Field("interfaces", OptionType(ListType(__Type)), resolve = _.value._2 match {
      case t: ObjectLikeType[_, _] if t.interfaces.nonEmpty => Some(t.interfaces.asInstanceOf[List[Type]] map (true -> _))
      case _ => None
    }),
    Field("possibleTypes", OptionType(ListType(__Type)), resolve = ctx => ctx.value._2 match {
      case t: AbstractType => ctx.schema.possibleTypes.get(t.name) map (_ sortBy (_.name) map (true -> _))
      case _ => None
    }),
    Field("enumValues", OptionType(ListType(__EnumValue)),
      arguments = includeDeprecated :: Nil,
      resolve = ctx => {
        val incDep = ctx.arg(includeDeprecated)

        ctx.value._2 match {
          case enum: EnumType[_] if incDep => Some(enum.values)
          case enum: EnumType[_] => Some(enum.values.filter(_.deprecationReason.isEmpty))
          case _ => None
        }
      }),
    Field("inputFields", OptionType(ListType(__InputValue)), resolve = _.value._2 match {
      case io: InputObjectType[_] => Some(io.fields)
      case _ => None
    })
  ))

  val __InputValue: ObjectType[Unit, InputValue[_]] = ObjectType(
    name = "__InputValue",
    fields = List[Field[Unit, InputValue[_]]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("type", __Type, resolve = false -> _.value.inputValueType),
      Field("defaultValue", OptionType(StringType),
        resolve = ctx => ctx.value.defaultValue.map(ctx.renderInputValueCompact(_, ctx.value.inputValueType)))
    ))

  val __EnumValue: ObjectType[Unit, EnumValue[_]] = ObjectType(
    name = "__EnumValue",
    fields = List[Field[Unit, EnumValue[_]]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("isDeprecated", BooleanType, resolve = _.value.deprecationReason.isDefined),
      Field("deprecationReason", OptionType(StringType), resolve = _.value.deprecationReason)
    ))

  val __Directive = ObjectType(
    name = "__Directive",
    fields = List[Field[Unit, Directive]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("args", ListType(__InputValue), resolve = _.value.arguments),
      Field("onOperation", BooleanType, resolve = _.value.onOperation),
      Field("onFragment", BooleanType, resolve = _.value.onFragment),
      Field("onField", BooleanType, resolve = _.value.onField)
    ))


  val __Schema = ObjectType(
    name = "__Schema",
    description =
        "A GraphQL Schema defines the capabilities of a GraphQL " +
        "server. It exposes all available types and directives on " +
        "the server, as well as the entry points for query and " +
        "mutation operations.",
    fields = List[Field[Unit, Schema[Any, Any]]](
      Field("types", ListType(__Type), Some("A list of all types supported by this server."), resolve = _.value.typeList map (true -> _)),
      Field("queryType", __Type, Some("The type that query operations will be rooted at."), resolve = true -> _.value.query),
      Field("mutationType", OptionType(__Type),
        Some("If this server supports mutation, the type that mutation operations will be rooted at."), resolve = _.value.mutation map (true -> _)),
      Field("directives", ListType(__Directive),
        Some("A list of all directives supported by this server."), resolve = _.value.directives)))

  val SchemaMetaField: Field[Unit, Unit] = Field(
    name = "__schema",
    fieldType = __Schema,
    description = Some("Access the current type schema of this server."),
    resolve = _.schema.asInstanceOf[Schema[Any, Any]])

  val TypeMetaField: Field[Unit, Unit] = Field(
    name = "__type",
    fieldType = OptionType(__Type),
    description = Some("Request the type information of a single type."),
    arguments = Argument("name", StringType) :: Nil,
    resolve = ctx => ctx.schema.types get ctx.arg[String]("name") map (true -> _._2))

  val TypeNameMetaField: Field[Unit, Unit] = Field(
    name = "__typename",
    fieldType = StringType,
    description = Some("The name of the current Object type at runtime."),
    resolve = ctx => ctx.parentType.name)

  lazy val Success(introspectionQuery) = QueryParser.parse(
    """
      |query IntrospectionQuery {
      |  __schema {
      |    queryType { name }
      |    mutationType { name }
      |    types {
      |      ...FullType
      |    }
      |    directives {
      |      name
      |      args {
      |        name
      |        type { ...TypeRef }
      |        defaultValue
      |      }
      |      onOperation
      |      onFragment
      |      onField
      |    }
      |  }
      |}
      |
      |fragment FullType on __Type {
      |  kind
      |  name
      |  fields {
      |    name
      |    args {
      |      name
      |      type { ...TypeRef }
      |      defaultValue
      |    }
      |    type {
      |      ...TypeRef
      |    }
      |    isDeprecated
      |    deprecationReason
      |  }
      |  inputFields {
      |    name
      |    type { ...TypeRef }
      |    defaultValue
      |  }
      |  interfaces {
      |    ...TypeRef
      |  }
      |  enumValues {
      |    name
      |    isDeprecated
      |    deprecationReason
      |  }
      |  possibleTypes {
      |    ...TypeRef
      |  }
      |}
      |
      |fragment TypeRef on __Type {
      |  kind
      |  name
      |  ofType {
      |    kind
      |    name
      |    ofType {
      |      kind
      |      name
      |      ofType {
      |        kind
      |        name
      |      }
      |    }
      |  }
      |}
    """.stripMargin)
}
