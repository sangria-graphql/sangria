package sangria.introspection

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.FutureResultSupport
import sangria.validation.QueryValidator

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class IntrospectionSpec extends WordSpec with Matchers with FutureResultSupport {
  "Introspection" should {
    "executes an introspection query" in {
      val schema = Schema(ObjectType[Unit, Unit]("QueryRoot", Nil))

      Executor.execute(schema, introspectionQuery).await should be (Map(
        "data" → Map(
          "__schema" → Map(
            "queryType" → Map(
              "name" → "QueryRoot"),
            "mutationType" → null,
            "subscriptionType" → null,
            "types" → Vector(
              Map(
                "inputFields" → null,
                "name" → "QueryRoot",
                "description" → null,
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Nil,
                "kind" → "OBJECT",
                "possibleTypes" → null
              ),
              Map(
                "inputFields" → null,
                "name" → "__Directive",
                "description" → "A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.\n\nIn some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.",
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Vector(
                  Map(
                    "name" → "name",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "String",
                        "ofType" → null))),
                  Map(
                    "name" → "description",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null)),
                  Map(
                    "name" → "locations",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "LIST",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "ENUM",
                            "name" → "__DirectiveLocation",
                            "ofType" → null))))),
                  Map("name" → "args",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "LIST",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "OBJECT",
                            "name" → "__InputValue",
                            "ofType" → null))))),
                  Map(
                    "name" → "onOperation",
                    "description" → null,
                    "isDeprecated" → true,
                    "deprecationReason" → "Use `locations`.",
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null))),
                  Map(
                    "name" → "onFragment",
                    "description" → null,
                    "isDeprecated" → true,
                    "deprecationReason" → "Use `locations`.",
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null))),
                  Map(
                    "name" → "onField",
                    "description" → null,
                    "isDeprecated" → true,
                    "deprecationReason" → "Use `locations`.",
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null)))),
                "kind" → "OBJECT",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "__DirectiveLocation",
                "description" → "A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.",
                "interfaces" → null,
                "enumValues" → Vector(
                  Map(
                    "name" → "QUERY",
                    "description" → "Location adjacent to a query operation.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "MUTATION",
                    "description" → "Location adjacent to a mutation operation.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "SUBSCRIPTION",
                    "description" → "Location adjacent to a subscription operation.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "FIELD",
                    "description" → "Location adjacent to a field.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "FRAGMENT_DEFINITION",
                    "description" → "Location adjacent to a fragment definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "FRAGMENT_SPREAD",
                    "description" → "Location adjacent to a fragment spread.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "INLINE_FRAGMENT",
                    "description" → "Location adjacent to an inline fragment.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "SCHEMA",
                    "description" → "Location adjacent to a schema definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "SCALAR",
                    "description" → "Location adjacent to a scalar definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "OBJECT",
                    "description" → "Location adjacent to an object type definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "FIELD_DEFINITION",
                    "description" → "Location adjacent to a field definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "ARGUMENT_DEFINITION",
                    "description" → "Location adjacent to an argument definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "INTERFACE",
                    "description" → "Location adjacent to an interface definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "UNION",
                    "description" → "Location adjacent to a union definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "ENUM",
                    "description" → "Location adjacent to an enum definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "ENUM_VALUE",
                    "description" → "Location adjacent to an enum value definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "INPUT_OBJECT",
                    "description" → "INPUT_OBJECT",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "INPUT_FIELD_DEFINITION",
                    "description" → "Location adjacent to an input object field definition.",
                    "isDeprecated" → false,
                    "deprecationReason" → null)),
                "fields" → null,
                "kind" → "ENUM",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "__EnumValue",
                "description" → "One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.",
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Vector(
                  Map(
                    "name" → "name",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "String",
                        "ofType" → null))),
                  Map(
                    "name" → "description",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null)),
                  Map(
                    "name" → "isDeprecated",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null))),
                  Map(
                    "name" → "deprecationReason",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null))
                ),
                "kind" → "OBJECT",
                "possibleTypes" → null
              ),
              Map(
                "inputFields" → null,
                "name" → "__Field",
                "description" → "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.",
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Vector(
                  Map(
                    "name" → "name",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "String",
                        "ofType" → null))),
                  Map(
                    "name" → "description",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null)),
                  Map(
                    "name" → "args",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "LIST",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "OBJECT",
                            "name" → "__InputValue",
                            "ofType" → null))))),
                  Map(
                    "name" → "type",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "OBJECT",
                        "name" → "__Type",
                        "ofType" → null))),
                  Map(
                    "name" → "isDeprecated",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null))),
                  Map(
                    "name" → "deprecationReason",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null))
                ),
                "kind" → "OBJECT",
                "possibleTypes" → null
              ),
              Map(
                "inputFields" → null,
                "name" → "__InputValue",
                "description" → "Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.",
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Vector(
                  Map(
                    "name" → "name",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "String",
                        "ofType" → null))),
                  Map(
                    "name" → "description",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null)),
                  Map(
                    "name" → "type",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "OBJECT",
                        "name" → "__Type",
                        "ofType" → null))),
                  Map(
                    "name" → "defaultValue",
                    "description" → "A GraphQL-formatted string representing the default value for this input value.",
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null))
                ),
                "kind" → "OBJECT",
                "possibleTypes" → null
              ),
              Map(
                "inputFields" → null,
                "name" → "__Schema",
                "description" → "A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.",
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Vector(
                  Map(
                    "name" → "types",
                    "description" → "A list of all types supported by this server.",
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "LIST",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "OBJECT",
                            "name" → "__Type",
                            "ofType" → null))))),
                  Map(
                    "name" → "queryType",
                    "description" → "The type that query operations will be rooted at.",
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "OBJECT",
                        "name" → "__Type",
                        "ofType" → null))),
                  Map(
                    "name" → "mutationType",
                    "description" → "If this server supports mutation, the type that mutation operations will be rooted at.",
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "OBJECT",
                      "name" → "__Type",
                      "ofType" → null)),
                  Map(
                    "name" → "subscriptionType",
                    "description" → "If this server support subscription, the type that subscription operations will be rooted at.",
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "OBJECT",
                      "name" → "__Type",
                      "ofType" → null)),
                  Map(
                    "name" → "directives",
                    "description" → "A list of all directives supported by this server.",
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "LIST",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "OBJECT",
                            "name" → "__Directive",
                            "ofType" → null)))))
                ),
                "kind" → "OBJECT",
                "possibleTypes" → null
              ),
              Map(
                "inputFields" → null,
                "name" → "__Type",
                "description" → "The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.\n\nDepending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.",
                "interfaces" → Nil,
                "enumValues" → null,
                "fields" → Vector(
                  Map(
                    "name" → "kind",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "ENUM",
                        "name" → "__TypeKind",
                        "ofType" → null))),
                  Map(
                    "name" → "name",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null)),
                  Map(
                    "name" → "description",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "SCALAR",
                      "name" → "String",
                      "ofType" → null)),
                  Map(
                    "name" → "fields",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Vector(
                      Map(
                        "name" → "includeDeprecated",
                        "description" → null,
                        "type" → Map(
                          "kind" → "SCALAR",
                          "name" → "Boolean",
                          "ofType" → null),
                        "defaultValue" → "false")),
                    "type" → Map(
                      "kind" → "LIST",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "NON_NULL",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "OBJECT",
                          "name" → "__Field",
                          "ofType" → null)))),
                  Map(
                    "name" → "interfaces",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "LIST",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "NON_NULL",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "OBJECT",
                          "name" → "__Type",
                          "ofType" → null)))),
                  Map(
                    "name" → "possibleTypes",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "LIST",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "NON_NULL",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "OBJECT",
                          "name" → "__Type",
                          "ofType" → null)))),
                  Map(
                    "name" → "enumValues",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Vector(
                      Map(
                        "name" → "includeDeprecated",
                        "description" → null,
                        "type" → Map(
                          "kind" → "SCALAR",
                          "name" → "Boolean",
                          "ofType" → null),
                        "defaultValue" → "false")),
                    "type" → Map(
                      "kind" → "LIST",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "NON_NULL",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "OBJECT",
                          "name" → "__EnumValue",
                          "ofType" → null)))),
                  Map(
                    "name" → "inputFields",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "LIST",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "NON_NULL",
                        "name" → null,
                        "ofType" → Map(
                          "kind" → "OBJECT",
                          "name" → "__InputValue",
                          "ofType" → null)))),
                  Map(
                    "name" → "ofType",
                    "description" → null,
                    "isDeprecated" → false,
                    "deprecationReason" → null,
                    "args" → Nil,
                    "type" → Map(
                      "kind" → "OBJECT",
                      "name" → "__Type",
                      "ofType" → null))
                ),
                "kind" → "OBJECT",
                "possibleTypes" → null
              ),
              Map(
                "inputFields" → null,
                "name" → "__TypeKind",
                "description" → "An enum describing what kind of type a given `__Type` is.",
                "interfaces" → null,
                "enumValues" → Vector(
                  Map(
                    "name" → "SCALAR",
                    "description" → "Indicates this type is a scalar.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "OBJECT",
                    "description" → "Indicates this type is an object. `fields` and `interfaces` are valid fields.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "INTERFACE",
                    "description" → "Indicates this type is an interface. `fields` and `possibleTypes` are valid fields.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "UNION",
                    "description" → "Indicates this type is a union. `possibleTypes` is a valid field.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "ENUM",
                    "description" → "Indicates this type is an enum. `enumValues` is a valid field.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "INPUT_OBJECT",
                    "description" → "Indicates this type is an input object. `inputFields` is a valid field.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "LIST",
                    "description" → "Indicates this type is a list. `ofType` is a valid field.",
                    "isDeprecated" → false,
                    "deprecationReason" → null),
                  Map(
                    "name" → "NON_NULL",
                    "description" → "Indicates this type is a non-null. `ofType` is a valid field.",
                    "isDeprecated" → false,
                    "deprecationReason" → null)
                ),
                "fields" → null,
                "kind" → "ENUM",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "BigDecimal",
                "description" → "The `BigDecimal` scalar type represents signed fractional values with arbitrary precision.",
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "BigInt",
                "description" → (
                  "The `BigInt` scalar type represents non-fractional signed whole numeric values. " +
                  "BigInt can represent arbitrary big values."),
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "Boolean",
                "description" → "The `Boolean` scalar type represents `true` or `false`.",
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "Float",
                "description" → (
                  "The `Float` scalar type represents signed double-precision fractional " +
                  "values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."),
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "ID",
                "description" → (
                  "The `ID` scalar type represents a unique identifier, often used to " +
                  "refetch an object or as key for a cache. The ID type appears in a JSON " +
                  "response as a String; however, it is not intended to be human-readable. " +
                  "When expected as an input type, any string (such as `\"4\"`) or integer " +
                  "(such as `4`) input value will be accepted as an ID."),
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "Int",
                "description" → (
                  "The `Int` scalar type represents non-fractional signed whole numeric values. " +
                  "Int can represent values between -(2^31) and 2^31 - 1."),
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "Long",
                "description" → (
                  "The `Long` scalar type represents non-fractional signed whole numeric values. " +
                  "Long can represent values between -(2^63) and 2^63 - 1."),
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null),
              Map(
                "inputFields" → null,
                "name" → "String",
                "description" → (
                  "The `String` scalar type represents textual data, represented as UTF-8 " +
                  "character sequences. The String type is most often used by GraphQL to " +
                  "represent free-form human-readable text."),
                "interfaces" → null,
                "enumValues" → null,
                "fields" → null,
                "kind" → "SCALAR",
                "possibleTypes" → null)),
            "directives" → Vector(
              Map(
                "name" → "include",
                "description" → "Directs the executor to include this field or fragment only when the `if` argument is true.",
                "locations" → Vector("FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"),
                "args" → Vector(
                  Map(
                    "name" → "if",
                    "description" → "Included when true.",
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null)),
                    "defaultValue" → null))),
              Map(
                "name" → "skip",
                "description" → "Directs the executor to skip this field or fragment when the `if` argument is true.",
                "locations" → Vector("FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"),
                "args" → Vector(
                  Map(
                    "name" → "if",
                    "description" → "Included when true.",
                    "type" → Map(
                      "kind" → "NON_NULL",
                      "name" → null,
                      "ofType" → Map(
                        "kind" → "SCALAR",
                        "name" → "Boolean",
                        "ofType" → null)),
                    "defaultValue" → null))),
              Map(
                "name" → "deprecated",
                "description" → "Marks an element of a GraphQL schema as no longer supported.",
                "locations" → Vector("ENUM_VALUE", "FIELD_DEFINITION"),
                "args" → Vector(
                  Map(
                    "name" → "reason",
                    "description" → "Explains why this element was deprecated, usually also including a suggestion for how to access supported similar data. Formattedin [Markdown](https://daringfireball.net/projects/markdown/).",
                    "type" → Map("kind" → "SCALAR", "name" → "String", "ofType" → null),
                    "defaultValue" → "\"No longer supported\""))))))))
    }

    "introspects on input object" in {
      val inputType = InputObjectType("TestInputObject", List(
        InputField("a", OptionInputType(StringType), defaultValue = "foo"),
        InputField("b", OptionInputType(ListInputType(OptionInputType(StringType))))
      ))

      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("field", OptionType(StringType),
          arguments = Argument("complex", OptionInputType(inputType)) :: Nil,
          resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __schema {
              types {
                kind
                name
                inputFields {
                  name
                  type { ...TypeRef }
                  defaultValue
                }
              }
            }
          }

          fragment TypeRef on __Type {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        """
      )

      val BuiltInTypes = List(
        Map(
          "kind" → "OBJECT",
          "name" → "TestType",
          "inputFields" → null
        ),
        Map(
          "kind" → "OBJECT",
          "name" → "__Directive",
          "inputFields" → null
        ),
        Map(
          "kind" → "ENUM",
          "name" → "__DirectiveLocation",
          "inputFields" → null
        ),
        Map(
          "kind" → "OBJECT",
          "name" → "__EnumValue",
          "inputFields" → null
        ),
        Map(
          "kind" → "OBJECT",
          "name" → "__Field",
          "inputFields" → null
        ),
        Map(
          "kind" → "OBJECT",
          "name" → "__InputValue",
          "inputFields" → null
        ),
        Map(
          "kind" → "OBJECT",
          "name" → "__Schema",
          "inputFields" → null
        ),
        Map(
          "kind" → "OBJECT",
          "name" → "__Type",
          "inputFields" → null
        ),
        Map(
          "kind" → "ENUM",
          "name" → "__TypeKind",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "BigDecimal",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "BigInt",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "Boolean",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "Float",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "ID",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "Int",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "Long",
          "inputFields" → null
        ),
        Map(
          "kind" → "SCALAR",
          "name" → "String",
          "inputFields" → null
        )
      )

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "__schema" → Map(
            "types" → (List(
              Map(
                "kind" → "INPUT_OBJECT",
                "name" → "TestInputObject",
                "inputFields" → List(
                  Map(
                    "name" → "a",
                    "type" →
                      Map(
                        "kind" → "SCALAR",
                        "name" → "String",
                        "ofType" → null
                      ),
                    "defaultValue" → "\"foo\""
                  ),
                  Map(
                    "name" → "b",
                    "type" →
                      Map(
                        "kind" → "LIST",
                        "name" → null,
                        "ofType" →
                          Map(
                            "kind" → "SCALAR",
                            "name" → "String",
                            "ofType" → null
                          )
                      ),
                    "defaultValue" → null
                  )
                )
              )
            ) ++ BuiltInTypes)
          )
        )
      ))
    }

    "supports the __type root field" in {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testField", OptionType(StringType), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __type(name: "TestType") {
              name
            }
          }
        """
      )

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "__type" → Map(
            "name" → "TestType"
          )
        )
      ))
    }

    "identifies deprecated fields" in {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __type(name: "TestType") {
              name
              fields(includeDeprecated: true) {
                name
                isDeprecated,
                deprecationReason
              }
            }
          }
        """
      )

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "__type" → Map(
            "name" → "TestType",
            "fields" → List(
              Map(
                "name" → "nonDeprecated",
                "isDeprecated" → false,
                "deprecationReason" → null
              ),
              Map(
                "name" → "deprecated",
                "isDeprecated" → true,
                "deprecationReason" → "Removed in 1.0"
              )
            )
          )
        )
      ))
    }

    "respects the includeDeprecated parameter for fields" in {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __type(name: "TestType") {
              name
              trueFields: fields(includeDeprecated: true) {
                name
              }
              falseFields: fields(includeDeprecated: false) {
                name
              }
              omittedFields: fields {
                name
              }
            }
          }
        """
      )

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "__type" → Map(
            "name" → "TestType",
            "trueFields" → List(
              Map(
                "name" → "nonDeprecated"
              ),
              Map(
                "name" → "deprecated"
              )
            ),
            "falseFields" → List(
              Map(
                "name" → "nonDeprecated"
              )
            ),
            "omittedFields" → List(
              Map(
                "name" → "nonDeprecated"
              )
            )
          )
        )
      ))
    }

    "identifies deprecated enum values" in {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testEnum", OptionType(testEnum), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __type(name: "TestEnum") {
              name
              enumValues(includeDeprecated: true) {
                name
                isDeprecated,
                deprecationReason
              }
            }
          }
        """
      )

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "__type" → Map(
            "name" → "TestEnum",
            "enumValues" → List(
              Map(
                "name" → "NONDEPRECATED",
                "isDeprecated" → false,
                "deprecationReason" → null
              ),
              Map(
                "name" → "DEPRECATED",
                "isDeprecated" → true,
                "deprecationReason" → "Removed in 1.0"
              ),
              Map(
                "name" → "ALSONONDEPRECATED",
                "isDeprecated" → false,
                "deprecationReason" → null
              )
            )
          )
        )
      ))
    }

    "respects the includeDeprecated parameter for enum values" in {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testEnum", OptionType(testEnum), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __type(name: "TestEnum") {
              name
              trueValues: enumValues(includeDeprecated: true) {
                name
              }
              falseValues: enumValues(includeDeprecated: false) {
                name
              }
              omittedValues: enumValues {
                name
              }
            }
          }
        """
      )

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "__type" → Map(
            "name" → "TestEnum",
            "trueValues" → List(
              Map(
                "name" → "NONDEPRECATED"
              ),
              Map(
                "name" → "DEPRECATED"
              ),
              Map(
                "name" → "ALSONONDEPRECATED"
              )
            ),
            "falseValues" → List(
              Map(
                "name" → "NONDEPRECATED"
              ),
              Map(
                "name" → "ALSONONDEPRECATED"
              )
            ),
            "omittedValues" → List(
              Map(
                "name" → "NONDEPRECATED"
              ),
              Map(
                "name" → "ALSONONDEPRECATED"
              )
            )
          )
        )
      ))
    }

    "fails as expected on the __type root field without an arg" in {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testField", OptionType(StringType), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            __type {
              name
            }
          }
        """
      )

      val result = Executor.execute(schema, query, queryValidator = QueryValidator.empty).await.asInstanceOf[Map[String, Any]]

      result("data") should be (Map("__type" → null))
      result("errors").asInstanceOf[Seq[Map[String, Any]]](0)("message").asInstanceOf[String] should include (
        "Null value was provided for the NotNull Type 'String!' at path 'name'.")
    }

    "exposes descriptions on types and fields" in {
      val schema = Schema(ObjectType[Unit, Unit]("QueryRoot", Nil))

      val Success(query) = QueryParser.parse(
        """
          {
            schemaType: __type(name: "__Schema") {
              name,
              description,
              fields {
                name,
                description
              }
            }
          }
        """)

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "schemaType" → Map(
            "name" → "__Schema",
            "description" → (
                "A GraphQL Schema defines the capabilities of a GraphQL " +
                "server. It exposes all available types and directives on " +
                "the server, as well as the entry points for query, mutation, and subscription operations."),
            "fields" → List(
              Map(
                "name" → "types",
                "description" → "A list of all types supported by this server."
              ),
              Map(
                "name" → "queryType",
                "description" → "The type that query operations will be rooted at."
              ),
              Map(
                "name" → "mutationType",
                "description" → (
                  "If this server supports mutation, the type that " +
                  "mutation operations will be rooted at.")
              ),
              Map(
                "name" → "subscriptionType",
                "description" → (
                  "If this server support subscription, the type that " +
                  "subscription operations will be rooted at.")
              ),
              Map(
                "name" → "directives",
                "description" → "A list of all directives supported by this server."
              )
            )
          )
        )
      ))
    }

    "exposes descriptions on enums" in {
      val schema = Schema(ObjectType[Unit, Unit]("QueryRoot", Nil))

      val Success(query) = QueryParser.parse(
        """
          {
            typeKindType: __type(name: "__TypeKind") {
              name,
              description,
              enumValues {
                name,
                description
              }
            }
          }
        """)

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "typeKindType" → Map(
            "name" → "__TypeKind",
            "description" → "An enum describing what kind of type a given `__Type` is.",
            "enumValues" → List(
              Map(
                "description" → "Indicates this type is a scalar.",
                "name" → "SCALAR"
              ),
              Map(
                "description" → (
                  "Indicates this type is an object. " +
                  "`fields` and `interfaces` are valid fields."),
                "name" → "OBJECT"
              ),
              Map(
                "description" → (
                  "Indicates this type is an interface. " +
                  "`fields` and `possibleTypes` are valid fields."),
                "name" → "INTERFACE"
              ),
              Map(
                "description" → (
                  "Indicates this type is a union. " +
                  "`possibleTypes` is a valid field."),
                "name" → "UNION"
              ),
              Map(
                "description" → (
                  "Indicates this type is an enum. " +
                  "`enumValues` is a valid field."),
                "name" → "ENUM"
              ),
              Map(
                "description" → (
                  "Indicates this type is an input object. " +
                  "`inputFields` is a valid field."),
                "name" → "INPUT_OBJECT"
              ),
              Map(
                "description" → (
                  "Indicates this type is a list. " +
                  "`ofType` is a valid field."),
                "name" → "LIST"
              ),
              Map(
                "description" → (
                  "Indicates this type is a non-null. " +
                  "`ofType` is a valid field."),
                "name" → "NON_NULL"
              )
            )
          )
        )
      ))
    }
  }
}