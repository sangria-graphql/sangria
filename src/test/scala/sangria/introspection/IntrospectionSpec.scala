package sangria.introspection

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class IntrospectionSpec extends WordSpec with Matchers with AwaitSupport {
  "Introspection" should {
    "executes an introspection query" in {
      val schema = Schema(ObjectType[Unit, Unit]("QueryRoot", Nil))

      Executor(schema).execute(introspectionQuery).await should be (Map(
        "data" -> Map(
          "__schema" -> Map(
            "mutationType" -> null,
            "queryType" -> Map(
              "name" -> "QueryRoot"
            ),
            "types" -> List(
              Map(
                "kind" -> "OBJECT",
                "name" -> "QueryRoot",
                "fields" -> List(),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "OBJECT",
                "name" -> "__Directive",
                "fields" -> List(
                  Map(
                    "name" -> "name",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "String",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "description",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "args",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "LIST",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "NON_NULL",
                          "name" -> null,
                          "ofType" -> Map(
                            "kind" -> "OBJECT",
                            "name" -> "__InputValue"
                          )
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "onOperation",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "onFragment",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "onField",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "OBJECT",
                "name" -> "__EnumValue",
                "fields" -> List(
                  Map(
                    "name" -> "name",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "String",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "description",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "isDeprecated",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "deprecationReason",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "OBJECT",
                "name" -> "__Field",
                "fields" -> List(
                  Map(
                    "name" -> "name",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "String",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "description",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "args",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "LIST",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "NON_NULL",
                          "name" -> null,
                          "ofType" -> Map(
                            "kind" -> "OBJECT",
                            "name" -> "__InputValue"
                          )
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "type",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "OBJECT",
                        "name" -> "__Type",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "isDeprecated",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "deprecationReason",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "OBJECT",
                "name" -> "__InputValue",
                "fields" -> List(
                  Map(
                    "name" -> "name",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "String",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "description",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "type",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "OBJECT",
                        "name" -> "__Type",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "defaultValue",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "OBJECT",
                "name" -> "__Schema",
                "fields" -> List(
                  Map(
                    "name" -> "types",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "LIST",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "NON_NULL",
                          "name" -> null,
                          "ofType" -> Map(
                            "kind" -> "OBJECT",
                            "name" -> "__Type"
                          )
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "queryType",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "OBJECT",
                        "name" -> "__Type",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "mutationType",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "OBJECT",
                      "name" -> "__Type",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "directives",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "LIST",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "NON_NULL",
                          "name" -> null,
                          "ofType" -> Map(
                            "kind" -> "OBJECT",
                            "name" -> "__Directive"
                          )
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "OBJECT",
                "name" -> "__Type",
                "fields" -> List(
                  Map(
                    "name" -> "kind",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "ENUM",
                        "name" -> "__TypeKind",
                        "ofType" -> null
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "name",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "description",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "SCALAR",
                      "name" -> "String",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "fields",
                    "args" -> List(
                      Map(
                        "name" -> "includeDeprecated",
                        "type" -> Map(
                          "kind" -> "SCALAR",
                          "name" -> "Boolean",
                          "ofType" -> null
                        ),
                        "defaultValue" -> "false"
                      )
                    ),
                    "type" -> Map(
                      "kind" -> "LIST",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "NON_NULL",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "OBJECT",
                          "name" -> "__Field",
                          "ofType" -> null
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "interfaces",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "LIST",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "NON_NULL",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "OBJECT",
                          "name" -> "__Type",
                          "ofType" -> null
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "possibleTypes",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "LIST",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "NON_NULL",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "OBJECT",
                          "name" -> "__Type",
                          "ofType" -> null
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "enumValues",
                    "args" -> List(
                      Map(
                        "defaultValue" -> "false",
                        "name" -> "includeDeprecated",
                        "type" -> Map(
                          "kind" -> "SCALAR",
                          "name" -> "Boolean",
                          "ofType" -> null
                        )
                      )
                    ),
                    "type" -> Map(
                      "kind" -> "LIST",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "NON_NULL",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "OBJECT",
                          "name" -> "__EnumValue",
                          "ofType" -> null
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "inputFields",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "LIST",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "NON_NULL",
                        "name" -> null,
                        "ofType" -> Map(
                          "kind" -> "OBJECT",
                          "name" -> "__InputValue",
                          "ofType" -> null
                        )
                      )
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "ofType",
                    "args" -> List(),
                    "type" -> Map(
                      "kind" -> "OBJECT",
                      "name" -> "__Type",
                      "ofType" -> null
                    ),
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "inputFields" -> null,
                "interfaces" -> List(),
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "ENUM",
                "name" -> "__TypeKind",
                "fields" -> null,
                "inputFields" -> null,
                "interfaces" -> null,
                "enumValues" -> List(
                  Map(
                    "name" -> "SCALAR",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "OBJECT",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "INTERFACE",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "UNION",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "ENUM",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "INPUT_OBJECT",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "LIST",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  ),
                  Map(
                    "name" -> "NON_NULL",
                    "isDeprecated" -> false,
                    "deprecationReason" -> null
                  )
                ),
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "SCALAR",
                "name" -> "Boolean",
                "fields" -> null,
                "inputFields" -> null,
                "interfaces" -> null,
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "SCALAR",
                "name" -> "Float",
                "fields" -> null,
                "inputFields" -> null,
                "interfaces" -> null,
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "SCALAR",
                "name" -> "ID",
                "fields" -> null,
                "inputFields" -> null,
                "interfaces" -> null,
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "SCALAR",
                "name" -> "Int",
                "fields" -> null,
                "inputFields" -> null,
                "interfaces" -> null,
                "enumValues" -> null,
                "possibleTypes" -> null
              ),
              Map(
                "kind" -> "SCALAR",
                "name" -> "String",
                "fields" -> null,
                "inputFields" -> null,
                "interfaces" -> null,
                "enumValues" -> null,
                "possibleTypes" -> null
              )
            ),
            "directives" -> List(
              Map(
                "name" -> "include",
                "args" -> List(
                  Map(
                    "defaultValue" -> null,
                    "name" -> "if",
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    )
                  )
                ),
                "onOperation" -> false,
                "onFragment" -> true,
                "onField" -> true
              ),
              Map(
                "name" -> "skip",
                "args" -> List(
                  Map(
                    "defaultValue" -> null,
                    "name" -> "if",
                    "type" -> Map(
                      "kind" -> "NON_NULL",
                      "name" -> null,
                      "ofType" -> Map(
                        "kind" -> "SCALAR",
                        "name" -> "Boolean",
                        "ofType" -> null
                      )
                    )
                  )
                ),
                "onOperation" -> false,
                "onFragment" -> true,
                "onField" -> true
              )
            )
          )
        )
      ))
    }

    "introspects on input object" in {
      val inputType = InputObjectType("TestInputObject", List(
        InputField("a", OptionInputType(StringType), defaultValue = Some("foo")),
        InputField("b", OptionInputType(ListInputType(OptionInputType(StringType))))
      ))

      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("field", OptionType(StringType),
          arguments = Argument("complex", OptionInputType(inputType)) :: Nil,
          resolve = _ => None)
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
          "kind" -> "OBJECT",
          "name" -> "TestType",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "OBJECT",
          "name" -> "__Directive",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "OBJECT",
          "name" -> "__EnumValue",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "OBJECT",
          "name" -> "__Field",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "OBJECT",
          "name" -> "__InputValue",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "OBJECT",
          "name" -> "__Schema",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "OBJECT",
          "name" -> "__Type",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "ENUM",
          "name" -> "__TypeKind",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "SCALAR",
          "name" -> "Boolean",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "SCALAR",
          "name" -> "Float",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "SCALAR",
          "name" -> "ID",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "SCALAR",
          "name" -> "Int",
          "inputFields" -> null
        ),
        Map(
          "kind" -> "SCALAR",
          "name" -> "String",
          "inputFields" -> null
        )
      )

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "__schema" -> Map(
            "types" -> (List(
              Map(
                "kind" -> "INPUT_OBJECT",
                "name" -> "TestInputObject",
                "inputFields" -> List(
                  Map(
                    "name" -> "a",
                    "type" ->
                      Map(
                        "kind" -> "SCALAR",
                        "name" -> "String",
                        "ofType" -> null
                      ),
                    "defaultValue" -> "foo"
                  ),
                  Map(
                    "name" -> "b",
                    "type" ->
                      Map(
                        "kind" -> "LIST",
                        "name" -> null,
                        "ofType" ->
                          Map(
                            "kind" -> "SCALAR",
                            "name" -> "String",
                            "ofType" -> null
                          )
                      ),
                    "defaultValue" -> null
                  )
                )
              )
            ) ++ BuiltInTypes)
          )
        )
      ))
    }

    "supports the __type root field" in {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testField", OptionType(StringType), resolve = _ => None)
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "__type" -> Map(
            "name" -> "TestType"
          )
        )
      ))
    }

    "identifies deprecated fields" in {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ => None)
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "__type" -> Map(
            "name" -> "TestType",
            "fields" -> List(
              Map(
                "name" -> "nonDeprecated",
                "isDeprecated" -> false,
                "deprecationReason" -> null
              ),
              Map(
                "name" -> "deprecated",
                "isDeprecated" -> true,
                "deprecationReason" -> "Removed in 1.0"
              )
            )
          )
        )
      ))
    }

    "respects the includeDeprecated parameter for fields" in {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ => None)
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "__type" -> Map(
            "name" -> "TestType",
            "trueFields" -> List(
              Map(
                "name" -> "nonDeprecated"
              ),
              Map(
                "name" -> "deprecated"
              )
            ),
            "falseFields" -> List(
              Map(
                "name" -> "nonDeprecated"
              )
            ),
            "omittedFields" -> List(
              Map(
                "name" -> "nonDeprecated"
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

      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testEnum", OptionType(testEnum), resolve = _ => None)
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "__type" -> Map(
            "name" -> "TestEnum",
            "enumValues" -> List(
              Map(
                "name" -> "NONDEPRECATED",
                "isDeprecated" -> false,
                "deprecationReason" -> null
              ),
              Map(
                "name" -> "DEPRECATED",
                "isDeprecated" -> true,
                "deprecationReason" -> "Removed in 1.0"
              ),
              Map(
                "name" -> "ALSONONDEPRECATED",
                "isDeprecated" -> false,
                "deprecationReason" -> null
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

      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testEnum", OptionType(testEnum), resolve = _ => None)
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "__type" -> Map(
            "name" -> "TestEnum",
            "trueValues" -> List(
              Map(
                "name" -> "NONDEPRECATED"
              ),
              Map(
                "name" -> "DEPRECATED"
              ),
              Map(
                "name" -> "ALSONONDEPRECATED"
              )
            ),
            "falseValues" -> List(
              Map(
                "name" -> "NONDEPRECATED"
              ),
              Map(
                "name" -> "ALSONONDEPRECATED"
              )
            ),
            "omittedValues" -> List(
              Map(
                "name" -> "NONDEPRECATED"
              ),
              Map(
                "name" -> "ALSONONDEPRECATED"
              )
            )
          )
        )
      ))
    }

    "fails as expected on the __type root field without an arg" in {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testField", OptionType(StringType), resolve = _ => None)
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

      val result = Executor(schema).execute(query).await.asInstanceOf[Map[String, Any]]

      result("data") should be (Map("__type" -> null))
      result("errors").asInstanceOf[List[Map[String, Any]]](0)("message").asInstanceOf[String] should include (
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "schemaType" -> Map(
            "name" -> "__Schema",
            "description" -> (
              "A GraphQL Schema defines the capabilities of a " +
              "GraphQL server. It exposes all available types and " +
              "directives on the server, as well as the entry " +
              "points for query and mutation operations."),
            "fields" -> List(
              Map(
                "name" -> "types",
                "description" -> "A list of all types supported by this server."
              ),
              Map(
                "name" -> "queryType",
                "description" -> "The type that query operations will be rooted at."
              ),
              Map(
                "name" -> "mutationType",
                "description" -> (
                  "If this server supports mutation, the type that " +
                  "mutation operations will be rooted at.")
              ),
              Map(
                "name" -> "directives",
                "description" -> "A list of all directives supported by this server."
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

      Executor(schema).execute(query).await should be (Map(
        "data" -> Map(
          "typeKindType" -> Map(
            "name" -> "__TypeKind",
            "description" -> "An enum describing what kind of type a given __Type is.",
            "enumValues" -> List(
              Map(
                "description" -> "Indicates this type is a scalar.",
                "name" -> "SCALAR"
              ),
              Map(
                "description" -> (
                  "Indicates this type is an object. " +
                  "`fields` and `interfaces` are valid fields."),
                "name" -> "OBJECT"
              ),
              Map(
                "description" -> (
                  "Indicates this type is an interface. " +
                  "`fields` and `possibleTypes` are valid fields."),
                "name" -> "INTERFACE"
              ),
              Map(
                "description" -> (
                  "Indicates this type is a union. " +
                  "`possibleTypes` is a valid field."),
                "name" -> "UNION"
              ),
              Map(
                "description" -> (
                  "Indicates this type is an enum. " +
                  "`enumValues` is a valid field."),
                "name" -> "ENUM"
              ),
              Map(
                "description" -> (
                  "Indicates this type is an input object. " +
                  "`inputFields` is a valid field."),
                "name" -> "INPUT_OBJECT"
              ),
              Map(
                "description" -> (
                  "Indicates this type is a list. " +
                  "`ofType` is a valid field."),
                "name" -> "LIST"
              ),
              Map(
                "description" -> (
                  "Indicates this type is a non-null. " +
                  "`ofType` is a valid field."),
                "name" -> "NON_NULL"
              )
            )
          )
        )
      ))
    }
  }
}