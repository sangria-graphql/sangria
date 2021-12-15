package sangria.introspection

import sangria.ast
import sangria.parser.QueryParser

/** Utility methods for introspection. */
object Introspection {
  def introspectionQuery: ast.Document = introspectionQuery()

  private[this] def introspectionQuery(schemaDescription: Boolean = true): ast.Document =
    QueryParser.parse(introspectionQueryString(schemaDescription)).get

  private[this] def introspectionQueryString(schemaDescription: Boolean = true): String =
    s"""query IntrospectionQuery {
       |  __schema {
       |    queryType { name }
       |    mutationType { name }
       |    subscriptionType { name }
       |    types {
       |      ...FullType
       |    }
       |    directives {
       |      name
       |      description
       |      locations
       |      args {
       |        ...InputValue
       |      }
       |    }
       |    ${if (schemaDescription) "description" else ""}
       |  }
       |}
       |fragment FullType on __Type {
       |  kind
       |  name
       |  description
       |  fields(includeDeprecated: true) {
       |    name
       |    description
       |    args {
       |      ...InputValue
       |    }
       |    type {
       |      ...TypeRef
       |    }
       |    isDeprecated
       |    deprecationReason
       |  }
       |  inputFields {
       |    ...InputValue
       |  }
       |  interfaces {
       |    ...TypeRef
       |  }
       |  enumValues(includeDeprecated: true) {
       |    name
       |    description
       |    isDeprecated
       |    deprecationReason
       |  }
       |  possibleTypes {
       |    ...TypeRef
       |  }
       |}
       |fragment InputValue on __InputValue {
       |  name
       |  description
       |  type { ...TypeRef }
       |  defaultValue
       |}
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
       |        ofType {
       |          kind
       |          name
       |          ofType {
       |            kind
       |            name
       |            ofType {
       |              kind
       |              name
       |              ofType {
       |                kind
       |                name
       |              }
       |            }
       |          }
       |        }
       |      }
       |    }
       |  }
       |}""".stripMargin
}
