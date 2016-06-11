package sangria.starWars

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestSchema.StarWarsSchema
import sangria.starWars.TestData.{FriendsResolver, CharacterRepo}
import sangria.util.FutureResultSupport

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class StarWarsIntrospectionSpec extends WordSpec with Matchers with FutureResultSupport {
  "Basic Introspection" should {
    "Allows querying the schema for types" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionTypeQuery {
          __schema {
            types {
              name
            }
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__schema" → Map(
              "types" → List(
                Map("name" → "Character"),
                Map("name" → "Droid"),
                Map("name" → "Episode"),
                Map("name" → "Human"),
                Map("name" → "Query"),
                Map("name" → "__Directive"),
                Map("name" → "__DirectiveLocation"),
                Map("name" → "__EnumValue"),
                Map("name" → "__Field"),
                Map("name" → "__InputValue"),
                Map("name" → "__Schema"),
                Map("name" → "__Type"),
                Map("name" → "__TypeKind"),
                Map("name" → "BigDecimal"),
                Map("name" → "BigInt"),
                Map("name" → "Boolean"),
                Map("name" → "Float"),
                Map("name" → "ID"),
                Map("name" → "Int"),
                Map("name" → "Long"),
                Map("name" → "String")
              )
            )
          )
        ))
    }

    "Allows querying the schema for query type" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionQueryTypeQuery {
          __schema {
            queryType {
              name
            }
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__schema" → Map(
              "queryType" → Map(
                "name" → "Query"
              )
            )
          )
        ))
    }

    "Allows querying the schema for a specific type" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionDroidTypeQuery {
          __type(name: "Droid") {
            name
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__type" → Map(
              "name" → "Droid"
            )
          )
        ))
    }

    "Allows querying the schema for an object kind" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionDroidKindQuery {
          __type(name: "Droid") {
            name
            kind
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__type" → Map(
              "name" → "Droid",
              "kind" → "OBJECT"
            )
          )
        ))
    }

    "Allows querying the schema for an interface kind" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionCharacterKindQuery {
          __type(name: "Character") {
            name
            kind
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__type" → Map(
              "name" → "Character",
              "kind" → "INTERFACE"
            )
          )
        ))
    }

    "Allows querying the schema for object fields" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionDroidFieldsQuery {
          __type(name: "Droid") {
            name
            fields {
              name
              type {
                name
                kind
              }
            }
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__type" → Map(
              "name" → "Droid",
              "fields" → List(
                Map(
                  "name" → "id",
                  "type" → Map(
                    "name" → null,
                    "kind" → "NON_NULL"
                  )
                ),
                Map(
                  "name" → "name",
                  "type" → Map(
                    "name" → "String",
                    "kind" → "SCALAR"
                  )
                ),
                Map(
                  "name" → "friends",
                  "type" → Map(
                    "name" → null,
                    "kind" → "LIST"
                  )
                ),
                Map(
                  "name" → "appearsIn",
                  "type" → Map(
                    "name" → null,
                    "kind" → "LIST"
                  )
                ),
                Map(
                  "name" → "primaryFunction",
                  "type" → Map(
                    "name" → "String",
                    "kind" → "SCALAR"
                  )
                ),
                Map(
                  "name" → "secretBackstory",
                  "type" → Map(
                    "name" → "String",
                    "kind" → "SCALAR"
                  )
                )
              )
            )
          )
        ))
    }

    "Allows querying the schema for nested object fields" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionDroidNestedFieldsQuery {
          __type(name: "Droid") {
            name
            fields {
              name
              type {
                name
                kind
                ofType {
                  name
                  kind
                }
              }
            }
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__type" → Map(
              "name" → "Droid",
              "fields" → List(
                Map(
                  "name" → "id",
                  "type" → Map(
                    "name" → null,
                    "kind" → "NON_NULL",
                    "ofType" → Map(
                      "name" → "String",
                      "kind" → "SCALAR"
                    )
                  )
                ),
                Map(
                  "name" → "name",
                  "type" → Map(
                    "name" → "String",
                    "kind" → "SCALAR",
                    "ofType" → null
                  )
                ),
                Map(
                  "name" → "friends",
                  "type" → Map(
                    "name" → null,
                    "kind" → "LIST",
                    "ofType" → Map(
                      "name" → "Character",
                      "kind" → "INTERFACE"
                    )
                  )
                ),
                Map(
                  "name" → "appearsIn",
                  "type" → Map(
                    "name" → null,
                    "kind" → "LIST",
                    "ofType" → Map(
                      "name" → "Episode",
                      "kind" → "ENUM"
                    )
                  )
                ),
                Map(
                  "name" → "primaryFunction",
                  "type" → Map(
                    "name" → "String",
                    "kind" → "SCALAR",
                    "ofType" → null
                  )
                ),
                Map(
                  "name" → "secretBackstory",
                  "type" → Map(
                    "name" → "String",
                    "kind" → "SCALAR",
                    "ofType" → null
                  )
                )
              )
            )
          )
        ))
    }


    "Allows querying the schema for field args" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionQueryTypeQuery {
          __schema {
            queryType {
              fields {
                name
                args {
                  name
                  description
                  type {
                    name
                    kind
                    ofType {
                      name
                      kind
                    }
                  }
                  defaultValue
                }
              }
            }
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__schema" → Map(
              "queryType" → Map(
                "fields" → List(
                  Map(
                    "name" → "hero",
                    "args" → List(
                      Map(
                        "defaultValue" → null,
                        "description" → (
                          "If omitted, returns the hero of the whole " +
                          "saga. If provided, returns the hero of " +
                          "that particular episode."),
                        "name" → "episode",
                        "type" → Map(
                          "kind" → "ENUM",
                          "name" → "Episode",
                          "ofType" → null
                        )
                      )
                    )
                  ),
                  Map(
                    "name" → "human",
                    "args" → List(
                      Map(
                        "name" → "id",
                        "description" → "id of the character",
                        "type" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "SCALAR",
                            "name" → "String"
                          )
                        ),
                        "defaultValue" → null
                      )
                    )
                  ),
                  Map(
                    "name" → "droid",
                    "args" → List(
                      Map(
                        "name" → "id",
                        "description" → "id of the character",
                        "type" → Map(
                          "kind" → "NON_NULL",
                          "name" → null,
                          "ofType" → Map(
                            "kind" → "SCALAR",
                            "name" → "String"
                          )
                        ),
                        "defaultValue" → null
                      )
                    )
                  )
                )
              )
            )
          )
        ))
    }

    "Allows querying the schema for documentation" in {
      val Success(query) = QueryParser.parse("""
        query IntrospectionDroidDescriptionQuery {
          __type(name: "Droid") {
            name
            description
          }
        }
        """)

      Executor.execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver).await should be (
        Map(
          "data" → Map(
            "__type" → Map(
              "name" → "Droid",
              "description" → "A mechanical creature in the Star Wars universe."
            )
          )
        ))
    }
  }
}