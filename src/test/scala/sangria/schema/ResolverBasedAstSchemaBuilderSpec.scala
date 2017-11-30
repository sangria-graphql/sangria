package sangria.schema

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.{Executor, QueryReducer}
import sangria.macros._
import sangria.marshalling.{InputUnmarshaller, ScalaInput}
import sangria.schema.AstSchemaBuilder.{EnumName, FieldName, TypeName, resolverBased}
import sangria.schema.{DirectiveLocation => DL}
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.validation.BaseViolation
import sangria.schema.ResolverBasedAstSchemaBuilder._
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

class ResolverBasedAstSchemaBuilderSpec extends WordSpec with Matchers with FutureResultSupport {
  case object UUIDViolation extends BaseViolation("Invalid UUID")

  def parseUuid(s: String) = Try(UUID.fromString(s)) match {
    case Success(s) ⇒ Right(s)
    case Failure(e) ⇒ Left(UUIDViolation)
  }

  val UUIDType =
    ScalarType[UUID]("UUID",
      coerceOutput = (v, _) ⇒ v.toString,
      coerceUserInput = {
        case s: String ⇒ parseUuid(s)
        case _ ⇒ Left(UUIDViolation)
      },
      coerceInput = {
        case ast.StringValue(s, _, _) ⇒ parseUuid(s)
        case _ ⇒ Left(UUIDViolation)
      })

  "ResolverBasedAstSchemaBuilder" should {
    "provide basic resolution capabilities" in {
      import sangria.marshalling.sprayJson._
      
      val ValueArg = Argument("value", StringType)
      val TestDir = Directive("test", arguments = ValueArg :: Nil, locations = Set(DL.FieldDefinition))

      case class CustomIntViolation(value: Int, min: Option[Int], max: Option[Int]) extends BaseViolation(
        s"Int value must be ${min.fold("")(_ + " <= ")}$value${max.fold("")(" <= " + _)}.")

      val MinArg = Argument("min", OptionInputType(IntType))
      val MaxArg = Argument("max", OptionInputType(IntType))
      val ValidateIntDir = Directive("validateInt", arguments = MinArg :: MaxArg :: Nil, locations = Set(DL.ArgumentDefinition, DL.InputFieldDefinition))

      def intValidationAlias(min: Option[Int], max: Option[Int]) = ScalarAlias[Int, Int](IntType,
        toScalar = identity,
        fromScalar = v ⇒ {
          if (min.isDefined && v < min.get) Left(CustomIntViolation(v, min, max))
          else if (max.isDefined && v > max.get) Left(CustomIntViolation(v, min, max))
          else Right(v)
        })

      val NVArg = Argument("v", IntType)

      val CoolDir = Directive("cool", locations = Set(DL.Scalar))
      val NumDir = Directive("num", arguments = NVArg :: Nil, locations = Set(DL.Schema, DL.Object))
      val AddExtraFieldsDir = Directive("addExtraFields", locations = Set(DL.Object))

      val builder = resolverBased[Any](
        AdditionalTypes(UUIDType),
        DirectiveFieldProvider(AddExtraFieldsDir, c ⇒ List(MaterializedField(c.origin,
          ast.FieldDefinition("extraField", ast.NamedType("Int"), Vector.empty)))),
        DynamicDirectiveFieldProvider[Any, JsValue]("addExtraDynFields", c ⇒ List(MaterializedField(c.origin,
          Field("extraDynField", c.materializer.getScalarType(c.origin, ast.NamedType("String")),
            resolve = (_: Context[Any, Any]) ⇒ "foo")))),
        AdditionalDirectives(Seq(NumDir)),
        DirectiveInputTypeResolver(ValidateIntDir, _.withArgs(MinArg, MaxArg)(intValidationAlias(_, _))),
        DirectiveScalarResolver(CoolDir, _ ⇒ StringType),
        DirectiveResolver(TestDir, resolve = _.arg(ValueArg)),
        DynamicDirectiveResolver[Any, JsValue]("json", resolve = _.args),
        FieldResolver {case (TypeName("Query"), FieldName("id")) ⇒ _ ⇒ UUID.fromString("a26bdfd4-0fcf-484f-b363-585091b3319f")},
        AnyFieldResolver.defaultInput[Any, JsValue])

      val schemaAst =
        gql"""
          type Person @addExtraFields {
            name: String!
            size: String
          }

          type Query @num(v: 22) {
            aaa: String @test(value: "foo")
            id(age: Int @validateInt(min: 2, max: 10)): UUID
            b: Person @json(name: "Oleg", extraDynField: "eca12748-ba8c-4dfd-951d-6e1fa21cc31e")
            c: Hello @test(value: "bar")
          }

          extend type Person @addExtraDynFields {
            foo: Int
          }

          scalar Hello @cool

          schema @num(v: 123) {
            query: Query
          }
        """

      val collectedValue = schemaAst.analyzer.resolveDirectives(
        GenericDirectiveResolver(NumDir, resolve = c ⇒ Some(c arg NVArg))).sum

      collectedValue should be (145)

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            id(age: 5)
            c
            aaa
            b {
              name
              size
              extraField
              extraDynField
            }

          }
        """

      Executor.execute(schema, query).await should be (
        """
        {
          "data": {
            "id": "a26bdfd4-0fcf-484f-b363-585091b3319f",
            "c": "bar",
            "aaa": "foo",
            "b": {
              "name": "Oleg",
              "size": null,
              "extraField": null,
              "extraDynField": "eca12748-ba8c-4dfd-951d-6e1fa21cc31e"
            }
          }
        }
        """.parseJson)

      val introspectionQuery =
        gql"""
          {
            __type(name: "Query") {
              fields {
                name
                type {
                  kind
                  name
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
            __schema {
              types {
                name
              }
            }
          }
        """

      Executor.execute(schema, introspectionQuery).await should be (
        """
        {
          "data": {
            "__type": {
              "fields": [{
                "name": "aaa",
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                }
              }, {
                "name": "id",
                "type": {
                  "kind": "SCALAR",
                  "name": "UUID",
                  "ofType": null
                }
              }, {
                "name": "b",
                "type": {
                  "kind": "OBJECT",
                  "name": "Person",
                  "ofType": null
                }
              }, {
                "name": "c",
                "type": {
                  "kind": "SCALAR",
                  "name": "String",
                  "ofType": null
                }
              }]
            },
            "__schema": {
              "types": [{
                "name": "Int"
              }, {
                "name": "Person"
              }, {
                "name": "Query"
              }, {
                "name": "UUID"
              }, {
                "name": "__Directive"
              }, {
                "name": "__DirectiveLocation"
              }, {
                "name": "__EnumValue"
              }, {
                "name": "__Field"
              }, {
                "name": "__InputValue"
              }, {
                "name": "__Schema"
              }, {
                "name": "__Type"
              }, {
                "name": "__TypeKind"
              }, {
                "name": "Boolean"
              }, {
                "name": "String"
              }]
            }
          }
        }
        """.parseJson)
    }

    "resolve fields based on the directives" in {
      val ValueArg = Argument("value", StringType)

      val ConstDir = Directive("const", arguments = ValueArg :: Nil, locations = Set(DL.FieldDefinition))
      val AddDir = Directive("add", arguments = ValueArg :: Nil, locations = Set(DL.FieldDefinition))
      val AddFinalDir = Directive("addFinal", locations = Set(DL.Schema, DL.FieldDefinition))

      val builder = resolverBased[Any](
        DirectiveResolver(ConstDir, c ⇒ c.arg(ValueArg)),
        DirectiveResolver(AddDir, c ⇒ c.withArgs(ValueArg) { value ⇒
          c.lastValue match {
            case Some(last) ⇒ last.map(_ + value)
            case None ⇒ value
          }
        }),
        DirectiveResolver(AddFinalDir,
          c ⇒ {
            val finalValue = c.ctx.arg[String]("final")

            c.lastValue match {
              case Some(last) ⇒ last.map(_ + finalValue)
              case None ⇒ finalValue
            }
          },
          complexity = Some(_ ⇒ (_, _, _) ⇒ 100.0)))

      val schemaAst =
        gql"""
          type Query {
            myStr(final: String!): String @const(value: "first") @add(value: "-second") @addFinal
            myStr1(final: String!): String @addFinal @add(value: "second")
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            myStr(final: "-last")
            myStr1(final: "realFirst-")
          }
        """

      val complexity = new AtomicInteger(0)
      val reducer = QueryReducer.measureComplexity[Any]((c, _) ⇒ complexity.set(c.toInt))

      Executor.execute(schema, query, queryReducers = reducer :: Nil).await should be (
        Map(
          "data" → Map(
            "myStr" → "first-second-last",
            "myStr1" → "realFirst-second")))

      complexity.get should be (200)
    }

    "resolve enum values" in {
      val builder = resolverBased[Any](
        SimpleEnumValueResolver {
          case (EnumName("Color"), v) if v.name == "RED" ⇒ "#FF0000"
          case (EnumName("Color"), v) if v.name == "GREEN" ⇒ "#00FF00"
          case (EnumName("Color"), v) if v.name == "BLUE" ⇒ "#0000FF"
        },
        FieldResolver {
          case (TypeName("Mutation"), FieldName("eat")) ⇒
            ctx ⇒ "tasty " + ctx.arg[String]("color") + " " + ctx.arg[InputObjectType.DefaultInput]("fruit")("color")
        })

      val schemaAst =
        gql"""
          enum Color {
            RED, GREEN, BLUE
          }

          input Fruit {
            name: String!
             color: Color!
          }

          extend type Mutation {
            eat(fruit: Fruit!, color: Color!): String
          }
        """

      val existingSchema = Schema(
        query = ObjectType("Query", fields[Any, Unit](
          Field("testQuery", StringType, resolve = _ ⇒ "test"))),
        mutation = Some(ObjectType("Mutation", fields[Any, Unit](
          Field("testMut", StringType, resolve = _ ⇒ "test")))))

      val schema = existingSchema.extend(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          mutation {
            testMut
            eat(fruit: {name: "Apple", color: RED}, color: GREEN)
          }
        """

      Executor.execute(schema, query).await should be (
        Map("data" → Map(
          "testMut" → "test",
          "eat" → "tasty #00FF00 #FF0000")))

      val queryWithVars =
        gql"""
          mutation Eat($$color1: Color!, $$color2: Color!, $$fruit: Fruit!) {
            eat(fruit: {name: "Apple", color: $$color1}, color: $$color2)
            more: eat(fruit: $$fruit, color: RED)
          }
        """

      val vars = InputUnmarshaller.mapVars(
        "color1" → "RED",
        "color2" → "BLUE",
        "fruit" → Map(
          "name" → "Banana",
          "color" → "GREEN"))

      Executor.execute(schema, queryWithVars, variables = vars).await should be (
        Map("data" → Map(
          "eat" → "tasty #0000FF #FF0000",
          "more" → "tasty #FF0000 #00FF00")))
    }

    "resolve fields based on the dynamic directives" in {
      import sangria.marshalling.sprayJson._

      val builder = resolverBased[Any](
        DynamicDirectiveResolver[Any, JsValue]("add", c ⇒ c.args.asJsObject.fields("value") match {
          case JsString(str) ⇒
            c.lastValue match {
              case Some(last) ⇒ last.map(_ + str)
              case None ⇒ str
            }
          case _ ⇒ c.lastValue.getOrElse("")
        }),
        DynamicDirectiveResolver[Any, JsValue]("addFinal",
          c ⇒ {
            val finalValue = c.ctx.arg[String]("final")

            c.lastValue match {
              case Some(last) ⇒ last.map(_ + finalValue)
              case None ⇒ finalValue
            }
          },
          complexity = Some(_ ⇒ (_, _, _) ⇒ 100.0)))

      val schemaAst =
        gql"""
          type Query {
            myStr(final: String!): String @add(value: "first") @addFinal
            myStr1(final: String!): String @addFinal @add(value: "second")
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            myStr(final: "-last")
            myStr1(final: "realFirst-")
          }
        """

      val complexity = new AtomicInteger(0)
      val reducer = QueryReducer.measureComplexity[Any]((c, _) ⇒ complexity.set(c.toInt))

      Executor.execute(schema, query, queryReducers = reducer :: Nil).await should be (
        """
          {
            "data": {
              "myStr": "first-last",
              "myStr1": "realFirst-second"
            }
          }
        """.parseJson)

      complexity.get should be (200)
    }

    "resolve fields based on names" in {
      val builder = resolverBased[Unit](
        FieldResolver {
          case (TypeName("Query"), field @ FieldName(fieldName)) if fieldName startsWith "test" ⇒
            c ⇒ c.arg[Int](field.arguments.head.name) + 1
        },
        FieldResolver.map(
          "Query" → Map(
            "a" → (_ ⇒ "a value"),
            "b" → (_ ⇒ "b value"))),
        ExistingFieldResolver {
          case (_, _, field) if field.name startsWith "existing" ⇒
            c ⇒ "replacement"
        },
        ExistingFieldResolver.map(
          "Query" → Map(
            "c" → (_ ⇒ "c value"))))

      val existingSchema = Schema(ObjectType("Query", fields[Unit, Unit](
        Field("simple", StringType, resolve = _ ⇒ "value"),
        Field("c", StringType, resolve = _ ⇒ "c value"),
        Field("existingField", StringType, resolve = _ ⇒ "foo"))))

      val schemaAst =
        gql"""
          extend type Query {
            a: String
            b: String
            testOne(size: Int!): Int
            testTwo(size: Int!): Int
          }
        """

      val schema = existingSchema.extend(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            existingField
            simple
            testOne(size: 123)
            testTwo(size: 1)
            a
            b
            c
          }
        """

      Executor.execute(schema, query).await should be (Map(
        "data" → Map(
          "simple" → "value",
          "existingField" → "replacement",
          "testOne" → 124,
          "testTwo" → 2,
          "a" → "a value",
          "b" → "b value",
          "c" → "c value")))
    }

    "support instance check" in {
      import sangria.marshalling.sprayJson._
      
      val builder = resolverBased[Unit](
        InstanceCheck.simple {
          case value: JsValue if value.asJsObject.fields.contains("type") ⇒
            value.asJsObject.fields("type").asInstanceOf[JsString].value
          case value: JsValue if value.asJsObject.fields.contains("name") ⇒
            "Dog"
          case _ ⇒
            "Cat"
        },
        AnyFieldResolver.defaultInput[Unit, JsValue])

      val schemaAst =
        gql"""
          enum Color {
            Red, Green, Blue
          }

          interface Fruit {
            id: ID!
          }

          type Apple implements Fruit {
            id: ID!
            color: Color
          }

          type Banana implements Fruit {
            id: ID!
            length: Int
          }

          type Dog {
            name: String!
          }

          type Cat {
            size: Int
          }

          union Pet = Dog | Cat

          type Query {
            fruits: [Fruit]
            pets: [Pet]
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            fruits {
              __typename
              id

              ... on Apple {color}
              ... on Banana {length}
            }
            pets {
              __typename

              ... on Dog {name}
              ... on Cat {size}
            }
          }
        """

      val data =
        """
          {
            "fruits": [{
              "type": "Apple",
              "id": "1",
              "color": "Red"
            }, {
              "type": "Banana",
              "id": "2",
              "length": 12
            }],
            "pets": [{
              "name": "foo"
            }, {
              "size": 50
            }]
          }
        """.parseJson

      Executor.execute(schema, query, root = data).await should be (
        """
          {
            "data": {
              "fruits": [{
                "__typename": "Apple",
                "id": "1",
                "color": "Red"
              }, {
                "__typename": "Banana",
                "id": "2",
                "length": 12
              }],
              "pets": [{
                "__typename": "Dog",
                "name": "foo"
              }, {
                "__typename": "Cat",
                "size": 50
              }]
            }
          }
        """.parseJson)
    }

    "support field-based instance check" in {
      import sangria.marshalling.sprayJson._

      val builder = resolverBased[Unit](
        InstanceCheck.field[Unit, JsValue],
        AnyFieldResolver.defaultInput[Unit, JsValue])

      val schemaAst =
        gql"""
          type Dog {
            name: String!
          }

          type Cat {
            size: Int
          }

          union Pet = Dog | Cat

          type Query {
            pets: [Pet]
          }
        """

      val schema = Schema.buildFromAst(schemaAst, builder.validateSchemaWithException(schemaAst))

      val query =
        gql"""
          {
            pets {
              __typename

              ... on Dog {name}
              ... on Cat {size}
            }
          }
        """

      val data =
        """
          {
            "pets": [{
              "type": "Dog",
              "name": "foo"
            }, {
              "type": "Cat",
              "size": 50
            }]
          }
        """.parseJson

      Executor.execute(schema, query, root = data).await should be (
        """
          {
            "data": {
              "pets": [{
                "__typename": "Dog",
                "name": "foo"
              }, {
                "__typename": "Cat",
                "size": 50
              }]
            }
          }
        """.parseJson)
    }
  }
}
