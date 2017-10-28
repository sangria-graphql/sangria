package sangria.schema

import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.{Executor, QueryReducer}
import sangria.macros._
import sangria.schema.AstSchemaBuilder.{FieldName, TypeName, resolverBased}
import sangria.schema.{DirectiveLocation => DL}
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.validation.BaseViolation
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class ResolverBasedAstSchemaBuilderSpec extends WordSpec with Matchers with FutureResultSupport {
  case object UUIDViolation extends BaseViolation("Invalid UUID")

  val UUIDType =
    ScalarType[UUID]("UUID",
      coerceOutput = (v, _) ⇒ v.toString,
      coerceUserInput = {
        case s: String ⇒ Try(UUID.fromString(s)).fold(_ ⇒ Left(UUIDViolation), Right(_))
        case _ ⇒ Left(UUIDViolation)
      },
      coerceInput = {
        case ast.StringValue(s, _, _) ⇒ Try(UUID.fromString(s)).fold(_ ⇒ Left(UUIDViolation), Right(_))
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
        ResolverBasedAstSchemaBuilder.defaultAnyInputResolver[Any, JsValue])

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
        DirectiveResolver(AddFinalDir, c ⇒ {
          val finalValue = c.ctx.arg[String]("final")

          c.lastValue match {
            case Some(last) ⇒ last.map(_ + finalValue)
            case None ⇒ finalValue
          }
        }, complexity = Some(_ ⇒ (_, _, _) ⇒ 100.0)))

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
  }
}
