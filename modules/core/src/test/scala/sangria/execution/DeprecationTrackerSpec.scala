package sangria.execution

import java.util.concurrent.atomic.AtomicInteger
import sangria.parser.QueryParser
import sangria.ast
import sangria.macros._
import sangria.schema._
import sangria.util.{FutureResultSupport, OutputMatchers}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeprecationTrackerSpec
    extends AnyWordSpec
    with Matchers
    with FutureResultSupport
    with OutputMatchers {
  class RecordingDeprecationTracker extends DeprecationTracker {
    val times = new AtomicInteger(0)
    var ctx: Option[Context[_, _]] = None

    var enumValue: Option[Any] = None
    var `enum`: Option[String] = None

    var argument: Option[Argument[_]] = None

    var inputObject: Option[InputObjectType[_]] = None
    var inputField: Option[InputField[_]] = None

    var directive: Option[Directive] = None

    def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) = {
      times.incrementAndGet()

      this.ctx = Some(ctx)
    }

    def deprecatedEnumValueUsed[T, Ctx](`enum`: EnumType[T], value: T, userContext: Ctx) = {
      times.incrementAndGet()
      this.enumValue = Some(value)
      this.`enum` = Some(`enum`.name)
    }

    def deprecatedFieldArgUsed[Ctx](arg: Argument[_], ctx: Context[Ctx, _]) = {
      times.incrementAndGet()
      this.ctx = Some(ctx)
      this.argument = Some(arg)
    }

    def deprecatedDirectiveArgUsed[Ctx](
        directive: Directive,
        arg: Argument[_],
        ctx: Context[Ctx, _]) = {
      times.incrementAndGet()
      this.ctx = Some(ctx)
      this.directive = Some(directive)
      this.argument = Some(arg)
    }

    def deprecatedInputObjectFieldUsed[T, Ctx](
        inputObject: InputObjectType[T],
        field: InputField[_],
        ctx: Context[Ctx, _]) = {
      times.incrementAndGet()
      this.ctx = Some(ctx)
      this.inputObject = Some(inputObject)
      this.inputField = Some(field)
    }
  }

  "DeprecationTracker" should {
    "not track non-deprecated fields" in {
      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
          Field(
            "deprecated",
            OptionType(StringType),
            deprecationReason = Some("Removed in 1.0"),
            resolve = _ => None)
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(0)
      deprecationTracker.ctx should be(None)
    }

    "track deprecated fields" in {
      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
          Field(
            "deprecated",
            OptionType(StringType),
            deprecationReason = Some("Removed in 1.0"),
            resolve = _ => None)
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated deprecated}")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("deprecated"))
      deprecationTracker.ctx.get.field.name should be("deprecated")
    }

    "not track non-deprecated fields args" in {
      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "someField",
            OptionType(StringType),
            resolve = _ => None,
            arguments = List(
              Argument("deprecated", OptionInputType(IntType)).withDeprecationReason(
                "use notDeprecated"),
              Argument("notDeprecated", OptionInputType(IntType))
            )
          )
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ someField(notDeprecated: 123) }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(0)
      deprecationTracker.ctx should be(None)
    }

    "track deprecated fields args" in {
      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "someField",
            OptionType(StringType),
            resolve = _ => None,
            arguments = List(
              Argument("deprecated", OptionInputType(IntType)).withDeprecationReason(
                "use notDeprecated"),
              Argument("notDeprecated", OptionInputType(IntType))
            )
          )
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ someField(deprecated: 123) }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("someField"))
      deprecationTracker.ctx.get.field.name should be("someField")

      deprecationTracker.argument.get.name should be("deprecated")
    }

    "not track non-deprecated input object fields" in {
      val inputObject = InputObjectType(
        "SomeFieldInput",
        List(
          InputField("deprecated", OptionInputType(IntType)).withDeprecationReason(
            "use notDeprecated"),
          InputField("notDeprecated", OptionInputType(IntType))
        )
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "someField",
            OptionType(StringType),
            resolve = _ => None,
            arguments = Argument("input", inputObject) :: Nil)
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ someField(input: { notDeprecated: 123}) }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(0)
      deprecationTracker.ctx should be(None)
    }

    "track deprecated input object fields" in {
      val inputObject = InputObjectType(
        "SomeFieldInput",
        List(
          InputField("deprecated", OptionInputType(IntType)).withDeprecationReason(
            "use notDeprecated"),
          InputField("notDeprecated", OptionInputType(IntType))
        )
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "someField",
            OptionType(StringType),
            resolve = _ => None,
            arguments = Argument("input", inputObject) :: Nil)
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ someField(input: { deprecated: 123}) }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("someField"))
      deprecationTracker.ctx.get.field.name should be("someField")

      deprecationTracker.inputObject.get.name should be("SomeFieldInput")
      deprecationTracker.inputField.get.name should be("deprecated")
    }

    "track nested deprecated input object fields" in {
      val schemaAst = gql"""
      input SomeFieldInput {
        deprecated: Int @deprecated(reason: "use notDeprecated")
        notDeprecated: Int
        nestedInput: SomeFieldInput
      }

      type Query {
        someField(input: SomeFieldInput!): String
      }
      """

      val schema = Schema.buildFromAst[Unit](schemaAst, AstSchemaBuilder.default)

      val query = gql"""{
        someField(
          input: {
            notDeprecated: 123,
            nestedInput: {
              deprecated: 123
            }
          }
        )
      }"""
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("someField"))
      deprecationTracker.ctx.get.field.name should be("someField")

      deprecationTracker.inputObject.get.name should be("SomeFieldInput")
      deprecationTracker.inputField.get.name should be("deprecated")
    }

    "track deprecated input object fields in a list" in {
      val schemaAst = gql"""
      input SomeFieldInput {
        deprecated: Int @deprecated(reason: "use notDeprecated")
        notDeprecated: Int
      }

      type Query {
        someField(input: [SomeFieldInput]): String
      }
      """

      val schema = Schema.buildFromAst[Unit](schemaAst, AstSchemaBuilder.default)

      val query = gql"""{
        someField(
          input: [{ notDeprecated: 123 }, { deprecated: 123 }]
        )
      }"""
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("someField"))
      deprecationTracker.ctx.get.field.name should be("someField")

      deprecationTracker.inputObject.get.name should be("SomeFieldInput")
      deprecationTracker.inputField.get.name should be("deprecated")
    }

    "not track non-deprecated directive args" in {
      val directive = Directive(
        "customDirective",
        locations = Set(DirectiveLocation.ArgumentDefinition, DirectiveLocation.Field),
        arguments = List(
          Argument("deprecated", OptionInputType(IntType)).withDeprecationReason(
            "use notDeprecated"),
          Argument("notDeprecated", OptionInputType(IntType))
        )
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "someField",
            OptionType(StringType),
            resolve = _ => None,
            astDirectives = Vector(
              ast.Directive(
                "customDirective",
                arguments = Vector(ast.Argument("notDeprecated", ast.IntValue(123)))))
          )
        )
      )

      val schema = Schema(testType, directives = directive :: Nil)
      val Success(query) = QueryParser.parse("{ someField @customDirective(notDeprecated: 123) }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(0)
      deprecationTracker.ctx should be(None)
    }

    "track deprecated directive args" in {
      // have the args have the same directive to detect infinite loops
      val astDirective = ast.Directive(
        "customDirective",
        arguments = Vector(ast.Argument("notDeprecated", ast.IntValue(123))))

      val astDirectiveWithDeprecated = ast.Directive(
        "customDirective",
        arguments = Vector(ast.Argument("deprecated", ast.IntValue(123))))

      val directive = Directive(
        "customDirective",
        locations = Set(DirectiveLocation.ArgumentDefinition, DirectiveLocation.Field),
        arguments = List(
          Argument("deprecated", OptionInputType(IntType))
            .withDeprecationReason("use notDeprecated")
            .withDirective(astDirective),
          Argument("notDeprecated", OptionInputType(IntType)).withDirective(astDirective)
        )
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "fooField",
            OptionType(StringType),
            resolve = _ => None,
            astDirectives = Vector(astDirective)
          ),
          Field(
            "barField",
            OptionType(StringType),
            resolve = _ => None,
            astDirectives = Vector(astDirectiveWithDeprecated)
          )
        )
      )

      val schema = Schema(testType, directives = directive :: Nil)
      val Success(query) = QueryParser.parse(
        "{ fooField @customDirective(notDeprecated: 123) barField @customDirective(deprecated: 123) }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("barField"))
      deprecationTracker.ctx.get.field.name should be("barField")

      deprecationTracker.directive.get.name should be("customDirective")
      deprecationTracker.argument.get.name should be("deprecated")
    }

    "provide context information" in {
      lazy val testType: ObjectType[Unit, Unit] = ObjectType(
        "TestType",
        () =>
          fields[Unit, Unit](
            Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
            Field(
              "deprecated",
              OptionType(StringType),
              deprecationReason = Some("Removed in 1.0"),
              resolve = _ => None),
            Field("nested", OptionType(testType), resolve = _ => Some(()))
          )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nested { aa: nested { bb: deprecated }}}")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("nested", "aa", "bb"))
      deprecationTracker.ctx.get.path.cacheKey should be(
        Vector("nested", "TestType", "aa", "TestType", "bb", "TestType"))
      deprecationTracker.ctx.get.path.cacheKeyReversed should be(
        List("TestType", "bb", "TestType", "aa", "TestType", "nested"))
      deprecationTracker.ctx.get.path.cacheKeyReversedIterator.toList should be(
        List("TestType", "bb", "TestType", "aa", "TestType", "nested"))
      deprecationTracker.ctx.get.field.name should be("deprecated")
      deprecationTracker.ctx.get.parentType.name should be("TestType")
    }

    "report usage even if field is defined only in the interface type" in {
      val testInt = InterfaceType(
        "TestInterface",
        () =>
          fields[Unit, Unit](
            Field(
              "foo",
              OptionType(StringType),
              deprecationReason = Some("Removed in 1.0"),
              resolve = _ => None)
          ))

      val testType = ObjectType(
        "TestType",
        interfaces[Unit, Unit](testInt),
        fields[Unit, Unit](
          Field("foo", OptionType(StringType), resolve = _ => None)
        ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ foo }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.ctx.get.path.path should be(Vector("foo"))
      deprecationTracker.ctx.get.field.name should be("foo")
      deprecationTracker.ctx.get.parentType.name should be("TestType")
    }

    "track deprecated enum values" in {
      val testEnum = EnumType[Int](
        "TestEnum",
        values = List(
          EnumValue("NONDEPRECATED", value = 1),
          EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
          EnumValue("ALSONONDEPRECATED", value = 3))
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "testEnum",
            OptionType(StringType),
            arguments = Argument("foo", testEnum) :: Nil,
            resolve = _ => None)
        ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse("""
          {
            a: testEnum(foo: NONDEPRECATED)
            b: testEnum(foo: DEPRECATED)
          }
        """)

      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(1)
      deprecationTracker.`enum` should be(Some("TestEnum"))
      deprecationTracker.enumValue should be(Some(2))
    }

    "not track non-deprecated enum values" in {
      val testEnum = EnumType[Int](
        "TestEnum",
        values = List(
          EnumValue("NONDEPRECATED", value = 1),
          EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
          EnumValue("ALSONONDEPRECATED", value = 3))
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "testEnum",
            OptionType(StringType),
            arguments = Argument("foo", testEnum) :: Nil,
            resolve = _ => None)
        ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse("""
          {
            a: testEnum(foo: NONDEPRECATED)
            b: testEnum(foo: ALSONONDEPRECATED)
          }
        """)

      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be(0)
      deprecationTracker.`enum` should be(None)
      deprecationTracker.enumValue should be(None)
    }
  }

  "NilDeprecationTracker" should {
    "shouldn't do anything" in {
      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
          Field(
            "deprecated",
            OptionType(StringType),
            deprecationReason = Some("Removed in 1.0"),
            resolve = _ => None)
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated }")

      Executor.execute(schema, query, deprecationTracker = DeprecationTracker.empty).await
    }
  }

  "LoggingDeprecationTracker" should {
    "track deprecated enum values" in {
      val testEnum = EnumType[Int](
        "TestEnum",
        values = List(
          EnumValue("NONDEPRECATED", value = 1),
          EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
          EnumValue("ALSONONDEPRECATED", value = 3))
      )

      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field(
            "testEnum",
            OptionType(StringType),
            arguments = Argument("foo", testEnum) :: Nil,
            resolve = _ => None)
        ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse("""
          {
            a: testEnum(foo: NONDEPRECATED)
            b: testEnum(foo: DEPRECATED)
          }
        """)

      val sb = new StringBuilder()
      val tracker = new LoggingDeprecationTracker(sb.append(_))

      Executor.execute(schema, query, deprecationTracker = tracker).await

      sb.toString() should include("Deprecated enum value '2' used of enum 'TestEnum'.")
    }

    "track deprecated fields" in {
      val testType = ObjectType(
        "TestType",
        fields[Unit, Unit](
          Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
          Field(
            "deprecated",
            OptionType(StringType),
            deprecationReason = Some("Removed in 1.0"),
            resolve = _ => None)
        )
      )

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated deprecated}")

      val sb = new StringBuilder()
      val tracker = new LoggingDeprecationTracker(sb.append(_))

      Executor.execute(schema, query, deprecationTracker = tracker).await

      sb.toString() should include(
        "Deprecated field 'TestType.deprecated' used at path 'deprecated'.")
    }
  }
}
