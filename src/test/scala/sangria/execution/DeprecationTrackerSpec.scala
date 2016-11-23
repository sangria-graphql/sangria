package sangria.execution

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{OutputMatchers, FutureResultSupport}

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class DeprecationTrackerSpec extends WordSpec with Matchers with FutureResultSupport with OutputMatchers {
  class RecordingDeprecationTracker extends DeprecationTracker {
    val times = new AtomicInteger(0)
    var ctx: Option[Context[_, _]] = None

    var enumValue: Option[Any] = None
    var enum: Option[String] = None

    def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) = {
      times.incrementAndGet()

      this.ctx = Some(ctx)
    }

    def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) = {
      times.incrementAndGet()
      this.enumValue = Some(value)
      this.enum = Some(enum.name)
    }
  }

  "DeprecationTracker" should {
    "not track non-deprecated fields" in  {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be (0)
      deprecationTracker.ctx should be (None)
    }

    "track deprecated fields" in  {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated deprecated}")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be (1)
      deprecationTracker.ctx.get.path.path should be (Vector("deprecated"))
      deprecationTracker.ctx.get.field.name should be ("deprecated")
    }

    "provide context information" in  {
      lazy val testType: ObjectType[Unit, Unit] = ObjectType("TestType", () ⇒ fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None),
        Field("nested", OptionType(testType), resolve = _ ⇒ Some(()))
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nested { aa: nested { bb: deprecated }}}")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be (1)
      deprecationTracker.ctx.get.path.path should be (Vector("nested", "aa", "bb"))
      deprecationTracker.ctx.get.field.name should be ("deprecated")
      deprecationTracker.ctx.get.parentType.name should be ("TestType")
    }

    "report usage even if field is defined only in the interface type" in  {
      val testInt = InterfaceType("TestInterface", () ⇒ fields[Unit, Unit](
        Field("foo", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val testType = ObjectType("TestType", interfaces[Unit, Unit](testInt), fields[Unit, Unit](
        Field("foo", OptionType(StringType), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ foo }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be (1)
      deprecationTracker.ctx.get.path.path should be (Vector("foo"))
      deprecationTracker.ctx.get.field.name should be ("foo")
      deprecationTracker.ctx.get.parentType.name should be ("TestType")
    }

    "track deprecated enum values" in  {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testEnum", OptionType(StringType),
          arguments = Argument("foo", testEnum) :: Nil,
          resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            a: testEnum(foo: NONDEPRECATED)
            b: testEnum(foo: DEPRECATED)
          }
        """)

      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be (1)
      deprecationTracker.enum should be (Some("TestEnum"))
      deprecationTracker.enumValue should be (Some(2))
    }

    "not track non-deprecated enum values" in  {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testEnum", OptionType(StringType),
          arguments = Argument("foo", testEnum) :: Nil,
          resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            a: testEnum(foo: NONDEPRECATED)
            b: testEnum(foo: ALSONONDEPRECATED)
          }
        """)

      val deprecationTracker = new RecordingDeprecationTracker

      Executor.execute(schema, query, deprecationTracker = deprecationTracker).await

      deprecationTracker.times.get should be (0)
      deprecationTracker.enum should be (None)
      deprecationTracker.enumValue should be (None)
    }
  }

  "NilDeprecationTracker" should {
    "shouldn't do anything" in  {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated }")

      Executor.execute(schema, query, deprecationTracker = DeprecationTracker.empty).await
    }
  }

  "PrintingDeprecationTracker" should {
    "track deprecated enum values" in  {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("testEnum", OptionType(StringType),
          arguments = Argument("foo", testEnum) :: Nil,
          resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)

      val Success(query) = QueryParser.parse(
        """
          {
            a: testEnum(foo: NONDEPRECATED)
            b: testEnum(foo: DEPRECATED)
          }
        """)

      val out = captureConsoleOut {
        Executor.execute(schema, query, deprecationTracker = DeprecationTracker.print).await
      }

      out should include ("Deprecated enum value '2' used of enum 'TestEnum'.")
    }

    "track deprecated fields" in  {
      val testType = ObjectType("TestType", fields[Unit, Unit](
        Field("nonDeprecated", OptionType(StringType), resolve = _ ⇒ None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ ⇒ None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated deprecated}")

      val out = captureConsoleOut {
        Executor.execute(schema, query, deprecationTracker = DeprecationTracker.print).await
      }

      out should include ("Deprecated field 'TestType.deprecated' used at path 'deprecated'.")
    }
  }
}
