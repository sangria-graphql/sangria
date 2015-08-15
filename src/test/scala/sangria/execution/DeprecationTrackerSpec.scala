package sangria.execution

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{OutputMatchers, AwaitSupport}

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class DeprecationTrackerSpec extends WordSpec with Matchers with AwaitSupport with OutputMatchers {
  class RecordingDeprecationTracker extends DeprecationTracker {
    val times = new AtomicInteger(0)
    var path: Option[List[String]] = None
    var name: Option[String] = None

    var enumValue: Option[Any] = None
    var enum: Option[String] = None

    def deprecatedFieldUsed[Ctx](path: List[String], field: Field[Ctx, _], userContext: Ctx) = {
      times.incrementAndGet()
      this.path = Some(path)
      this.name = Some(field.name)
    }

    def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) = {
      times.incrementAndGet()
      this.enumValue = Some(value)
      this.enum = Some(enum.name)
    }
  }

  "DeprecationTracker" should {
    "not track non-deprecated fields" in  {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ => None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated }")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor(schema, deprecationTracker = deprecationTracker).execute(query).await

      deprecationTracker.times.get should be (0)
      deprecationTracker.path should be (None)
      deprecationTracker.name should be (None)
    }

    "track deprecated fields" in  {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ => None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated deprecated}")
      val deprecationTracker = new RecordingDeprecationTracker

      Executor(schema, deprecationTracker = deprecationTracker).execute(query).await

      deprecationTracker.times.get should be (1)
      deprecationTracker.path should be (Some(List("deprecated")))
      deprecationTracker.name should be (Some("deprecated"))
    }

    "track deprecated enum values" in  {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testEnum", OptionType(StringType),
          arguments = Argument("foo", testEnum) :: Nil,
          resolve = _ => None)
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

      Executor(schema, deprecationTracker = deprecationTracker).execute(query).await

      deprecationTracker.times.get should be (1)
      deprecationTracker.enum should be (Some("TestEnum"))
      deprecationTracker.enumValue should be (Some(2))
    }

    "not track non-deprecated enum values" in  {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testEnum", OptionType(StringType),
          arguments = Argument("foo", testEnum) :: Nil,
          resolve = _ => None)
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

      Executor(schema, deprecationTracker = deprecationTracker).execute(query).await

      deprecationTracker.times.get should be (0)
      deprecationTracker.enum should be (None)
      deprecationTracker.enumValue should be (None)
    }
  }

  "NilDeprecationTracker" should {
    "shouldn't do anything" in  {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ => None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated }")

      Executor(schema, deprecationTracker = DeprecationTracker.empty).execute(query).await
    }
  }

  "PrintingDeprecationTracker" should {
    "track deprecated enum values" in  {
      val testEnum = EnumType[Int]("TestEnum", values = List(
        EnumValue("NONDEPRECATED", value = 1),
        EnumValue("DEPRECATED", value = 2, deprecationReason = Some("Removed in 1.0")),
        EnumValue("ALSONONDEPRECATED", value = 3)))

      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("testEnum", OptionType(StringType),
          arguments = Argument("foo", testEnum) :: Nil,
          resolve = _ => None)
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
        Executor(schema, deprecationTracker = DeprecationTracker.print).execute(query).await
      }

      out should include ("Deprecated enum value '2' used of enum 'TestEnum'.")
    }

    "track deprecated fields" in  {
      val testType = ObjectType("TestType", List[Field[Unit, Unit]](
        Field("nonDeprecated", OptionType(StringType), resolve = _ => None),
        Field("deprecated", OptionType(StringType), deprecationReason = Some("Removed in 1.0"), resolve = _ => None)
      ))

      val schema = Schema(testType)
      val Success(query) = QueryParser.parse("{ nonDeprecated deprecated}")

      val out = captureConsoleOut {
        Executor(schema, deprecationTracker = DeprecationTracker.print).execute(query).await
      }

      out should include ("Deprecated field 'deprecated' used at path 'deprecated'.")
    }
  }
}
