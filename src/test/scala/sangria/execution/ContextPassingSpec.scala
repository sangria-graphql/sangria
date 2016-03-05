package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.FutureResultSupport

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

class ContextPassingSpec extends WordSpec with Matchers with FutureResultSupport {
  trait ColorComponent {
    def color = "green"
  }

  trait NameComponent {
    def name = "foo"
  }

  trait PersonComponent {
    this: NameComponent ⇒

    def fullName = name + " bar"
  }

  class Cake extends ColorComponent with NameComponent with PersonComponent

  val ColorType = ObjectType("Color", fields[ColorComponent with NameComponent, Unit](
    Field("colorName", StringType, resolve = _.ctx.color),
    Field("name", StringType, resolve = _.ctx.name)))

  val NameType = ObjectType("Name", fields[NameComponent, Unit](
    Field("name", StringType, resolve = _.ctx.name)))

  val PersonType = ObjectType("Person", fields[PersonComponent, Unit](
    Field("fullName", StringType, resolve = _.ctx.fullName),
    Field("name", NameType, resolve = _ ⇒ ())))


  val QueryType = ObjectType("Query", fields[Cake, Unit](
    Field("color", ColorType, resolve = _ ⇒ ()),
    Field("person", PersonType, resolve = _ ⇒ ())
  ))

  val schema = Schema(QueryType)

  "Context" should {
    "should respect inheritance" in {
      val Success(doc) = QueryParser.parse("""
        {
          color {name, colorName}
          person {
            name {name}
            fullName
          }
        }
        """)

      Executor.execute(schema, doc, userContext = new Cake).await should be (Map(
        "data" → Map(
          "color" → Map(
            "name" → "foo",
            "colorName" → "green"),
          "person" → Map(
            "name" → Map("name" → "foo"),
            "fullName" → "foo bar"))))
    }
  }

}
