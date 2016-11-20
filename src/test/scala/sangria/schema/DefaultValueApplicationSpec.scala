package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.execution.Executor
import sangria.macros._
import sangria.util.FutureResultSupport
import spray.json.JsValue

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

import spray.json._
import sangria.marshalling.sprayJson._

class DefaultValueApplicationSpec extends WordSpec with Matchers with FutureResultSupport {
  "Default value application" should {
    "use default value if argument is not provided" in {
      val assertion = (args: Args) ⇒ args.withArgs(AArg, SizeArg) { (test, size) ⇒
        test should be ("default")
        size should be (42)

        args.argDefinedInQuery(AArg) should be (false)
        args.argOpt(AArg) should be (Some("default"))
      }

      check(graphql"{ test }", "{}", assertion)
      check(graphql"query ($$x: String) { test(a: $$x) }", "{}", assertion)
    }

    "use default value if argument is `null`" in {
      val assertion = (args: Args) ⇒ args.withArgs(AArg) { test ⇒
        test should be ("default")

        args.argOpt(AArg) should be (None)
        args.argDefinedInQuery(AArg) should be (true)
      }

      check(graphql"{ test(a: null) }", "{}", assertion)
      check(graphql"query ($$x: String = null) { test(a: $$x) }", "{}", assertion)
      check(graphql"query ($$x: String) { test(a: $$x) }", """{"x": null}""", assertion)
    }

    "not use default value if value is provided" in {
      val assertion = (args: Args) ⇒ args.withArgs(AArg) { test ⇒
        test should be ("bar")

        args.argOpt(AArg) should be (Some("bar"))
        args.argDefinedInQuery(AArg) should be (true)
      }

      check(graphql"""{ test(a: "bar") }""", "{}", assertion)
      check(graphql"query ($$x: String) { test(a: $$x) }", """{"x": "bar"}""", assertion)
    }

    "default variable value should not be used if variable is set to `null`" in {
      check(graphql"""query ($$x: String = "bar") { test(a: $$x) }""", """{"x": null}""", args ⇒ {
        args.arg(AArg) should be ("bar")

        args.argOpt(AArg) should be (None)
        args.argDefinedInQuery(AArg) should be (true)
      })
    }

    "default variable value should not be used if variable is not set" in {
      check(graphql"""query ($$x: String = "bar") { test(a: $$x) }""", """{}""", args ⇒ {
        args.arg(AArg) should be ("bar")

        args.argOpt(AArg) should be (Some("bar"))
        args.argDefinedInQuery(AArg) should be (true)
      })
    }

    "use default value if field is not provided" in {
      val assertion = (args: Args) ⇒ args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) ⇒
        inp should be (Map("f" → Some("default")))
        inpJson should be ("""{"f": "default"}""".parseJson)
      }

      check(graphql"{ testInp(inp: {}, inpJson: {}) }", "{}", assertion)
      check(graphql"query ($$x: String) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }", "{}", assertion)
    }

    "not use default value if field is `null`" in {
      val assertion = (args: Args) ⇒ args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) ⇒
        inp should be (Map("f" → None))
        inpJson should be ("""{"f": null}""".parseJson)
      }

      check(graphql"{ testInp(inp: {f: null}, inpJson: {f: null}) }", "{}", assertion)
      check(graphql"query ($$x: String = null) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }", "{}", assertion)
      check(graphql"query ($$x: String) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }", """{"x": null}""", assertion)
    }

    "not use default value if field value is provided" in {
      val assertion = (args: Args) ⇒ args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) ⇒
        inp should be (Map("f" → Some("bar")))
        inpJson should be ("""{"f": "bar"}""".parseJson)
      }

      check(graphql"""{ testInp(inp: {f: "bar"}, inpJson: {f: "bar"}) }""", "{}", assertion)
      check(graphql"query ($$x: String) { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }", """{"x": "bar"}""", assertion)
    }

    "default variable value should not be used if variable is set to `null` for a field" in {
      check(graphql"""query ($$x: String = "bar") { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }""", """{"x": null}""",
        args ⇒ args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) ⇒
          inp should be (Map("f" → None))
          inpJson should be ("""{"f": null}""".parseJson)
        })
    }

    "default variable value should not be used if variable is not set for a field" in {
      check(graphql"""query ($$x: String = "bar") { testInp(inp: {f: $$x}, inpJson: {f: $$x}) }""", """{}""",
        args ⇒ args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) ⇒
          inp should be (Map("f" → Some("bar")))
          inpJson should be ("""{"f": "bar"}""".parseJson)
        })
    }

    "set fields to `null` if value is not set, but default is set to null" in {
      check(graphql"""query ($$x: String = "bar", $$y: String = null) { testInp(inp: {f: $$x, fo: $$y}, inpJson: {f: $$x, fo: $$y}) }""", """{}""",
        args ⇒ args.withArgs(InpArg, InpJsonArg) { (inp, inpJson) ⇒
          inp should be (Map("f" → Some("bar"), "fo" → None))
          inpJson should be ("""{"f": "bar", "fo": null}""".parseJson)
        })
    }
  }

  class Ctx {
    var args: Option[Args] = None
  }

  val TestInputJsonType = InputObjectType[JsValue]("TestInput", fields = List(
    InputField("f", OptionInputType(StringType), defaultValue = "default"),
    InputField("fo", OptionInputType(StringType))))

  val TestInputType = InputObjectType("TestInput", fields = List(
    InputField("f", OptionInputType(StringType), defaultValue = "default"),
    InputField("fo", OptionInputType(StringType))))

  val AArg = Argument("a", OptionInputType(StringType), defaultValue = "default")
  val InpArg = Argument("inp", TestInputType)
  val InpJsonArg = Argument("inpJson", TestInputJsonType)
  val SizeArg = Argument("size", OptionInputType(IntType), 42)

  val QueryType = ObjectType("Query", fields[Ctx, Unit](
    Field("test", StringType,
      arguments = AArg :: SizeArg :: Nil,
      resolve = c ⇒ {
        c.ctx.args = Some(c.args)
        "foo"
      }),

    Field("testInp", StringType,
      arguments = InpArg :: InpJsonArg :: Nil,
      resolve = c ⇒ {
        c.ctx.args = Some(c.args)
        "foo"
      })
  ))

  val schema = Schema(QueryType)

  def check[T](query: Document, vars: String, assertions: Args ⇒ T): T = {
    val ctx = new Ctx

    Executor.execute(schema, query, ctx, variables = vars.parseJson).await

    assertions(ctx.args.getOrElse(fail("field was not used in query!")))
  }
}