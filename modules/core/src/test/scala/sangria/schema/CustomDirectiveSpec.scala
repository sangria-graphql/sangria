package sangria.schema

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast
import sangria.renderer.QueryRenderer

class CustomDirectiveSpec extends AnyWordSpec with Matchers {

  case class Domain(value: Int)

  private val CustomDirective1 = ast.Directive("custom-directive-1")
  private val CustomDirective2 = ast.Directive("custom-directive-2")

  private val resolve: Context[Unit, Domain] => Action[Unit, Int] = _.value.value

  "custom directive" when {
    "in context of a Field" should {
      "be applied" in {
        fields[Unit, Domain](
          Field("field", IntType, resolve = resolve, astDirectives = Vector(CustomDirective1)))

        val field = (Field("field", IntType, resolve = resolve): Field[Unit, Domain])
          .withDirective(CustomDirective1)
          .withDirectives(CustomDirective1, CustomDirective2)
        field.astDirectives should be(Vector(CustomDirective1, CustomDirective1, CustomDirective2))
      }

      "be combined with the @deprecated directive" in {
        val field = (Field(
          "field",
          IntType,
          resolve = resolve,
          deprecationReason = Some("use field2")): Field[Unit, Domain])
          .withDirective(CustomDirective1)

        field.astDirectives should be(Vector(CustomDirective1))
        QueryRenderer.renderPretty(field.toAst) should equal(
          """field: Int! @custom-directive-1 @deprecated(reason: "use field2")""")
      }
    }
  }

  "in context of an Argument" should {
    "be applied" in {
      val arg = Argument("name", IntType, 42)
        .withDirective(CustomDirective1)
        .withDirectives(CustomDirective1, CustomDirective2)
      arg.astDirectives should be(Vector(CustomDirective1, CustomDirective1, CustomDirective2))
    }
  }

  "in context of an ObjectType" should {
    "be applied" in {
      val obj = ObjectType[Unit, Domain]("name", fields[Unit, Domain]())
        .withDirective(CustomDirective1)
        .withDirectives(CustomDirective1, CustomDirective2)
      obj.astDirectives should be(Vector(CustomDirective1, CustomDirective1, CustomDirective2))
    }
  }

  "in context of an InterfaceType" should {
    "be applied if marked with OnInterfaceType" in {
      val interface = InterfaceType[Unit, Domain]("name", fields[Unit, Domain]())
        .withDirective(CustomDirective1)
        .withDirectives(CustomDirective1, CustomDirective2)
      interface.astDirectives should be(
        Vector(CustomDirective1, CustomDirective1, CustomDirective2))
    }
  }
}
