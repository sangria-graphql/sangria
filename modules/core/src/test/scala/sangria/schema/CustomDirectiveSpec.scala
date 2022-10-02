package sangria.schema

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast
import sangria.renderer.QueryRenderer
import sangria.schema.DirectiveLocationValue.On
import sangria.util.tag.@@

class CustomDirectiveSpec extends AnyWordSpec with Matchers {

  case class Domain(value: Int)

  private val AllDirective = new ast.Directive("field-directive")
    with On[DirectiveLocationValue.Field.type with DirectiveLocationValue.ArgumentDefinition.type]

//    with OnField
//    with OnArgument
//    with OnObjectType
//    with OnInterfaceType

  private val FieldDirective: ast.Directive with On[DirectiveLocationValue.Field.type] =
    new ast.Directive("field-directive") with On[DirectiveLocationValue.Field.type]

//  private val ArgumentDirective = new ast.Directive("arg-directive") with OnArgument
//
//  private val ObjectDirective = new ast.Directive("object-directive") with OnObjectType
//
//  private val InterfaceDirective = new ast.Directive("interface-directive") with OnInterfaceType
//
//  private val CustomDirective = ast.Directive("custom-directive")

  private val resolve: Context[Unit, Domain] => Action[Unit, Int] = _.value.value

  "custom directive" when {
    "in context of a Field" should {
      "be applied if marked with OnField" in {
        fields[Unit, Domain](
          Field("field", IntType, resolve = resolve, astDirectives = Vector(FieldDirective)))

        fields[Unit, Domain](
          Field("field", IntType, resolve = resolve, astDirectives = Vector(AllDirective)))

        fields[Unit, Domain](
          Field(
            "field",
            IntType,
            resolve = resolve,
            astDirectives = Vector(FieldDirective, AllDirective)))

        Field("field", IntType, resolve = resolve, astDirectives = Vector(FieldDirective)): Field[
          Unit,
          Domain]

//        val field = (Field("field", IntType, resolve = resolve): Field[Unit, Domain])
//          .withDirective(FieldDirective)
//          .withDirectives(FieldDirective, AllDirective)
//        field.astDirectives should be(Vector(FieldDirective, FieldDirective, AllDirective))
      }

      "not be applied if not marked with OnField" in {
        assertTypeError("""
            |fields[Unit, Domain](
            |  Field("field", IntType, resolve = resolve, astDirectives = Vector(CustomDirective)))
            |""".stripMargin)

        assertTypeError("""
            |val field: Field[Unit, Domain] =
            |  Field("field", IntType, resolve = resolve, astDirectives = Vector(CustomDirective))
            |""".stripMargin)
      }

//      "be combined with the @deprecated directive" in {
//        val field = (Field(
//          "field",
//          IntType,
//          resolve = resolve,
//          deprecationReason = Some("use field2")): Field[Unit, Domain])
//          .withDirective(FieldDirective)
//
//        field.astDirectives should be(Vector(FieldDirective))
//        QueryRenderer.renderPretty(field.toAst) should equal(
//          """field: Int! @field-directive @deprecated(reason: "use field2")""")
//      }
    }
  }

//  "in context of an Argument" should {
//    "be applied if marked with OnArgument" in {
//      Argument("name", IntType, 42, astDirectives = Vector(ArgumentDirective))
//      Argument("name", IntType, 42, astDirectives = Vector(AllDirective))
//      Argument("name", IntType, 42).withDirective(ArgumentDirective)
//      val arg = Argument("name", IntType, 42)
//        .withDirective(AllDirective)
//        .withDirectives(ArgumentDirective, ArgumentDirective)
//      arg.astDirectives should be(Vector(AllDirective, ArgumentDirective, ArgumentDirective))
//    }
//
//    "not be applied if not marked with OnArgument" in {
//      assertTypeError("""
//          |Argument("name", IntType, 42, astDirectives = Vector(FieldDirective))
//          |""".stripMargin)
//      assertTypeError("""
//          |Argument("name", IntType, 42).withDirective(FieldDirective)
//          |""".stripMargin)
//    }
//  }
//
//  "in context of an ObjectType" should {
//    "be applied if marked with OnObjectType" in {
//      val obj = ObjectType[Unit, Domain]("name", fields[Unit, Domain]())
//        .withDirective(ObjectDirective)
//        .withDirectives(AllDirective, ObjectDirective)
//      obj.astDirectives should be(Vector(ObjectDirective, AllDirective, ObjectDirective))
//    }
//
//    "not be applied if not marked with OnObjectType" in {
//      assertTypeError("""
//          |ObjectType[Unit, Domain]("name", fields[Unit, Domain]()).withDirective(CustomDirective)
//          |""".stripMargin)
//      assertTypeError("""
//          |ObjectType[Unit, Domain]("name", fields[Unit, Domain]()).withDirective(FieldDirective)
//          |""".stripMargin)
//    }
//  }
//
//  "in context of an InterfaceType" should {
//    "be applied if marked with OnInterfaceType" in {
//      val interface = InterfaceType[Unit, Domain]("name", fields[Unit, Domain]())
//        .withDirective(InterfaceDirective)
//        .withDirectives(AllDirective, InterfaceDirective)
//      interface.astDirectives should be(
//        Vector(InterfaceDirective, AllDirective, InterfaceDirective))
//    }
//
//    "not be applied if not marked with OnObjectType" in {
//      assertTypeError("""
//          |InterfaceType[Unit, Domain]("name", fields[Unit, Domain]()).withDirective(CustomDirective)
//          |""".stripMargin)
//      assertTypeError("""
//          |InterfaceType[Unit, Domain]("name", fields[Unit, Domain]()).withDirective(FieldDirective)
//          |""".stripMargin)
//    }
//  }
}
