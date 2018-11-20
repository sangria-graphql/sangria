package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.macros._
import sangria.renderer.SchemaRenderer
import sangria.util.{FutureResultSupport, StringMatchers}

class SchemaTransformationSpec extends WordSpec  with Matchers with FutureResultSupport with StringMatchers {

  private val testSchema = Schema.buildFromAst(graphql"""
        type Query {
          someObject: SomeObject
        }

        interface SomeInterface {
          publicInterfaceField: String
          privateInterfaceField: String
        }

        type SomeObject implements SomeInterface {
          privateObjectField: String
          publicObjectField: String
        }
      """)

  "filterFields" should {
    "filter fields on ObjectTypes" in {
      val filteredSchema = testSchema.filterFields(_.filterNot(_.name == "privateObjectField"))
      SchemaRenderer.renderSchema(filteredSchema) should not include "privateObjectField"
    }

    "filter fields on Interfaces" in {
      val filteredSchema = testSchema.filterFields(_.filterNot(_.name == "privateInterfaceField"))
      SchemaRenderer.renderSchema(filteredSchema) should not include "privateInterfaceField"
    }
  }
}
