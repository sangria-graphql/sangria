package sangria.schema

import language.existentials

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.schema.SchemaChange._
import sangria.macros._
import sangria.util.DebugUtil

import scala.reflect.ClassTag

class SchemaComparatorSpec extends WordSpec with Matchers {
  "SchemaComparator" should {
    val QueryType = ObjectType("Query", fields[Unit, Unit](
      Field("field1", OptionType(StringType), resolve = _ ⇒ "foo")))

    "should detect if a type was removed or added" in {
      val type1 = ObjectType("Type1", fields[Unit, Unit](
        Field("field1", OptionType(StringType), resolve = _ ⇒ "foo")))
      val type2 = ObjectType("Type2", fields[Unit, Unit](
        Field("field1", OptionType(StringType), resolve = _ ⇒ "foo")))

      val oldSchema = Schema(QueryType, additionalTypes = type1 :: type2 :: Nil)
      val newSchema = Schema(QueryType, additionalTypes = type2 :: Nil)

      assertChanges(newSchema.compare(oldSchema),
        change[TypeRemoved]("`Type1` type was removed"))

      assertChanges(oldSchema.compare(newSchema),
        change[TypeAdded]("`Type1` type was added"))

      oldSchema.compare(oldSchema) should be (Vector.empty)
    }

    "should detect if a type changed its kind" in checkChanges(
      graphql"""
        interface Type1 {field1: String}
      """,

      graphql"""
        type ObjectType {field1: String}
        union Type1 = ObjectType
      """,

      change[TypeAdded]("`ObjectType` type was added"),
      change[TypeKindChanged]("`Type1` changed from an Interface type to a Union type"))
  }

  def change[T : ClassTag](description: String) =
    implicitly[ClassTag[T]].runtimeClass → description

  def checkChanges(oldDoc: Document, newDoc: Document, expectedChanges: (Class[_], String)*) = {
    val queryType =
      graphql"""
        type Query {
          field1: String
        }
      """

    val oldSchema = Schema.buildFromAst(oldDoc merge queryType)
    val newSchema = Schema.buildFromAst(newDoc merge queryType)

    assertChanges(newSchema.compare(oldSchema), expectedChanges: _*)
  }

  def assertChanges(actualChanges: Vector[SchemaChange], expectedChanges: (Class[_], String)*) = {
    val actualRendered = actualChanges.map(c ⇒ s"  * ${c.getClass.getSimpleName}: ${c.description}").mkString("\n")

    withClue(s"Actual changes:\n$actualRendered\n") {
      actualChanges should have size expectedChanges.size

      val notFound = expectedChanges.filter(expectedChange ⇒
        !actualChanges.exists(ac ⇒ expectedChange._1.isAssignableFrom(ac.getClass) && ac.description == expectedChange._2))

      if (notFound.nonEmpty) {
        val str = notFound.map(nf ⇒ s"  * ${nf._1.getSimpleName}: ${nf._2}").mkString("\n")

        fail(s"Changes not found:\n $str")
      }
    }
  }
}
