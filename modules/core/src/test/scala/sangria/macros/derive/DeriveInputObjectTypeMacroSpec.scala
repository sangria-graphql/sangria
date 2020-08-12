package sangria.macros.derive

import sangria.execution.Executor
import sangria.introspection.{IntrospectionInputObjectType, IntrospectionParser}
import sangria.marshalling.ScalaInput._
import sangria.schema._
import sangria.macros._
import sangria.util.FutureResultSupport
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeriveInputObjectTypeMacroSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  case class TestInputObj(id: String, list: List[String], excluded: Option[List[Option[Int]]])

  @GraphQLName("MyInput")
  @GraphQLDescription("My type!")
  case class TestInputObjAnnotated(
    @GraphQLDescription("my id")
    id: String,

    @GraphQLName("myList")
    list: List[String],

    @GraphQLExclude
    excluded: Int = 123)

  case class A(id: Int, b: Option[B])
  case class B(name: String, a: A, b: Option[B])

  case class TestDeeper(s: String, @GraphQLDefault(123) foo: Int = 456)
  case class TestNested(
    stub: TestDeeper,
    @GraphQLDefault(scalaInput(List(3, 4, 5)))
    name: List[Int],
    @GraphQLDefault(Some(List(TestDeeper("aa", 1))): Option[List[TestDeeper]])
    deeper: Option[List[TestDeeper]])

  case class TestDefaults(
    id: String = "fgh",
    optNone: Option[String] = None,
    list: Int = 324,
    nested: TestNested,
    nestedDef: Option[List[TestNested]] = Some(List(TestNested(TestDeeper("ee", 1), List(1), None), TestNested(TestDeeper("ff", 1), List(1), None))))

  case class TestObjectWithIdType(
    @GraphQLInputType(IDType)
    id: String
  )

  case class TestObjectWithOptionalIdType(
    @GraphQLInputType(OptionInputType(IDType))
    id: Option[String])

  case class InnerClass(someValue: String)

  private val innerClassType = deriveInputObjectType[InnerClass]()

  case class TestNestedObjectGivenFieldWithoutImplicit(
    @GraphQLInputType(innerClassType)
    inner: Option[InnerClass])

  "InputObjectType derivation" should {
    "use class name and have no description by default" in {
      val tpe = deriveInputObjectType[TestInputObj]()

      tpe.name should be ("TestInputObj")
      tpe.description should be (None)
    }

    "allow to change name and description with config" in {
      val tpe = deriveInputObjectType[TestInputObj](
        InputObjectTypeName("Foo"),
        InputObjectTypeDescription("my desc"))

      tpe.name should be ("Foo")
      tpe.description should be (Some("my desc"))
    }

    "allow to change name and description with annotations" in {
      val tpe = deriveObjectType[Unit, TestInputObjAnnotated]()

      tpe.name should be ("MyInput")
      tpe.description should be (Some("My type!"))
    }

    "prioritize config over annotation for name and description" in {
      val tpe = deriveObjectType[Unit, TestInputObjAnnotated](
        ObjectTypeName("Foo"),
        ObjectTypeDescription("my desc"))

      tpe.name should be ("Foo")
      tpe.description should be (Some("my desc"))
    }

    "expose case class fields" in {
      val tpe = deriveInputObjectType[TestInputObj]()

      tpe.fields.sortBy(_.name).map(f => f.name -> f.fieldType) should be (List(
        "excluded" -> OptionInputType(ListInputType(OptionInputType(IntType))),
        "id" -> StringType,
        "list" -> ListInputType(StringType)))
    }

    "validate known field names" in {
      """deriveInputObjectType[TestInputObj](IncludeInputFields("id", "list1"))""" shouldNot compile
      """deriveInputObjectType[TestInputObj](ExcludeInputFields("id1"))""" shouldNot compile
      """deriveInputObjectType[TestInputObj](DocumentInputField("id1", "foo"))""" shouldNot compile
      """deriveInputObjectType[TestInputObj](RenameInputField("id1", "foo"))""" shouldNot compile
      """deriveInputObjectType[TestInputObj](ReplaceInputField("id1", InputField("id1", IntType)))""" shouldNot compile
    }

    "respect whitelist and blacklist provided via config" in {
      val tpe = deriveInputObjectType[TestInputObj](
        IncludeInputFields("id", "list"),
        ExcludeInputFields("list"))

      tpe.fields should have size 1
      tpe.fields(0).name should be ("id")
    }

    "respect blacklist provided via annotations" in {
      val tpe = deriveInputObjectType[TestInputObjAnnotated](
        IncludeInputFields("id", "list", "excluded"),
        ExcludeInputFields("id"))

      tpe.fields should have size 1
      tpe.fields(0).name should be ("myList")
    }

    "allow field names transformation" in {
      val tpe = deriveInputObjectType[TestInputObjAnnotated](
        TransformInputFieldNames(_.toUpperCase))

      tpe.fields.map(_.name) should (
        have(size(2)) and
        contain("ID") and
        contain("MYLIST"))

      val transformer2 = (s: String) => s.zipWithIndex.map {
        case (c, i) if i % 2 == 0 => c.toLower
        case (c, _) => c.toUpper
      }.mkString("")

      val tpe2 = deriveInputObjectType[TestInputObjAnnotated](
        TransformInputFieldNames(transformer2))

      tpe2.fields.map(_.name) should (
        have(size(2)) and
        contain("iD") and
        contain("mYlIsT"))
    }

    "allow to set name and description with config" in {
      val tpe = deriveInputObjectType[TestInputObj](
        DocumentInputField("id", "the object ID"),
        RenameInputField("id", "identifier"),
        RenameInputField("list", "colors"),
        DocumentInputField("list", "my colors"))

      tpe.fields should have size 3

      tpe.fields(0).name should be ("identifier")
      tpe.fields(0).description should be (Some("the object ID"))
      tpe.fields(0).fieldType should be (StringType)

      tpe.fields(1).name should be ("colors")
      tpe.fields(1).description should be (Some("my colors"))
      tpe.fields(1).fieldType should be (ListInputType(StringType))

      tpe.fields(2).name should be ("excluded")
      tpe.fields(2).description should be (None)
      tpe.fields(2).fieldType should be (OptionInputType(ListInputType(OptionInputType(IntType))))
    }

  }
}
