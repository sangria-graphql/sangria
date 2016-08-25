package sangria.macros.derive

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.introspection.{IntrospectionInputObjectType, IntrospectionObjectType, IntrospectionParser}
import sangria.marshalling.FromInput
import sangria.marshalling.ScalaInput._
import sangria.schema._
import sangria.macros._
import sangria.renderer.SchemaRenderer
import sangria.util.FutureResultSupport
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global

class DeriveInputObjectTypeMacroSpec extends WordSpec with Matchers with FutureResultSupport {
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

      tpe.fields.sortBy(_.name).map(f ⇒ f.name → f.fieldType) should be (List(
        "excluded" → OptionInputType(ListInputType(OptionInputType(IntType))),
        "id" → StringType,
        "list" → ListInputType(StringType)))
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

    "allow to set name and description with annotations" in {
      val tpe = deriveInputObjectType[TestInputObjAnnotated]()

      tpe.fields should have size 2

      tpe.fields(0).name should be ("id")
      tpe.fields(0).description should be (Some("my id"))

      tpe.fields(1).name should be ("myList")
      tpe.fields(1).description should be (None)
    }

    "prioritize field config name and description" in {
      val tpe = deriveInputObjectType[TestInputObjAnnotated](
        RenameInputField("list", "fooBar"),
        DocumentInputField("id", "new descr"))

      tpe.fields should have size 2

      tpe.fields(0).name should be ("id")
      tpe.fields(0).description should be (Some("new descr"))

      tpe.fields(1).name should be ("fooBar")
      tpe.fields(1).description should be (None)
    }

    "be able handle recursive input types with replaced fields" in {
      class Query {
        @GraphQLField def foo(a: A) = "" + a
      }

      object MyJsonProtocol extends DefaultJsonProtocol {
        implicit lazy val BFormat: JsonFormat[B] = lazyFormat(jsonFormat3(B.apply))
        implicit lazy val AFormat: JsonFormat[A] = lazyFormat(jsonFormat2(A.apply))
      }

      import MyJsonProtocol._
      import sangria.marshalling.sprayJson._

      implicit lazy val AType: InputObjectType[A] = deriveInputObjectType[A](
        ReplaceInputField("b", InputField("b", OptionInputType(BType))))

      implicit lazy val BType: InputObjectType[B] = deriveInputObjectType[B](
        ReplaceInputField("a", InputField("a", AType)),
        ReplaceInputField("b", InputField("b", OptionInputType(BType))))

      val QueryType = deriveObjectType[Unit, Query]()

      val schema = Schema(QueryType)

      val query =
        graphql"""{foo(a: {id: 21, b: {name: "it's b", a: {id: 34}, b: {name: "another", a: {id: 56}}}})}"""

      Executor.execute(schema, query, root = new Query).await should be (
        JsObject("data" → JsObject("foo" →
          JsString("A(21,Some(B(it's b,A(34,None),Some(B(another,A(56,None),None)))))"))))
    }

    "be able to use default values" in {
      class Query {
        @GraphQLField def foo(a: TestDefaults) = "" + a
      }

      object MyJsonProtocol extends DefaultJsonProtocol {
        implicit val TestDeeperFormat = jsonFormat2(TestDeeper.apply)
        implicit val TestNestedFormat = jsonFormat3(TestNested.apply)
        implicit val TestDefaultsFormat = jsonFormat5(TestDefaults.apply)
      }

      import MyJsonProtocol._
      import sangria.marshalling.sprayJson._
      import sangria.parser.DeliveryScheme.Throw

      implicit lazy val TestDeeperType = deriveInputObjectType[TestDeeper]()
      implicit lazy val TestNestedType = deriveInputObjectType[TestNested]()
      implicit lazy val TestDefaultsType = deriveInputObjectType[TestDefaults]()

      TestDeeperType.fields.sortBy(_.name).map(f ⇒ f.name → f.fieldType) should be (List(
        "foo" → OptionInputType(IntType),
        "s" → StringType))

      TestNestedType.fields.sortBy(_.name).map(f ⇒ f.name → f.fieldType) should be (List(
        "deeper" → OptionInputType(ListInputType(TestDeeperType)),
        "name" → OptionInputType(ListInputType(IntType)),
        "stub" → TestDeeperType))

      val QueryType = deriveObjectType[Unit, Query]()

      val schema = Schema(QueryType)

      val query = graphql"""{foo(a: {nested: {stub: {s: "foo"}}})}"""

      Executor.execute(schema, query, root = new Query).await should be (
        JsObject("data" → JsObject("foo" →
          JsString("TestDefaults(fgh,None,324,TestNested(TestDeeper(foo,123),List(3, 4, 5),Some(List(TestDeeper(aa,1)))),Some(List(TestNested(TestDeeper(ee,1),List(1),Some(List(TestDeeper(aa,1)))), TestNested(TestDeeper(ff,1),List(1),Some(List(TestDeeper(aa,1)))))))"))))

      val intro = IntrospectionParser.parse(Executor.execute(schema, sangria.introspection.introspectionQuery, root = new Query).await)

      intro.typesByName("TestDefaults").asInstanceOf[IntrospectionInputObjectType].inputFieldsByName("optNone").defaultValue should be (None)
    }
  }
}
