package sangria.macros.derive

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.macros._
import sangria.schema._
import sangria.util.{DebugUtil, FutureResultSupport}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class DeriveWrappedObjectTypeMacroSpec extends WordSpec with Matchers with FutureResultSupport {
  case class MergedValue[T](value: T, extras: Map[String, Any])

  object MergedValue {
    implicit object MergedWrapper extends ObjectWrapper[MergedValue] {
      def get[T](wrapper: MergedValue[T]) = wrapper.value
      def wrap[Ctx, Old, New](ctx: Context[Ctx, MergedValue[Old]], content: New) =
        ctx.value.copy(
          value = content,
          extras =
            if (ctx.field.name == "product")
              ctx.value.extras.getOrElse("product", Map.empty).asInstanceOf[Map[String, Any]]
            else ctx.value.extras)
    }
  }

  case class Product(id: Int)
  case class Category(id: Int, product: Option[Product])

  class Repo {
    def loadCategory: Future[Category] =
      Future.successful(
        Category(1, Some(Product(2))))

    def loadExtras: Future[Map[String, Any]] =
      Future.successful(
        Map("name" → "Cat 1", "product" → Map(
          "name" → "Prod 2")))
  }

  "Derivation with wrapper" should {
    "generate ObjectType based on the wrapped type" in {
      implicit val product: ObjectType[Repo, MergedValue[Product]] =
        deriveWrappedObjectType[Repo, MergedValue, Product]()

      implicit val category: ObjectType[Repo, MergedValue[Category]] =
        deriveWrappedObjectType[Repo, MergedValue, Category]()

      val schema =
        Schema(ObjectType("Query", fields[Repo, Unit](
          Field("category", category, resolve = c ⇒
            Action.sequence[Repo, Any](Seq(FutureValue[Repo, Category](c.ctx.loadCategory), FutureValue[Repo, Map[String, Any]](c.ctx.loadExtras)))
              .map {case Seq(cat: Category, extras: Map[String, Any]) ⇒ MergedValue(cat, extras)}))))

      val extension =
        gql"""
          extend type Category {
            name: String!
          }

          extend type Product {
            name: String!
          }
        """

      val builder = AstSchemaBuilder.resolverBased[Repo](
        FieldResolver {
          case (_, field) ⇒ _.value.asInstanceOf[MergedValue[_]].extras(field.name)
        })

      val extendedSchema = schema.extend(extension, builder)

      val query =
        gql"""
          {
            category {
              id
              name
              product {
                id
                name
              }
            }
          }
        """

      Executor.execute(extendedSchema, query, new Repo).await should be (
        Map(
          "data" → Map(
            "category" → Map(
              "id" → 1,
              "name" → "Cat 1",
              "product" → Map(
                "id" → 2,
                "name" → "Prod 2")))))
    }
  }
}
