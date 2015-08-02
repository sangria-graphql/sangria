package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.util.{Try, Success}

class ProjectorSpec extends WordSpec with Matchers with AwaitSupport {
  case class Product(id: String, variants: List[Variant])
  case class Variant(id: String, relatedProductIds: List[String])

  case class ProductDefer(productIds: List[String]) extends Deferred[List[Right[String, Product]]]

  val VariantType = ObjectType("Variant", () => List[Field[Unit, Variant]](
    Field("id", IDType, resolve = _.value.id),
    Field("typeId", StringType, resolve = NoProjection(_ => "variant")),
    Field("relatedProducts", ListType(ProductType), resolve = Projection("rp", Projector(1, (ctx, projected) => projected match {
      case Vector(ProjectedName("id", _)) => Value(ctx.value.relatedProductIds map (Left(_)))
      case _ => ProductDefer(ctx.value.relatedProductIds)
    })))
  ))

  val ProductType: ObjectType[Unit, Either[String, Product]] =
    ObjectType("Product", List[Field[Unit, Either[String, Product]]](
      Field("id", IDType, resolve = _.value.fold(identity, _.id)),
      Field("typeId", StringType, resolve = NoProjection(_ => "product")),
      Field("variants", ListType(VariantType), resolve = _.value.right.get.variants)
    ))

  val QueryType = ObjectType("Query", List[Field[Ctx, Unit]](
    Field("products", ListType(ProductType), resolve = _.ctx.products map (Right(_))),
    Field("projectAll", ListType(ProductType), resolve = Projector((ctx, proj) => {
      ctx.ctx.allProjections = proj
      ctx.ctx.products map (Right(_))
    })),
    Field("projectOne", ListType(ProductType), resolve = Projector(1, (ctx, proj) => {
      ctx.ctx.oneLevelprojections = proj
      ctx.ctx.products map (Right(_))
    }))
  ))

  val schema = Schema(QueryType)

  class Ctx {
    val products: List[Product] = List(
      Product("1", List(
        Variant("1", Nil),
        Variant("2", List("1", "2"))
      )),
      Product("2", List(
        Variant("1", Nil)
      ))
    )

    var allProjections: Vector[ProjectedName] = Vector.empty
    var oneLevelprojections: Vector[ProjectedName] = Vector.empty
  }

  class ProductResolver(ctx: Ctx) extends DeferredResolver {
    override def resolve(deferred: List[Deferred[Any]]) = Future.fromTry(Try(deferred map {
      case ProductDefer(ids) => ids map (id => Right(ctx.products.find(_.id == id).get))
    }))
  }

  "Projector" should {
    "project all fields except explicitly marked with `NoProjection`" in {
      val Success(query) = QueryParser.parse(
        """
          {
            projectAll {
              id
              typeId
              variants {
                id
                typeId
                relatedProducts {
                  id
                  typeId
                  variants {
                    id
                  }
                }
              }
            }
            projectOne {
              id
              typeId
              variants {
                id
                typeId
              }
            }
          }
        """)

      val ctx = new Ctx
      val resolver = new ProductResolver(ctx)

      Executor(schema, userContext = ctx, deferredResolver = resolver).execute(query).await should be (
        Map("data" ->
          Map(
            "projectAll" ->
              List(
                Map(
                  "id" -> "1",
                  "typeId" -> "product",
                  "variants" -> List(
                    Map(
                      "id" -> "1",
                      "typeId" -> "variant",
                      "relatedProducts" -> Nil),
                    Map(
                      "id" -> "2",
                      "typeId" -> "variant",
                      "relatedProducts" -> List(
                        Map(
                          "id" -> "1",
                          "typeId" -> "product",
                          "variants" -> List(
                            Map("id" -> "1"),
                            Map("id" -> "2"))),
                        Map(
                          "id" -> "2",
                          "typeId" -> "product",
                          "variants" -> List(
                            Map("id" -> "1"))))))),
                Map(
                  "id" -> "2",
                  "typeId" -> "product",
                  "variants" -> List(
                    Map(
                      "id" -> "1",
                      "typeId" -> "variant",
                      "relatedProducts" -> Nil)))),
          "projectOne" ->
            List(
              Map(
                "id" -> "1",
                "typeId" -> "product",
                "variants" -> List(
                  Map(
                    "id" -> "1",
                    "typeId" -> "variant"),
                  Map(
                    "id" -> "2",
                    "typeId" -> "variant"))),
              Map(
                "id" -> "2",
                "typeId" -> "product",
                "variants" -> List(
                  Map(
                    "id" -> "1",
                    "typeId" -> "variant")))))))

      ctx.allProjections should be (
        Vector(
          ProjectedName("id", Vector.empty),
          ProjectedName("variants", Vector(
            ProjectedName("id", Vector.empty),
            ProjectedName("rp", Vector(
              ProjectedName("id", Vector.empty),
              ProjectedName("variants", Vector(
                ProjectedName("id", Vector.empty)))))))))

      ctx.oneLevelprojections should be (
        Vector(
          ProjectedName("id", Vector.empty),
          ProjectedName("variants", Vector.empty)))
    }
  }
}