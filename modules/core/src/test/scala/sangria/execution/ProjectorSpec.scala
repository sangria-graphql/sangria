package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.FutureResultSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

class ProjectorSpec extends WordSpec with Matchers with FutureResultSupport {
  case class Product(id: String, variants: List[Variant])
  case class Variant(id: String, relatedProductIds: List[String])

  case class ProductDefer(productIds: List[String]) extends Deferred[List[Right[String, Product]]]

  val ProductAttributeType = InterfaceType("ProductAttribute", fields[Unit, (String, Any)](
    Field("name", StringType, resolve = _.value._1)))

  val StringAttributeType = ObjectType("StringAttribute", interfaces[Unit, (String, Any)](ProductAttributeType), fields[Unit, (String, Any)](
    Field("name", StringType, resolve = _.value._1),
    Field("strValue", StringType, resolve = _.value._2.asInstanceOf[String]))).withInstanceCheck((v, _, _) => v.asInstanceOf[(String, Any)]._2.isInstanceOf[String])

  val IntAttributeType = ObjectType("IntAttribute", interfaces[Unit, (String, Any)](ProductAttributeType), fields[Unit, (String, Any)](
    Field("name", StringType, resolve = _.value._1),
    Field("intValue", IntType, resolve = _.value._2.asInstanceOf[Int]))).withInstanceCheck((v, _, _) => v.asInstanceOf[(String, Any)]._2.isInstanceOf[Int])

  val VariantType = ObjectType("Variant", () => fields[Unit, Variant](
    Field("id", IDType, resolve = _.value.id),
    Field("attributes", ListType(ProductAttributeType), resolve = _ => List("foo" -> "hello", "bar" -> 123)),
    Field("mixed", StringType,
      tags = ProjectionName("mixed1") :: ProjectionName("mixed2") :: Nil,
      resolve = _.value.id),
    Field("typeId", StringType, tags = ProjectionExclude :: Nil, resolve = _ => "variant"),
    Field("relatedProducts", ListType(ProductType),
      tags = ProjectionName("rp") :: Nil,
      resolve = Projector(1, (ctx, projected) => projected match {
        case Vector(ProjectedName("id", _)) => Value(ctx.value.relatedProductIds map (Left(_)))
        case _ => ProductDefer(ctx.value.relatedProductIds)
      }))
  ))

  val ProductType: ObjectType[Unit, Either[String, Product]] =
    ObjectType("Product", List[Field[Unit, Either[String, Product]]](
      Field("id", IDType, resolve = _.value.fold(identity, _.id)),
      Field("variantIds", ListType(IDType),
        tags = ProjectionName("masterVariant.id") :: ProjectionName("variants.id") :: Nil,
        resolve = _ => Nil),
      Field("typeId", StringType, tags = ProjectionExclude :: Nil, resolve = _ => "product"),
      Field("masterVariant", VariantType,
        tags = ProjectionName("master1") :: ProjectionName("master2") :: Nil,
        resolve = _.value.right.get.variants.head),
      Field("variants", ListType(VariantType), resolve = _.value.right.get.variants.tail)
    ))

  val QueryType = ObjectType("Query", fields[Ctx, Unit](
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

  val schema = Schema(QueryType, additionalTypes = StringAttributeType :: IntAttributeType :: Nil)

  trait WithProducts {
    def products: List[Product]
  }

  class Ctx extends WithProducts {
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

  class ProductResolver extends DeferredResolver[WithProducts] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: WithProducts, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
      case ProductDefer(ids) =>
        Future.fromTry(Try(ids map (id => Right(ctx.products.find(_.id == id).get))))
    }
  }

  def compareUnorderedProjectedNames(x: ProjectedName, y: ProjectedName): Boolean =
    (x.name == y.name) &&
    (x.children.size == y.children.size) &&
    (x.children.sortBy(_.name).zip(y.children.sortBy(_.name)).forall {
      case (xc, yc) => compareUnorderedProjectedNames(xc, yc)
    })

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
                attributes {
                  name

                  ... on StringAttribute {strValue}
                  ... on IntAttribute {intValue}
                }
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

      Executor.execute(schema, query, ctx, deferredResolver = new ProductResolver).await should be (
        Map("data" ->
          Map(
            "projectAll" ->
              List(
                Map(
                  "id" -> "1",
                  "typeId" -> "product",
                  "variants" -> List(
                    Map(
                      "id" -> "2",
                      "attributes" -> Vector(
                        Map("name" -> "foo", "strValue" -> "hello"),
                        Map("name" -> "bar", "intValue" -> 123)),
                      "typeId" -> "variant",
                      "relatedProducts" -> List(
                        Map(
                          "id" -> "1",
                          "typeId" -> "product",
                          "variants" -> List(
                            Map("id" -> "2"))),
                        Map(
                          "id" -> "2",
                          "typeId" -> "product",
                          "variants" -> Nil))))),
                Map(
                  "id" -> "2",
                  "typeId" -> "product",
                  "variants" -> Nil)),
          "projectOne" ->
            List(
              Map(
                "id" -> "1",
                "typeId" -> "product",
                "variants" -> List(
                  Map(
                    "id" -> "2",
                    "typeId" -> "variant"))),
              Map(
                "id" -> "2",
                "typeId" -> "product",
                "variants" -> Nil)))))

      val expected = Vector(
        ProjectedName("id", Vector.empty),
        ProjectedName("variants", Vector(
          ProjectedName("id", Vector.empty),
          ProjectedName("attributes", Vector(
            ProjectedName("strValue", Vector.empty),
            ProjectedName("name", Vector.empty),
            ProjectedName("intValue", Vector.empty))),
          ProjectedName("rp", Vector(
            ProjectedName("id", Vector.empty),
            ProjectedName("variants", Vector(
              ProjectedName("id", Vector.empty))))))))

      ctx.allProjections.zip(expected).map {
        case (x, y) => compareUnorderedProjectedNames(x, y)
      }

      ctx.oneLevelprojections should be (
        Vector(
          ProjectedName("id", Vector.empty),
          ProjectedName("variants", Vector.empty)))
    }

    "handle multiple projected names" in {
      val Success(query) = QueryParser.parse(
        """
          {
            projectAll {
              id
              variantIds
              masterVariant {
                mixed
              }
              variants {
                id
                mixed
              }
            }

            projectOne {
              id
              variantIds
              masterVariant {
                mixed
              }
              variants {
                id
                mixed
              }
            }
          }
        """)

      val ctx = new Ctx

      Executor.execute(schema, query, ctx, deferredResolver = new ProductResolver).await should be (
        Map("data" ->
          Map(
            "projectAll" -> Vector(
              Map(
                "id" -> "1",
                "variantIds" -> Nil,
                "masterVariant" -> Map("mixed" -> "1"),
                "variants" -> Vector(Map("id" -> "2", "mixed" -> "2"))),
              Map(
                "id" -> "2",
                "variantIds" -> Nil,
                "masterVariant" -> Map("mixed" -> "1"),
                "variants" -> Nil)),
            "projectOne" -> Vector(
              Map(
                "id" -> "1",
                "variantIds" -> Nil,
                "masterVariant" -> Map("mixed" -> "1"),
                "variants" -> Vector(Map("id" -> "2", "mixed" -> "2"))),
              Map(
                "id" -> "2",
                "variantIds" -> Nil,
                "masterVariant" -> Map("mixed" -> "1"),
                "variants" -> Nil)))))

      ctx.allProjections should be (
        Vector(
          ProjectedName("id", Vector.empty),
          ProjectedName("masterVariant.id", Vector.empty),
          ProjectedName("variants.id", Vector.empty),
          ProjectedName("master1", Vector(
              ProjectedName("mixed1", Vector.empty),
              ProjectedName("mixed2", Vector.empty))),
          ProjectedName("master2", Vector(
              ProjectedName("mixed1", Vector.empty),
              ProjectedName("mixed2", Vector.empty))),
          ProjectedName("variants",
            Vector(
              ProjectedName("id", Vector.empty),
              ProjectedName("mixed1", Vector.empty),
              ProjectedName("mixed2", Vector.empty)))))

      ctx.oneLevelprojections should be (
        Vector(
          ProjectedName("id", Vector.empty),
          ProjectedName("masterVariant.id", Vector.empty),
          ProjectedName("variants.id", Vector.empty),
          ProjectedName("master1", Vector.empty),
          ProjectedName("master2", Vector.empty),
          ProjectedName("variants", Vector.empty)))
    }
  }
}
