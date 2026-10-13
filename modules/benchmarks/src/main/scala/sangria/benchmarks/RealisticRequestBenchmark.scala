package sangria.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.execution.{
  ExceptionHandler,
  Executor,
  FieldTag,
  Middleware,
  MiddlewareAfterField,
  MiddlewareBeforeField,
  MiddlewareErrorField,
  MiddlewareQueryContext,
  QueryReducer
}
import sangria.introspection.introspectionQuery
import sangria.marshalling.FromInput.coercedScalaInput
import sangria.marshalling.ScalaInput
import sangria.marshalling.scalaMarshalling._
import sangria.parser.QueryParser
import sangria.schema._

import scala.concurrent.{Await, ExecutionContext, Future}
import java.util.concurrent.atomic.LongAdder
import scala.concurrent.duration._

/** Benchmarks complete requests (parse, validate with all default rules, reduce, execute) against a
  * schema shaped like a typical e-commerce API: an interface, a paginated result wrapper, localized
  * fields taking arguments, batched reference expansion with a deferred resolver, a mutation taking
  * a list of update actions, variables, directives and fragments.
  */
object RealisticRequestBenchmark {
  case class Money(centAmount: Long, currencyCode: String)
  case class Price(value: Money, discounted: Option[Money])
  case class Attribute(name: String, value: String)
  case class Image(url: String, label: Option[String])
  case class Availability(isOnStock: Boolean, availableQuantity: Int)
  case class Variant(
      id: Int,
      sku: String,
      price: Price,
      attributes: List[Attribute],
      images: List[Image],
      availability: Availability)
  case class Category(id: String, key: String, names: Map[String, String], parentId: Option[String])
  case class Product(
      id: String,
      version: Long,
      key: String,
      names: Map[String, String],
      slugs: Map[String, String],
      descriptions: Map[String, String],
      categoryIds: List[String],
      masterVariant: Variant,
      variants: List[Variant])
  case class QueryResult[A](offset: Int, count: Int, total: Long, results: List[A])
  case class LineItem(id: String, productId: String, quantity: Int, price: Money)
  case class Cart(id: String, version: Long, lineItems: List[LineItem], totalPrice: Money)

  case class CategoryRef(id: String) extends Deferred[Option[Category]]
  case class CategoryRefs(ids: List[String]) extends Deferred[List[Category]]

  class Repo(
      val products: Vector[Product],
      val categories: Map[String, Category],
      val scopes: Set[String])

  case class RequiresScope(scope: String) extends FieldTag

  /** Typical middlewares: field metrics, scope based authorization and error reporting. */
  class FieldMetricsMiddleware extends Middleware[Repo] with MiddlewareAfterField[Repo] {
    type QueryVal = Unit
    type FieldVal = Long

    val fieldCount = new LongAdder
    val fieldNanos = new LongAdder
    val queryCount = new LongAdder

    def beforeQuery(context: MiddlewareQueryContext[Repo, _, _]): Unit = queryCount.increment()
    def afterQuery(queryVal: Unit, context: MiddlewareQueryContext[Repo, _, _]): Unit = ()

    def beforeField(
        queryVal: Unit,
        mctx: MiddlewareQueryContext[Repo, _, _],
        ctx: Context[Repo, _]) =
      continue(System.nanoTime())

    def afterField(
        queryVal: Unit,
        fieldVal: Long,
        value: Any,
        mctx: MiddlewareQueryContext[Repo, _, _],
        ctx: Context[Repo, _]): Option[Any] = {
      fieldCount.increment()
      fieldNanos.add(System.nanoTime() - fieldVal)
      None
    }
  }

  class ScopeMiddleware extends Middleware[Repo] with MiddlewareBeforeField[Repo] {
    type QueryVal = Unit
    type FieldVal = Unit

    def beforeQuery(context: MiddlewareQueryContext[Repo, _, _]): Unit = ()
    def afterQuery(queryVal: Unit, context: MiddlewareQueryContext[Repo, _, _]): Unit = ()

    def beforeField(
        queryVal: Unit,
        mctx: MiddlewareQueryContext[Repo, _, _],
        ctx: Context[Repo, _]) = {
      ctx.field.tags.foreach {
        case RequiresScope(scope) if !ctx.ctx.scopes.contains(scope) =>
          throw new IllegalAccessException(s"missing scope $scope")
        case _ =>
      }
      continue
    }
  }

  class ErrorReportingMiddleware extends Middleware[Repo] with MiddlewareErrorField[Repo] {
    type QueryVal = Unit
    type FieldVal = Unit

    val errors = new LongAdder

    def beforeQuery(context: MiddlewareQueryContext[Repo, _, _]): Unit = ()
    def afterQuery(queryVal: Unit, context: MiddlewareQueryContext[Repo, _, _]): Unit = ()

    def beforeField(
        queryVal: Unit,
        mctx: MiddlewareQueryContext[Repo, _, _],
        ctx: Context[Repo, _]) =
      continue

    def fieldError(
        queryVal: Unit,
        fieldVal: Unit,
        error: Throwable,
        mctx: MiddlewareQueryContext[Repo, _, _],
        ctx: Context[Repo, _]): Unit = errors.increment()
  }

  class CategoryResolver extends DeferredResolver[Repo] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Repo, queryState: Any)(implicit
        ec: ExecutionContext): Vector[Future[Any]] =
      deferred.map {
        case CategoryRef(id) => Future.successful(ctx.categories.get(id))
        case CategoryRefs(ids) => Future.successful(ids.flatMap(ctx.categories.get))
        case other => Future.failed(new IllegalArgumentException(s"unknown deferred $other"))
      }
  }

  private def localized[Ctx, T](name: String, get: T => Map[String, String]): Field[Ctx, T] =
    Field(
      name,
      OptionType(StringType),
      arguments = Argument("locale", StringType) :: Nil,
      resolve = c => get(c.value).get(c.arg[String]("locale")))

  val MoneyType: ObjectType[Repo, Money] = ObjectType(
    "Money",
    fields[Repo, Money](
      Field("centAmount", LongType, resolve = _.value.centAmount),
      Field("currencyCode", StringType, resolve = _.value.currencyCode),
      Field("fractionDigits", IntType, resolve = _ => 2)
    )
  )

  val PriceType: ObjectType[Repo, Price] = ObjectType(
    "Price",
    fields[Repo, Price](
      Field("value", MoneyType, resolve = _.value.value),
      Field("discounted", OptionType(MoneyType), resolve = _.value.discounted)))

  val AttributeType: ObjectType[Repo, Attribute] = ObjectType(
    "Attribute",
    fields[Repo, Attribute](
      Field("name", StringType, resolve = _.value.name),
      Field("value", StringType, resolve = _.value.value)))

  val ImageType: ObjectType[Repo, Image] = ObjectType(
    "Image",
    fields[Repo, Image](
      Field("url", StringType, resolve = _.value.url),
      Field("label", OptionType(StringType), resolve = _.value.label)))

  val AvailabilityType: ObjectType[Repo, Availability] = ObjectType(
    "Availability",
    fields[Repo, Availability](
      Field("isOnStock", BooleanType, resolve = _.value.isOnStock),
      Field("availableQuantity", IntType, resolve = _.value.availableQuantity))
  )

  val VariantType: ObjectType[Repo, Variant] = ObjectType(
    "ProductVariant",
    fields[Repo, Variant](
      Field("id", IntType, resolve = _.value.id),
      Field("sku", StringType, resolve = _.value.sku),
      Field("price", PriceType, complexity = Some((_, _, c) => 1 + c), resolve = _.value.price),
      Field("attributes", ListType(AttributeType), resolve = _.value.attributes),
      Field("images", ListType(ImageType), resolve = _.value.images),
      Field("availability", AvailabilityType, resolve = _.value.availability)
    )
  )

  lazy val CategoryType: ObjectType[Repo, Category] = ObjectType(
    "Category",
    () =>
      fields[Repo, Category](
        Field("id", IDType, resolve = _.value.id),
        Field("key", StringType, resolve = _.value.key),
        localized[Repo, Category]("name", _.names),
        Field(
          "parent",
          OptionType(CategoryType),
          complexity = Some((_, _, c) => 5 + c),
          resolve = c =>
            c.value.parentId.fold(Value(None): LeafAction[Repo, Option[Category]])(id =>
              DeferredValue(CategoryRef(id)))
        )
      )
  )

  val ResourceType: InterfaceType[Repo, Any] = InterfaceType(
    "Resource",
    fields[Repo, Any](
      Field("id", IDType, resolve = _ => ""),
      Field("version", LongType, resolve = _ => 0L)))

  val ProductType: ObjectType[Repo, Product] = ObjectType(
    "Product",
    interfaces[Repo, Product](ResourceType),
    fields[Repo, Product](
      Field("id", IDType, resolve = _.value.id),
      Field("version", LongType, resolve = _.value.version),
      Field("key", StringType, resolve = _.value.key),
      localized[Repo, Product]("name", _.names),
      localized[Repo, Product]("slug", _.slugs),
      localized[Repo, Product]("description", _.descriptions),
      Field(
        "categories",
        ListType(CategoryType),
        complexity = Some((_, _, c) => 5 + c),
        resolve = c => CategoryRefs(c.value.categoryIds)
      ),
      Field("masterVariant", VariantType, resolve = _.value.masterVariant),
      Field("variants", ListType(VariantType), resolve = _.value.variants)
    )
  )

  def queryResultType[A](
      name: String,
      elem: ObjectType[Repo, A]): ObjectType[Repo, QueryResult[A]] =
    ObjectType(
      name,
      fields[Repo, QueryResult[A]](
        Field("offset", IntType, resolve = _.value.offset),
        Field("count", IntType, resolve = _.value.count),
        Field("total", LongType, resolve = _.value.total),
        Field("results", ListType(elem), resolve = _.value.results)
      )
    )

  val LineItemType: ObjectType[Repo, LineItem] = ObjectType(
    "LineItem",
    fields[Repo, LineItem](
      Field("id", IDType, resolve = _.value.id),
      Field("quantity", IntType, resolve = _.value.quantity),
      Field("price", MoneyType, resolve = _.value.price))
  )

  val CartType: ObjectType[Repo, Cart] = ObjectType(
    "Cart",
    interfaces[Repo, Cart](ResourceType),
    fields[Repo, Cart](
      Field("id", IDType, resolve = _.value.id),
      Field("version", LongType, resolve = _.value.version),
      Field("lineItems", ListType(LineItemType), resolve = _.value.lineItems),
      Field("totalPrice", MoneyType, resolve = _.value.totalPrice)
    )
  )

  val AddLineItemType: InputObjectType[InputObjectType.DefaultInput] = InputObjectType(
    "AddLineItem",
    List(
      InputField("productId", IDType),
      InputField("variantId", OptionInputType(IntType)),
      InputField("quantity", OptionInputType(IntType), defaultValue = 1))
  )

  val UpdateActionType: InputObjectType[InputObjectType.DefaultInput] = InputObjectType(
    "CartUpdateAction",
    List(
      InputField("addLineItem", OptionInputType(AddLineItemType)),
      InputField("setCustomerEmail", OptionInputType(StringType))))

  val LimitArg: Argument[Int] = Argument("limit", OptionInputType(IntType), 20)
  val OffsetArg: Argument[Int] = Argument("offset", OptionInputType(IntType), 0)

  val QueryType: ObjectType[Repo, Unit] = ObjectType(
    "Query",
    fields[Repo, Unit](
      Field(
        "products",
        queryResultType("ProductQueryResult", ProductType),
        arguments = List(
          LimitArg,
          OffsetArg,
          Argument("where", OptionInputType(StringType)),
          Argument("sort", OptionInputType(ListInputType(StringType)))),
        tags = List(RequiresScope("view_products")),
        complexity = Some((_, args, c) => 2 + args.arg(LimitArg) * c),
        resolve = c => {
          val limit = c.arg(LimitArg)
          val offset = c.arg(OffsetArg)
          val all = c.ctx.products
          val page = all.slice(offset, offset + limit).toList
          Future.successful(QueryResult(offset, page.size, all.size.toLong, page))
        }
      ),
      Field(
        "product",
        OptionType(ProductType),
        arguments = Argument("id", IDType) :: Nil,
        resolve = c => c.ctx.products.find(_.id == c.arg[String]("id")))
    )
  )

  val MutationType: ObjectType[Repo, Unit] = ObjectType(
    "Mutation",
    fields[Repo, Unit](
      Field(
        "updateCart",
        CartType,
        arguments = List(
          Argument("id", IDType),
          Argument("version", LongType),
          Argument("actions", ListInputType(UpdateActionType))),
        tags = List(RequiresScope("manage_orders")),
        resolve = c => {
          val items = c
            .arg[Seq[Map[String, Any]]]("actions")
            .toList
            .flatMap(_.get("addLineItem").collect { case Some(m: Map[_, _]) =>
              val li = m.asInstanceOf[Map[String, Any]]
              val qty = li.get("quantity").collect { case Some(q: Int) => q }.getOrElse(1)
              LineItem(
                s"li-${li("productId")}",
                li("productId").toString,
                qty,
                Money(qty * 1000L, "EUR"))
            })
          Future.successful(
            Cart(
              c.arg[String]("id"),
              c.arg[Long]("version") + 1,
              items,
              Money(items.map(_.price.centAmount).sum, "EUR")))
        }
      )
    )
  )

  val schema: Schema[Repo, Unit] = Schema(QueryType, Some(MutationType))

  def makeRepo(size: Int): Repo = {
    val categories = (0 until 10).map { i =>
      val id = s"cat$i"
      id -> Category(
        id,
        s"category-$i",
        Map("en" -> s"Category $i", "de" -> s"Kategorie $i"),
        if (i == 0) None else Some("cat0"))
    }.toMap
    def variant(p: Int, v: Int) = Variant(
      v,
      s"sku-$p-$v",
      Price(Money(1000L + v, "EUR"), if (v % 2 == 0) Some(Money(900L + v, "EUR")) else None),
      List(Attribute("color", s"color$v"), Attribute("size", s"size$v")),
      List(Image(s"https://images.example.com/$p/$v.jpg", Some(s"image $v"))),
      Availability(v % 3 != 0, v * 7)
    )
    val products = Vector.tabulate(size) { i =>
      Product(
        s"p$i",
        1L,
        s"product-$i",
        Map("en" -> s"Product $i", "de" -> s"Produkt $i"),
        Map("en" -> s"product-$i", "de" -> s"produkt-$i"),
        Map("en" -> s"A fine description for product $i", "de" -> s"Beschreibung $i"),
        List(s"cat${i % 10}"),
        variant(i, 1),
        List(variant(i, 2), variant(i, 3), variant(i, 4))
      )
    }
    new Repo(products, categories, Set("view_products", "manage_orders"))
  }

  val listingQuery: String =
    """
    query ProductListing($limit: Int!, $offset: Int!, $locale: String!, $withVariants: Boolean!) {
      products(limit: $limit, offset: $offset, sort: ["createdAt desc"]) {
        offset
        count
        total
        results {
          ...ProductFields
        }
      }
    }

    fragment ProductFields on Product {
      id
      version
      key
      name(locale: $locale)
      slug(locale: $locale)
      description(locale: $locale)
      categories {
        id
        key
        name(locale: $locale)
        parent { id name(locale: $locale) }
      }
      masterVariant { ...VariantFields }
      variants @include(if: $withVariants) { ...VariantFields }
    }

    fragment VariantFields on ProductVariant {
      id
      sku
      price {
        value { centAmount currencyCode fractionDigits }
        discounted { centAmount currencyCode }
      }
      attributes { name value }
      images { url label }
      availability { isOnStock availableQuantity }
    }
    """

  val mutationQuery: String =
    """
    mutation UpdateCart($id: ID!, $version: Long!, $actions: [CartUpdateAction!]!) {
      updateCart(id: $id, version: $version, actions: $actions) {
        id
        version
        totalPrice { centAmount currencyCode }
        lineItems { id quantity price { centAmount currencyCode } }
      }
    }
    """
}

@State(Scope.Thread)
class RealisticRequestBenchmark {
  import RealisticRequestBenchmark._
  import ExecutionContext.Implicits.global

  @Param(Array("20", "100"))
  var pageSize: Int = _

  var repo: Repo = _
  var reducers: List[QueryReducer[Repo, _]] = _
  var middleware: List[Middleware[Repo]] = _

  @Setup
  def setup(): Unit = {
    repo = makeRepo(pageSize)
    middleware = List(new FieldMetricsMiddleware, new ScopeMiddleware, new ErrorReportingMiddleware)
    reducers = List(
      QueryReducer.rejectMaxDepth[Repo](15),
      QueryReducer
        .rejectComplexQueries[Repo](1000000, (_, _) => new IllegalStateException("too complex")))
  }

  private def run(query: String, variables: Map[String, Any]): Any = {
    val result = Await.result(
      Executor.execute(
        schema,
        QueryParser.parse(query).get,
        repo,
        variables = ScalaInput.scalaInput(variables),
        deferredResolver = new CategoryResolver,
        exceptionHandler = ExceptionHandler.empty,
        middleware = middleware,
        queryReducers = reducers
      ),
      30.seconds
    )
    require(!result.asInstanceOf[Map[String, Any]].contains("errors"), result.toString)
    result
  }

  @Benchmark
  def productListing(bh: Blackhole): Unit =
    bh.consume(
      run(
        listingQuery,
        Map("limit" -> pageSize, "offset" -> 0, "locale" -> "en", "withVariants" -> true)))

  @Benchmark
  def productListingWithoutVariants(bh: Blackhole): Unit =
    bh.consume(
      run(
        listingQuery,
        Map("limit" -> pageSize, "offset" -> 0, "locale" -> "de", "withVariants" -> false)))

  @Benchmark
  def cartMutation(bh: Blackhole): Unit =
    bh.consume(
      run(
        mutationQuery,
        Map[String, Any](
          "id" -> "cart1",
          "version" -> 3L,
          "actions" -> List[Map[String, Any]](
            Map("addLineItem" -> Map[String, Any]("productId" -> "p1", "quantity" -> 2)),
            Map("addLineItem" -> Map[String, Any]("productId" -> "p2")),
            Map("setCustomerEmail" -> "a@example.com")
          )
        )
      ))

  @Benchmark
  def introspection(bh: Blackhole): Unit =
    bh.consume(
      Await.result(
        Executor.execute(schema, introspectionQuery, repo, middleware = middleware),
        30.seconds))
}
