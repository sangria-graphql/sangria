package sangria.macros.derive

import sangria.schema._

import scala.concurrent.Future

object Issue906Context {

  @GraphQLName("SampleCaseClass")
  @GraphQLDescription("A sample case class")
  final case class SampleCaseClass(
      name: String
  ) {
    @GraphQLField
    @GraphQLName("myString")
    def graphqlMyString: String = "Hello World"

    @GraphQLField
    @GraphQLName("myFutureString")
    def graphqlMyFutureString: Future[String] = Future.successful("Hello World")

  }

  object SampleCaseClass {
    val sample: SampleCaseClass = SampleCaseClass("My test")
  }

  trait MyRepository {
    def getSampleCaseClass: SampleCaseClass
  }

  object MyRepository {
    def sample: MyRepository = new MyRepository {
      override def getSampleCaseClass: SampleCaseClass = SampleCaseClass.sample
    }
  }

  object MySample {
    val schema: Schema[MyRepository, Unit] = {

      implicit val graphqlSampleCaseClass: ObjectType[MyRepository, SampleCaseClass] =
        deriveObjectType[MyRepository, SampleCaseClass]()

      val queries: ObjectType[MyRepository, Unit] = ObjectType(
        "Query",
        fields(
          Field(
            "mySample",
            graphqlSampleCaseClass,
            Some("test"),
            arguments = Nil,
            resolve = (ctx: Context[MyRepository, Unit]) => SampleCaseClass.sample
          )
        )
      )

      Schema(
        query = queries,
        mutation = None,
        additionalTypes = Nil
      )
    }
  }
}
