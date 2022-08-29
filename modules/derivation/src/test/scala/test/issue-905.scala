import io.circe.Decoder
import sangria.schema.{InputObjectType, ObjectType}

import scala.concurrent.Future

case class Ctx(ctx: String)

object PostSchema {
  import sangria.marshalling.circe._
  import io.circe.generic.semiauto._
  import sangria.macros.derive._

  case class UpdatePostGql() {
    @GraphQLField
    def text(text: TextInputGql): Future[String] = ???
  }

  object UpdatePostGql {
    implicit val _ot: ObjectType[Ctx, UpdatePostGql] =
      deriveObjectType[Ctx, UpdatePostGql](
        ObjectTypeName("UpdatePost")
      )
  }

  case class TextInputGql(
      @GraphQLInputType(sangria.schema.StringType)
      text: String)

  object TextInputGql {
    implicit val decoder: Decoder[TextInputGql] = deriveDecoder[TextInputGql]
    implicit val _it: InputObjectType[TextInputGql] =
      deriveInputObjectType[TextInputGql](
        InputObjectTypeName("TextInput")
      )
  }
}
