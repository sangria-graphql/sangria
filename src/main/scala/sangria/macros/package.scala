package sangria

import sangria.execution.FieldTag
import sangria.macros._
import sangria.schema._

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros ⇒ `scalac, please just let me do it!`}

import sangria.ast.{Document, Value}

package object macros {
  implicit class LiteralGraphQLStringContext(val sc: StringContext) extends AnyVal {
    def graphql(): Document = macro ParseMacro.impl
    def graphqlInput(): Value = macro ParseMacro.implInput
  }


  def deriveObjectType[Ctx, Val](config: DeriveObjectTypeConfig[Ctx, Val]*): ObjectType[Ctx, Val] = macro DeriveMacro.deriveObjectType[Ctx, Val]

  def deriveEnum[T](config: DeriveEnumTypeConfig*): Int = macro DeriveMacro.deriveEnumType[T]

  class GraphQLName(name: String) extends StaticAnnotation
  class GraphQLDescription(description: String) extends StaticAnnotation
  class GraphQLDeprecated(deprecationReason: String) extends StaticAnnotation
  class GraphQLFieldTags(fieldTags: FieldTag*) extends StaticAnnotation
  class GraphQLExclude extends StaticAnnotation
}

