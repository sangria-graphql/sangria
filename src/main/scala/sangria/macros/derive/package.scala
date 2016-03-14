package sangria.macros

import scala.language.experimental.{macros ⇒ `scalac, please just let me do it!`}

import sangria.execution.FieldTag
import sangria.schema.{EnumType, ObjectType}

import scala.annotation.StaticAnnotation

package object derive {
  def deriveObjectType[Ctx, Val](config: DeriveObjectTypeConfig[Ctx, Val]*): ObjectType[Ctx, Val] = macro DeriveMacro.deriveObjectType[Ctx, Val]

  def deriveEnum[T](config: DeriveEnumTypeConfig*): EnumType[T] = macro DeriveMacro.deriveEnumType[T]

  class GraphQLName(name: String) extends StaticAnnotation
  class GraphQLDescription(description: String) extends StaticAnnotation
  class GraphQLDeprecated(deprecationReason: String) extends StaticAnnotation
  class GraphQLFieldTags(fieldTags: FieldTag*) extends StaticAnnotation
  class GraphQLExclude extends StaticAnnotation
}
