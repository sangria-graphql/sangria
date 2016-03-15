package sangria.macros

import scala.language.experimental.{macros ⇒ `scalac, please just let me do it!`}

import sangria.execution.FieldTag
import sangria.schema.{EnumType, ObjectType}

import scala.annotation.StaticAnnotation

package object derive {
  def deriveObjectType[Ctx, Val](config: DeriveObjectSetting[Ctx, Val]*): ObjectType[Ctx, Val] = macro DeriveObjectTypeMacro.deriveObjectType[Ctx, Val]

  def deriveEnumType[T](config: DeriveEnumSetting*): EnumType[T] = macro DeriveEnumTypeMacro.deriveEnumType[T]

  class GraphQLName(name: String) extends StaticAnnotation
  class GraphQLDescription(description: String) extends StaticAnnotation
  class GraphQLDeprecated(deprecationReason: String) extends StaticAnnotation
  class GraphQLFieldTags(fieldTags: FieldTag*) extends StaticAnnotation
  class GraphQLExclude extends StaticAnnotation
  class GraphQLField extends StaticAnnotation
}
