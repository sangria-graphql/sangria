package sangria.macros.derive

import language.existentials

import sangria.execution.FieldTag

import scala.annotation.StaticAnnotation

class GraphQLName(name: String) extends StaticAnnotation
class GraphQLDescription(description: String) extends StaticAnnotation
class GraphQLDeprecated(deprecationReason: String) extends StaticAnnotation
class GraphQLFieldTags(fieldTags: FieldTag*) extends StaticAnnotation
class GraphQLExclude extends StaticAnnotation
class GraphQLField extends StaticAnnotation
class GraphQLDefault(defaultValue: T forSome {type T}) extends StaticAnnotation