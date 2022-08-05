package sangria.macros.derive

import language.existentials
import sangria.execution.FieldTag
import sangria.schema.{InputType, OutputType}

import scala.annotation.StaticAnnotation

class GraphQLName(name: String) extends StaticAnnotation
class GraphQLDescription(description: String) extends StaticAnnotation
class GraphQLDeprecated(deprecationReason: String) extends StaticAnnotation
class GraphQLFieldTags(fieldTags: FieldTag*) extends StaticAnnotation
class GraphQLExclude extends StaticAnnotation
class GraphQLField extends StaticAnnotation
class GraphQLDefault[T](defaultValue: T) extends StaticAnnotation
class GraphQLOutputType(graphQLType: OutputType[_]) extends StaticAnnotation
class GraphQLInputType(graphQLType: InputType[_]) extends StaticAnnotation
