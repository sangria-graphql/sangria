package sangria

import scala.language.experimental.{macros ⇒ `scalac, please just let me do it!`}

import sangria.execution.FieldTag
import sangria.macros._
import sangria.schema._

import scala.annotation.StaticAnnotation

import sangria.ast.{Document, Value}

package object macros {
  implicit class LiteralGraphQLStringContext(val sc: StringContext) extends AnyVal {
    def gql(args: Any*): Document = macro ParseMacro.impl
    def gqlInp(args: Any*): Value = macro ParseMacro.implInput

    def graphql(args: Any*): Document = macro ParseMacro.impl
    def graphqlInput(args: Any*): Value = macro ParseMacro.implInput
  }

}

