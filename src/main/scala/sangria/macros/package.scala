package sangria

import scala.language.experimental.{macros ⇒ `scalac, please just let me do it!`}
import sangria.ast.{Document, InputDocument, Value}

package object macros {
  implicit class LiteralGraphQLStringContext(val sc: StringContext) extends AnyVal {
    def gql(args: Any*): Document = macro ParseMacro.impl
    def gqlInp(args: Any*): Value = macro ParseMacro.implInput
    def gqlInpDoc(args: Any*): InputDocument = macro ParseMacro.implInputDoc

    def graphql(args: Any*): Document = macro ParseMacro.impl
    def graphqlInput(args: Any*): Value = macro ParseMacro.implInput
    def graphqlInputDoc(args: Any*): InputDocument = macro ParseMacro.implInputDoc
  }

}

