package sangria

import sangria.ast.{Document, InputDocument, Value}

package object macros {
  extension (inline sc: StringContext) {
    inline def gql(inline args: Any*): Document = ${ ParseMacro.impl('sc) }
    inline def gqlInp(inline args: Any*): Value = ${ ParseMacro.implInput('sc) }
    inline def gqlInpDoc(inline args: Any*): InputDocument = ${ ParseMacro.implInputDoc('sc) }

    inline def graphql(inline args: Any*): Document = ${ ParseMacro.impl('sc) }
    inline def graphqlInput(inline args: Any*): Value = ${ ParseMacro.implInput('sc) }
    inline def graphqlInputDoc(inline args: Any*): InputDocument = ${ ParseMacro.implInputDoc('sc) }
  }

}
