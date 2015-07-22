package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._
import sangria.util.{GraphQlSupport, AwaitSupport}

class VariablesSpec extends WordSpec with Matchers with AwaitSupport with GraphQlSupport {
  val TestInputObject = InputObjectType("TestInputObject", List(
    InputField("a", OptionInputType(StringType)),
    InputField("b", OptionInputType(ListInputType(OptionInputType(StringType)))),
    InputField("c", StringType)))

//  val TestType = ObjectType("TestType", List[Field[Unit, Unit]](
//    Field("fieldWithObjectInput"
//      arguments = Argument("input", OptionInputType(TestInputObject)) :: Nil)
//  ))

  def schema = ???
}
