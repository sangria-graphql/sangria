package sangria.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeliverySchemeSpec extends AnyWordSpec with Matchers {
  "DeliveryScheme" should {
    "by default support `Try`" in {
      import com.twitter.util.{Throw, Return}

      QueryParser.parse("{ field }") shouldBe a[Return[_]]
      QueryParser.parse("}") shouldBe a[Throw[_]]
    }

    "support `Either`" in {
      import sangria.parser.DeliveryScheme.Either
      import scala.util.{Left, Right}

      QueryParser.parse("{ field }") shouldBe a[Right[_, _]]
      QueryParser.parse("}") shouldBe a[Left[_, _]]
    }

    "support exception throwing" in {
      import sangria.parser.DeliveryScheme.Throw
      import sangria.ast.Document

      QueryParser.parse("{ field }") shouldBe a[Document]
      a[SyntaxError] should be thrownBy QueryParser.parse("}")
    }
  }
}
