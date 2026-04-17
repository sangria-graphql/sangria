package sangria.schema

import java.util.Date

import sangria.ast._
import sangria.parser.QueryParser
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CoercionSpec extends AnyWordSpec with Matchers {
  "Type System: Scalar coercion" when {
    "coerces output" should {
      "Int" in {
        IntType.coerceOutput(1, Set.empty) should be(1)
        IntType.coerceOutput(Integer.MAX_VALUE, Set.empty) should be(Integer.MAX_VALUE)
        IntType.coerceOutput(Integer.MIN_VALUE, Set.empty) should be(Integer.MIN_VALUE)
      }

      "Long" in {
        LongType.coerceOutput(1111111111111L, Set.empty) should be(1111111111111L)
        LongType.coerceOutput(Long.MaxValue, Set.empty) should be(Long.MaxValue)
        LongType.coerceOutput(Long.MinValue, Set.empty) should be(Long.MinValue)
      }

      "BigInt" in {
        BigIntType.coerceOutput(BigInt("89493264763287"), Set.empty) should be(
          BigInt("89493264763287"))
      }

      "Float" in {
        FloatType.coerceOutput(123.456, Set.empty) should be(123.456)
      }

      "Float preserves 0.1 (not losslessly representable as double)" in {
        FloatType.coerceOutput(0.1, Set.empty) should be(0.1)
      }

      "Float preserves 144.75999999999999 (not losslessly representable as double)" in {
        val v = 144.75999999999999
        FloatType.coerceOutput(v, Set.empty) should be(v)
      }

      "Float coerces NaN to null" in {
        FloatType.coerceOutput(Double.NaN, Set.empty) should be(null.asInstanceOf[java.lang.Double])
      }

      "Float coerces infinity to null" in {
        FloatType.coerceOutput(Double.PositiveInfinity, Set.empty) should be(
          null.asInstanceOf[java.lang.Double])
        FloatType.coerceOutput(Double.NegativeInfinity, Set.empty) should be(
          null.asInstanceOf[java.lang.Double])
      }

      "BigDecimal" in {
        BigDecimalType.coerceOutput(
          BigDecimal("467356453726547623.37467823648763238479823"),
          Set.empty) should be(BigDecimal("467356453726547623.37467823648763238479823"))
      }

      "Boolean" in {
        BooleanType.coerceOutput(true, Set.empty) should be(true: Any)
        BooleanType.coerceOutput(false, Set.empty) should be(false: Any)
      }

      "String" in {
        StringType.coerceOutput("foo", Set.empty) should be("foo")
        StringType.coerceOutput("", Set.empty) should be("")
      }
    }

    "coerces input" should {
      "Int" in {
        IntType.coerceInput(IntValue(123)) should be(Right(123))

        IntType.coerceInput(BigIntValue(BigInt("123234"))) should be(Right(123234))
        IntType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))).isLeft should be(
          true)

        IntType.coerceInput(FloatValue(12.34)).isLeft should be(true)
        IntType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be(true)
        IntType.coerceInput(BooleanValue(true)).isLeft should be(true)
        IntType.coerceInput(StringValue("123")).isLeft should be(true)
      }

      "Long" in {
        LongType.coerceInput(IntValue(123)) should be(Right(123L))

        LongType.coerceInput(BigIntValue(BigInt("123234"))) should be(Right(123234L))
        LongType.coerceInput(BigIntValue(BigInt("1232342131243432"))) should be(
          Right(1232342131243432L))
        LongType.coerceInput(StringValue(Long.MaxValue.toString)) should be(Right(Long.MaxValue))
        LongType
          .coerceInput(BigIntValue(BigInt("123234438749837964783648763284768372648723684763287")))
          .isLeft should be(true)

        LongType.coerceInput(FloatValue(12.34)).isLeft should be(true)
        LongType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be(true)
        LongType.coerceInput(BooleanValue(true)).isLeft should be(true)
        LongType
          .coerceInput(StringValue("123234438749837964783648763284768372648723684763287"))
          .isLeft should be(true)
      }

      "BigInt" in {
        BigIntType.coerceInput(IntValue(123)) should be(Right(BigInt("123")))

        BigIntType.coerceInput(BigIntValue(BigInt("123234"))) should be(Right(BigInt("123234")))
        BigIntType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))) should be(
          Right(BigInt("12323443874982374987329749823")))

        BigIntType.coerceInput(FloatValue(12.34)).isLeft should be(true)
        BigIntType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be(true)
        BigIntType.coerceInput(BooleanValue(true)).isLeft should be(true)
        BigIntType.coerceInput(StringValue("123")).isLeft should be(true)
      }

      "Float" in {
        FloatType.coerceInput(IntValue(123)) should be(Right(123.0d))

        FloatType.coerceInput(BigIntValue(BigInt("123234"))) should be(Right(123234.0d))
        FloatType
          .coerceInput(BigIntValue(BigInt("12323443874982374987329749823")))
          .isLeft should be(true)

        FloatType.coerceInput(FloatValue(12.34)) should be(Right(12.34d))
        FloatType.coerceInput(FloatValue(0.1)) should be(Right(0.1))
        FloatType.coerceInput(BigDecimalValue(BigDecimal("0.1"))) should be(Right(0.1))
        FloatType.coerceInput(FloatValue(144.75999999999999)) should be(Right(144.75999999999999))
        FloatType.coerceInput(BigDecimalValue(BigDecimal("144.75999999999999"))) match {
          case Right(d) => d should be(144.75999999999999)
          case Left(v) =>
            fail(s"""BigDecimal("144.75999999999999") coercion MUST succeed, got Left($v)""")
        }
        FloatType.coerceInput(BigDecimalValue(BigDecimal("144.76"))) match {
          case Right(d) => d should be(144.75999999999999)
          case Left(v) => fail(s"""BigDecimal("144.76") coercion MUST succeed, got Left($v)""")
        }
        // Spec §3.5.2: non-finite (NaN, Infinity) must raise request error
        FloatType.coerceInput(FloatValue(Double.NaN)).isLeft should be(true)
        FloatType.coerceInput(FloatValue(Double.PositiveInfinity)).isLeft should be(true)
        FloatType.coerceInput(FloatValue(Double.NegativeInfinity)).isLeft should be(true)
        FloatType.coerceInput(BigDecimalValue(BigDecimal(12.34))) should be(Right(12.34d))
        FloatType
          .coerceInput(
            BigDecimalValue(BigDecimal("367476315476516457632.473854635267452376546732")))
          .isRight should be(true)
        FloatType.coerceInput(BooleanValue(true)).isLeft should be(true)
        FloatType.coerceInput(StringValue("123")).isLeft should be(true)
      }

      "BigDecimal" in {
        BigDecimalType.coerceInput(IntValue(123)) should be(Right(BigDecimal(123.0d)))

        BigDecimalType.coerceInput(BigIntValue(BigInt("123234"))) should be(
          Right(BigDecimal(123234.0d)))
        BigDecimalType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))) should be(
          Right(BigDecimal("12323443874982374987329749823")))

        BigDecimalType.coerceInput(FloatValue(12.34)) should be(Right(BigDecimal(12.34d)))
        BigDecimalType.coerceInput(BigDecimalValue(BigDecimal(12.34))) should be(
          Right(BigDecimal(12.34d)))
        BigDecimalType.coerceInput(
          BigDecimalValue(BigDecimal("367476315476516457632.473854635267452376546732"))) should be(
          Right(BigDecimal("367476315476516457632.473854635267452376546732")))

        BigDecimalType.coerceInput(BooleanValue(true)).isLeft should be(true)
        BigDecimalType.coerceInput(StringValue("123")).isLeft should be(true)
      }

      "Boolean" in {
        BooleanType.coerceInput(IntValue(123)).isLeft should be(true)

        BooleanType.coerceInput(BigIntValue(BigInt("123234"))).isLeft should be(true)
        BooleanType
          .coerceInput(BigIntValue(BigInt("12323443874982374987329749823")))
          .isLeft should be(true)

        BooleanType.coerceInput(FloatValue(12.34)).isLeft should be(true)
        BooleanType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be(true)
        BooleanType.coerceInput(BooleanValue(true)) should be(Right(true))
        BooleanType.coerceInput(StringValue("123")).isLeft should be(true)
      }

      "String" in {
        StringType.coerceInput(IntValue(123)).isLeft should be(true)

        StringType.coerceInput(BigIntValue(BigInt("123234"))).isLeft should be(true)
        StringType
          .coerceInput(BigIntValue(BigInt("12323443874982374987329749823")))
          .isLeft should be(true)

        StringType.coerceInput(FloatValue(12.34)).isLeft should be(true)
        StringType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be(true)
        StringType.coerceInput(BooleanValue(true)).isLeft should be(true)
        StringType.coerceInput(StringValue("123")) should be(Right("123"))
        StringType.coerceInput(StringValue("")) should be(Right(""))
      }
    }

    "coerces user input" should {
      "Int" in {
        IntType.coerceUserInput(123) should be(Right(123))

        IntType.coerceUserInput(123L) should be(Right(123))
        IntType.coerceUserInput(12334324234L).isLeft should be(true)

        IntType.coerceUserInput(BigInt("123234")) should be(Right(123234))
        IntType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be(true)

        IntType.coerceUserInput(123.0d) should be(Right(123))
        IntType.coerceUserInput(123.001d).isLeft should be(true)
        IntType.coerceUserInput(1212321321321321321321312.0d).isLeft should be(true)

        IntType.coerceUserInput(BigDecimal("123.0")) should be(Right(123))
        IntType.coerceUserInput(BigDecimal("123.000001")).isLeft should be(true)
        IntType.coerceUserInput(BigDecimal("123243432432432423423442342.0")).isLeft should be(true)

        IntType.coerceUserInput(12.34d).isLeft should be(true)
        IntType.coerceUserInput(BigDecimal(12.34)).isLeft should be(true)
        IntType.coerceUserInput(true).isLeft should be(true)
        IntType.coerceUserInput("123").isLeft should be(true)
        IntType.coerceUserInput(new Date).isLeft should be(true)
      }

      "Long" in {
        LongType.coerceUserInput(123) should be(Right(123L))

        LongType.coerceUserInput(123L) should be(Right(123L))
        LongType.coerceUserInput(12334324234L) should be(Right(12334324234L))

        LongType.coerceUserInput(BigInt("123234")) should be(Right(123234L))
        LongType.coerceUserInput(BigInt("12323475436574")) should be(Right(12323475436574L))
        LongType
          .coerceUserInput(BigInt("1232344387498237498732974982334324234325435"))
          .isLeft should be(true)

        LongType.coerceUserInput(123123123123123.0d) should be(Right(123123123123123L))
        LongType.coerceUserInput(123.001d).isLeft should be(true)

        LongType.coerceUserInput(BigDecimal("123.0")) should be(Right(123L))
        LongType.coerceUserInput(BigDecimal("123.000001")).isLeft should be(true)
        LongType.coerceUserInput(BigDecimal("12321344346783264876328764872368.0")).isLeft should be(
          true)

        LongType.coerceUserInput(12.34d).isLeft should be(true)
        LongType.coerceUserInput(BigDecimal(12.34)).isLeft should be(true)
        LongType.coerceUserInput(true).isLeft should be(true)
        LongType.coerceUserInput("123") should be(Right(123L))
        LongType.coerceUserInput("").isLeft should be(true)
        LongType
          .coerceUserInput("1232344387498237498732974982334324234325435")
          .isLeft should be(true)
        LongType.coerceUserInput("123.0").isLeft should be(true)
        LongType.coerceUserInput(new Date).isLeft should be(true)
      }

      "BigInt" in {
        BigIntType.coerceUserInput(123) should be(Right(BigInt("123")))

        BigIntType.coerceUserInput(123L) should be(Right(BigInt("123")))
        BigIntType.coerceUserInput(12334324234L) should be(Right(BigInt("12334324234")))

        BigIntType.coerceUserInput(123123123123123.0d) should be(Right(BigInt("123123123123123")))
        BigIntType.coerceUserInput(123.001d).isLeft should be(true)

        BigIntType.coerceUserInput(BigDecimal("123.0")) should be(Right(BigInt(123)))
        BigIntType.coerceUserInput(BigDecimal("123.000001")).isLeft should be(true)

        BigIntType.coerceUserInput(BigInt("123234")) should be(Right(BigInt("123234")))
        BigIntType.coerceUserInput(BigInt("12323443874982374987329749823")) should be(
          Right(BigInt("12323443874982374987329749823")))

        BigIntType.coerceUserInput(12.34).isLeft should be(true)
        BigIntType.coerceUserInput(BigDecimal(12.34)).isLeft should be(true)
        BigIntType.coerceUserInput(true).isLeft should be(true)
        BigIntType.coerceUserInput("123").isLeft should be(true)
        BigIntType.coerceUserInput(new Date).isLeft should be(true)
      }

      "Float" in {
        FloatType.coerceUserInput(123) should be(Right(123.0d))

        FloatType.coerceUserInput(123L) should be(Right(123.0d))
        FloatType.coerceUserInput(12334324234L) should be(Right(12334324234.0d))

        FloatType.coerceUserInput(BigInt("123234")) should be(Right(123234.0d))
        FloatType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be(true)

        FloatType.coerceUserInput(12.34) should be(Right(12.34d))
        FloatType.coerceUserInput(0.1) should be(Right(0.1))
        FloatType.coerceUserInput(BigDecimal("0.1")) should be(Right(0.1))
        FloatType.coerceUserInput(144.75999999999999) should be(Right(144.75999999999999))
        FloatType.coerceUserInput(BigDecimal("144.75999999999999")) match {
          case Right(d) => d should be(144.75999999999999)
          case Left(v) =>
            fail(s"""BigDecimal("144.75999999999999") coercion MUST succeed, got Left($v)""")
        }
        FloatType.coerceUserInput(BigDecimal("144.76")) match {
          case Right(d) => d should be(144.75999999999999)
          case Left(v) => fail(s"""BigDecimal("144.76") coercion MUST succeed, got Left($v)""")
        }
        // Spec §3.5.2: non-finite (NaN, Infinity) must raise request error
        FloatType.coerceUserInput(Double.NaN).isLeft should be(true)
        FloatType.coerceUserInput(Double.PositiveInfinity).isLeft should be(true)
        FloatType.coerceUserInput(Double.NegativeInfinity).isLeft should be(true)
        FloatType.coerceUserInput(BigDecimal(12.34)) should be(Right(12.34d))
        FloatType
          .coerceUserInput(BigDecimal("367476315476516457632.473854635267452376546732"))
          .isRight should be(true)
        FloatType.coerceUserInput(true).isLeft should be(true)
        FloatType.coerceUserInput("123").isLeft should be(true)
        FloatType.coerceUserInput(new Date).isLeft should be(true)
      }

      "BigDecimal" in {
        BigDecimalType.coerceUserInput(123) should be(Right(BigDecimal(123.0d)))

        BigDecimalType.coerceUserInput(123L) should be(Right(BigDecimal(123.0d)))
        BigDecimalType.coerceUserInput(12334324234L) should be(Right(BigDecimal(12334324234.0d)))

        BigDecimalType.coerceUserInput(BigInt("123234")) should be(Right(BigDecimal(123234.0d)))
        BigDecimalType.coerceUserInput(BigInt("12323443874982374987329749823")) should be(
          Right(BigDecimal("12323443874982374987329749823")))

        BigDecimalType.coerceUserInput(12.34) should be(Right(BigDecimal(12.34d)))
        BigDecimalType.coerceUserInput(BigDecimal(12.34)) should be(Right(BigDecimal(12.34d)))
        BigDecimalType.coerceUserInput(
          BigDecimal("367476315476516457632.473854635267452376546732")) should be(
          Right(BigDecimal("367476315476516457632.473854635267452376546732")))

        BigDecimalType.coerceUserInput(true).isLeft should be(true)
        BigDecimalType.coerceUserInput("123").isLeft should be(true)
        BigDecimalType.coerceUserInput(new Date).isLeft should be(true)
      }

      "Boolean" in {
        BooleanType.coerceUserInput(123).isLeft should be(true)

        BooleanType.coerceUserInput(1232342323L).isLeft should be(true)

        BooleanType.coerceUserInput(BigInt("123234")).isLeft should be(true)
        BooleanType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be(true)

        BooleanType.coerceUserInput(12.34).isLeft should be(true)
        BooleanType.coerceUserInput(BigDecimal(12.34)).isLeft should be(true)
        BooleanType.coerceUserInput(true) should be(Right(true))
        BooleanType.coerceUserInput(false) should be(Right(false))
        BooleanType.coerceUserInput("123").isLeft should be(true)
      }

      "String" in {
        StringType.coerceUserInput(123).isLeft should be(true)

        BooleanType.coerceUserInput(1232342323L).isLeft should be(true)

        StringType.coerceUserInput(BigInt("123234")).isLeft should be(true)
        StringType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be(true)

        StringType.coerceUserInput(12.34).isLeft should be(true)
        StringType.coerceUserInput(BigDecimal(12.34)).isLeft should be(true)
        StringType.coerceUserInput(true).isLeft should be(true)
        StringType.coerceUserInput("123") should be(Right("123"))
        StringType.coerceUserInput("") should be(Right(""))
      }
    }
  }

  "Float decimal identity and BigDecimal precision" when {
    "144.75999999999999 (Double) equals 144.76 (Double) in Scala" in {
      (144.75999999999999: Double) should be(144.76)
    }

    "GraphQL Float: parsing \"144.75999999999999\" and \"144.76\" yields the same Double" in {
      val ast1 = QueryParser.parseInput("144.75999999999999").get
      val ast2 = QueryParser.parseInput("144.76").get
      val r1 = FloatType.coerceInput(ast1)
      val r2 = FloatType.coerceInput(ast2)
      r1.isRight should be(true)
      r2.isRight should be(true)
      r1 should equal(r2)
      (r1, r2) should be((Right(144.75999999999999), Right(144.75999999999999)))
    }

    "BigDecimal preserves exact decimals (144.75999999999999 != 144.76)" in {
      BigDecimal("144.75999999999999") should not be BigDecimal("144.76")
      BigDecimal("144.75999999999999").toString should be("144.75999999999999")
      BigDecimal("144.76").toString should be("144.76")
    }

    "BigDecimal preserves exact decimal 0.1" in {
      BigDecimal("0.1").toString should be("0.1")
    }

    "GraphQL Float \"0.1\" parses to Double (nearest IEEE 754, not exact decimal 0.1)" in {
      val ast = QueryParser.parseInput("0.1").get
      val result = FloatType.coerceInput(ast)
      result should be(Right(0.1))
      val d = result.toOption.get
      d should be(0.1)
      // 0.1 has no exact binary representation; Double holds the nearest IEEE 754 value
      // (exact decimal 0.1 would require BigDecimal; d is slightly != mathematical 0.1)
      java.lang.Double.doubleToLongBits(d) should be(java.lang.Double.doubleToLongBits(0.1))
    }
  }
}
