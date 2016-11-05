package sangria.schema

import java.util.Date

import org.scalatest.{Matchers, WordSpec}
import sangria.ast._

class CoercionSpec extends WordSpec with Matchers {
  "Type System: Scalar coercion" when {
    "coerces output" should {
      "Int" in {
        IntType.coerceOutput(1, Set.empty) should be (1)
        IntType.coerceOutput(Integer.MAX_VALUE, Set.empty) should be (Integer.MAX_VALUE)
        IntType.coerceOutput(Integer.MIN_VALUE, Set.empty) should be (Integer.MIN_VALUE)
      }

      "Long" in {
        LongType.coerceOutput(1111111111111L, Set.empty) should be (1111111111111L)
        LongType.coerceOutput(Long.MaxValue, Set.empty) should be (Long.MaxValue)
        LongType.coerceOutput(Long.MinValue, Set.empty) should be (Long.MinValue)
      }

      "BigInt" in {
        BigIntType.coerceOutput(BigInt("89493264763287"), Set.empty) should be (BigInt("89493264763287"))
      }

      "Float" in {
        FloatType.coerceOutput(123.456, Set.empty) should be (123.456)
      }

      "Float coerces NaN to null" in {
        FloatType.coerceOutput(Double.NaN, Set.empty) should be (null.asInstanceOf[java.lang.Double])
      }

      "Float coerces infinity to null" in {
        FloatType.coerceOutput(Double.PositiveInfinity, Set.empty) should be (null.asInstanceOf[java.lang.Double])
        FloatType.coerceOutput(Double.NegativeInfinity, Set.empty) should be (null.asInstanceOf[java.lang.Double])
      }

      "BigDecimal" in {
        BigDecimalType.coerceOutput(BigDecimal("467356453726547623.37467823648763238479823"), Set.empty) should be (
          BigDecimal("467356453726547623.37467823648763238479823"))
      }

      "Boolean" in {
        BooleanType.coerceOutput(true, Set.empty) should be (true: Any)
        BooleanType.coerceOutput(false, Set.empty) should be (false: Any)
      }

      "String" in {
        StringType.coerceOutput("foo", Set.empty) should be ("foo")
        StringType.coerceOutput("", Set.empty) should be ("")
      }
    }

    "coerces input" should {
      "Int" in {
        IntType.coerceInput(IntValue(123)) should be (Right(123))

        IntType.coerceInput(BigIntValue(BigInt("123234"))) should be (Right(123234))
        IntType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))).isLeft should be (true)

        IntType.coerceInput(FloatValue(12.34)).isLeft should be (true)
        IntType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be (true)
        IntType.coerceInput(BooleanValue(true)).isLeft should be (true)
        IntType.coerceInput(StringValue("123")).isLeft should be (true)
      }

      "Long" in {
        LongType.coerceInput(IntValue(123)) should be (Right(123L))

        LongType.coerceInput(BigIntValue(BigInt("123234"))) should be (Right(123234L))
        LongType.coerceInput(BigIntValue(BigInt("1232342131243432"))) should be (Right(1232342131243432L))
        LongType.coerceInput(BigIntValue(BigInt("123234438749837964783648763284768372648723684763287"))).isLeft should be (true)

        LongType.coerceInput(FloatValue(12.34)).isLeft should be (true)
        LongType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be (true)
        LongType.coerceInput(BooleanValue(true)).isLeft should be (true)
        LongType.coerceInput(StringValue("123")).isLeft should be (true)
      }

      "BigInt" in {
        BigIntType.coerceInput(IntValue(123)) should be (Right(BigInt("123")))

        BigIntType.coerceInput(BigIntValue(BigInt("123234"))) should be (Right(BigInt("123234")))
        BigIntType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))) should be (
          Right(BigInt("12323443874982374987329749823")))

        BigIntType.coerceInput(FloatValue(12.34)).isLeft should be (true)
        BigIntType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be (true)
        BigIntType.coerceInput(BooleanValue(true)).isLeft should be (true)
        BigIntType.coerceInput(StringValue("123")).isLeft should be (true)
      }

      "Float" in {
        FloatType.coerceInput(IntValue(123)) should be (Right(123.0D))

        FloatType.coerceInput(BigIntValue(BigInt("123234"))) should be (Right(123234.0D))
        FloatType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))).isLeft should be (true)

        FloatType.coerceInput(FloatValue(12.34)) should be (Right(12.34D))
        FloatType.coerceInput(FloatValue(Double.NaN)).right.exists(_.isNaN) should be (true)
        FloatType.coerceInput(FloatValue(Double.PositiveInfinity)).right.exists(_.isPosInfinity) should be (true)
        FloatType.coerceInput(FloatValue(Double.NegativeInfinity)).right.exists(_.isNegInfinity) should be (true)
        FloatType.coerceInput(BigDecimalValue(BigDecimal(12.34))) should be (Right(12.34D))
        FloatType.coerceInput(BigDecimalValue(BigDecimal("367476315476516457632.473854635267452376546732"))).isLeft should be (true)
        FloatType.coerceInput(BooleanValue(true)).isLeft should be (true)
        FloatType.coerceInput(StringValue("123")).isLeft should be (true)
      }

      "BigDecimal" in {
        BigDecimalType.coerceInput(IntValue(123)) should be (Right(BigDecimal(123.0D)))

        BigDecimalType.coerceInput(BigIntValue(BigInt("123234"))) should be (Right(BigDecimal(123234.0D)))
        BigDecimalType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))) should be (
          Right(BigDecimal("12323443874982374987329749823")))

        BigDecimalType.coerceInput(FloatValue(12.34)) should be (Right(BigDecimal(12.34D)))
        BigDecimalType.coerceInput(BigDecimalValue(BigDecimal(12.34))) should be (Right(BigDecimal(12.34D)))
        BigDecimalType.coerceInput(BigDecimalValue(BigDecimal("367476315476516457632.473854635267452376546732"))) should be (
          Right(BigDecimal("367476315476516457632.473854635267452376546732")))

        BigDecimalType.coerceInput(BooleanValue(true)).isLeft should be (true)
        BigDecimalType.coerceInput(StringValue("123")).isLeft should be (true)
      }

      "Boolean" in {
        BooleanType.coerceInput(IntValue(123)).isLeft should be (true)

        BooleanType.coerceInput(BigIntValue(BigInt("123234"))).isLeft should be (true)
        BooleanType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))).isLeft should be (true)

        BooleanType.coerceInput(FloatValue(12.34)).isLeft should be (true)
        BooleanType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be (true)
        BooleanType.coerceInput(BooleanValue(true)) should be (Right(true))
        BooleanType.coerceInput(StringValue("123")).isLeft should be (true)
      }

      "String" in {
        StringType.coerceInput(IntValue(123)).isLeft should be (true)

        StringType.coerceInput(BigIntValue(BigInt("123234"))).isLeft should be (true)
        StringType.coerceInput(BigIntValue(BigInt("12323443874982374987329749823"))).isLeft should be (true)

        StringType.coerceInput(FloatValue(12.34)).isLeft should be (true)
        StringType.coerceInput(BigDecimalValue(BigDecimal(12.34))).isLeft should be (true)
        StringType.coerceInput(BooleanValue(true)).isLeft should be (true)
        StringType.coerceInput(StringValue("123")) should be (Right("123"))
        StringType.coerceInput(StringValue("")) should be (Right(""))
      }
    }

    "coerces user input" should {
      "Int" in {
        IntType.coerceUserInput(123) should be (Right(123))

        IntType.coerceUserInput(123L) should be (Right(123))
        IntType.coerceUserInput(12334324234L).isLeft should be (true)

        IntType.coerceUserInput(BigInt("123234")) should be (Right(123234))
        IntType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        IntType.coerceUserInput(12.34D).isLeft should be (true)
        IntType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        IntType.coerceUserInput(true).isLeft should be (true)
        IntType.coerceUserInput("123").isLeft should be (true)
        IntType.coerceUserInput(new Date).isLeft should be (true)
      }

      "Long" in {
        LongType.coerceUserInput(123) should be (Right(123L))

        LongType.coerceUserInput(123L) should be (Right(123L))
        LongType.coerceUserInput(12334324234L) should be (Right(12334324234L))

        LongType.coerceUserInput(BigInt("123234")) should be (Right(123234L))
        LongType.coerceUserInput(BigInt("12323475436574")) should be (Right(12323475436574L))
        LongType.coerceUserInput(BigInt("1232344387498237498732974982334324234325435")).isLeft should be (true)

        LongType.coerceUserInput(12.34D).isLeft should be (true)
        LongType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        LongType.coerceUserInput(true).isLeft should be (true)
        LongType.coerceUserInput("123").isLeft should be (true)
        LongType.coerceUserInput(new Date).isLeft should be (true)
      }

      "BigInt" in {
        BigIntType.coerceUserInput(123) should be (Right(BigInt("123")))

        BigIntType.coerceUserInput(123L) should be (Right(BigInt("123")))
        BigIntType.coerceUserInput(12334324234L) should be (Right(BigInt("12334324234")))

        BigIntType.coerceUserInput(BigInt("123234")) should be (Right(BigInt("123234")))
        BigIntType.coerceUserInput(BigInt("12323443874982374987329749823")) should be (
          Right(BigInt("12323443874982374987329749823")))

        BigIntType.coerceUserInput(12.34).isLeft should be (true)
        BigIntType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        BigIntType.coerceUserInput(true).isLeft should be (true)
        BigIntType.coerceUserInput("123").isLeft should be (true)
        BigIntType.coerceUserInput(new Date).isLeft should be (true)
      }

      "Float" in {
        FloatType.coerceUserInput(123) should be (Right(123.0D))

        FloatType.coerceUserInput(123L) should be (Right(123.0D))
        FloatType.coerceUserInput(12334324234L) should be (Right(12334324234.0D))

        FloatType.coerceUserInput(BigInt("123234")) should be (Right(123234.0D))
        FloatType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        FloatType.coerceUserInput(12.34) should be (Right(12.34D))
        FloatType.coerceUserInput(Double.NaN).right.exists(_.isNaN) should be (true)
        FloatType.coerceUserInput(Double.PositiveInfinity).right.exists(_.isPosInfinity) should be (true)
        FloatType.coerceUserInput(Double.NegativeInfinity).right.exists(_.isNegInfinity) should be (true)
        FloatType.coerceUserInput(BigDecimal(12.34)) should be (Right(12.34D))
        FloatType.coerceUserInput(BigDecimal("367476315476516457632.473854635267452376546732")).isLeft should be (true)
        FloatType.coerceUserInput(true).isLeft should be (true)
        FloatType.coerceUserInput("123").isLeft should be (true)
        FloatType.coerceUserInput(new Date).isLeft should be (true)
      }

      "BigDecimal" in {
        BigDecimalType.coerceUserInput(123) should be (Right(BigDecimal(123.0D)))

        BigDecimalType.coerceUserInput(123L) should be (Right(BigDecimal(123.0D)))
        BigDecimalType.coerceUserInput(12334324234L) should be (Right(BigDecimal(12334324234.0D)))

        BigDecimalType.coerceUserInput(BigInt("123234")) should be (Right(BigDecimal(123234.0D)))
        BigDecimalType.coerceUserInput(BigInt("12323443874982374987329749823")) should be (
          Right(BigDecimal("12323443874982374987329749823")))

        BigDecimalType.coerceUserInput(12.34) should be (Right(BigDecimal(12.34D)))
        BigDecimalType.coerceUserInput(BigDecimal(12.34)) should be (Right(BigDecimal(12.34D)))
        BigDecimalType.coerceUserInput(BigDecimal("367476315476516457632.473854635267452376546732")) should be (
          Right(BigDecimal("367476315476516457632.473854635267452376546732")))

        BigDecimalType.coerceUserInput(true).isLeft should be (true)
        BigDecimalType.coerceUserInput("123").isLeft should be (true)
        BigDecimalType.coerceUserInput(new Date).isLeft should be (true)
      }

      "Boolean" in {
        BooleanType.coerceUserInput(123).isLeft should be (true)

        BooleanType.coerceUserInput(1232342323L).isLeft should be (true)

        BooleanType.coerceUserInput(BigInt("123234")).isLeft should be (true)
        BooleanType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        BooleanType.coerceUserInput(12.34).isLeft should be (true)
        BooleanType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        BooleanType.coerceUserInput(true) should be (Right(true))
        BooleanType.coerceUserInput(false) should be (Right(false))
        BooleanType.coerceUserInput("123").isLeft should be (true)
      }

      "String" in {
        StringType.coerceUserInput(123).isLeft should be (true)

        BooleanType.coerceUserInput(1232342323L).isLeft should be (true)

        StringType.coerceUserInput(BigInt("123234")).isLeft should be (true)
        StringType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        StringType.coerceUserInput(12.34).isLeft should be (true)
        StringType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        StringType.coerceUserInput(true).isLeft should be (true)
        StringType.coerceUserInput("123") should be (Right("123"))
        StringType.coerceUserInput("") should be (Right(""))
      }
    }
  }
}
