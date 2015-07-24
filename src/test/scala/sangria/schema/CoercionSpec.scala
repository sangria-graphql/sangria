package sangria.schema

import java.util.Date

import org.scalatest.{Matchers, WordSpec}
import sangria.ast._

class CoercionSpec extends WordSpec with Matchers {
  "Type System: Scalar coercion" when {
    "coerces output" should {
      "Int" in {
        IntType.coerceOutput(1) should be (IntValue(1))
        IntType.coerceOutput(Integer.MAX_VALUE) should be (IntValue(Integer.MAX_VALUE))
        IntType.coerceOutput(Integer.MIN_VALUE) should be (IntValue(Integer.MIN_VALUE))
      }

      "BigInt" in {
        BigIntType.coerceOutput(BigInt("89493264763287")) should be (BigIntValue(BigInt("89493264763287")))
      }

      "Float" in {
        FloatType.coerceOutput(123.456) should be (FloatValue(123.456))
      }

      "BigDecimal" in {
        BigDecimalType.coerceOutput(BigDecimal("467356453726547623.37467823648763238479823")) should be (
          BigDecimalValue(BigDecimal("467356453726547623.37467823648763238479823")))
      }

      "Boolean" in {
        BooleanType.coerceOutput(true) should be (BooleanValue(true))
        BooleanType.coerceOutput(false) should be (BooleanValue(false))
      }

      "String" in {
        StringType.coerceOutput("foo") should be (StringValue("foo"))
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
      }
    }

    "coerces user input" should {
      "Int" in {
        IntType.coerceUserInput(123) should be (Right(123))

        IntType.coerceUserInput(BigInt("123234")) should be (Right(123234))
        IntType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        IntType.coerceUserInput(12.34D).isLeft should be (true)
        IntType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        IntType.coerceUserInput(true).isLeft should be (true)
        IntType.coerceUserInput("123").isLeft should be (true)
        IntType.coerceUserInput(new Date).isLeft should be (true)
      }

      "BigInt" in {
        BigIntType.coerceUserInput(123) should be (Right(BigInt("123")))

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

        FloatType.coerceUserInput(BigInt("123234")) should be (Right(123234.0D))
        FloatType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        FloatType.coerceUserInput(12.34) should be (Right(12.34D))
        FloatType.coerceUserInput(BigDecimal(12.34)) should be (Right(12.34D))
        FloatType.coerceUserInput(BigDecimal("367476315476516457632.473854635267452376546732")).isLeft should be (true)
        FloatType.coerceUserInput(true).isLeft should be (true)
        FloatType.coerceUserInput("123").isLeft should be (true)
        FloatType.coerceUserInput(new Date).isLeft should be (true)
      }

      "BigDecimal" in {
        BigDecimalType.coerceUserInput(123) should be (Right(BigDecimal(123.0D)))

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

        BooleanType.coerceUserInput(BigInt("123234")).isLeft should be (true)
        BooleanType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        BooleanType.coerceUserInput(12.34).isLeft should be (true)
        BooleanType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        BooleanType.coerceUserInput(true) should be (Right(true))
        BooleanType.coerceUserInput("123").isLeft should be (true)
      }

      "String" in {
        StringType.coerceUserInput(123).isLeft should be (true)

        StringType.coerceUserInput(BigInt("123234")).isLeft should be (true)
        StringType.coerceUserInput(BigInt("12323443874982374987329749823")).isLeft should be (true)

        StringType.coerceUserInput(12.34).isLeft should be (true)
        StringType.coerceUserInput(BigDecimal(12.34)).isLeft should be (true)
        StringType.coerceUserInput(true).isLeft should be (true)
        StringType.coerceUserInput("123") should be (Right("123"))
      }
    }
  }
}
