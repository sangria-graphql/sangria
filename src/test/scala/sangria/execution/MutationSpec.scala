package sangria.execution

import java.util.Random
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.schema._
import sangria.util.{FutureResultSupport, GraphQlSupport, SimpleGraphQlSupport}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class MutationSpec extends WordSpec with Matchers with GraphQlSupport {
  case class SuccessfulDefer(num: NumberHolder) extends Deferred[NumberHolder]
  case class FailedDefer(num: NumberHolder) extends Deferred[NumberHolder]

  class Resolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
      case SuccessfulDefer(n) ⇒ Future.successful(n)
      case FailedDefer(_) ⇒ Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  class NumberHolder(initial: Int) {
    val theNumber = new AtomicInteger(initial)

    def getAndSet(newNumber: Int) =
      theNumber getAndSet newNumber
  }

  class Root(initial: Int) {
    val numberHolder = new NumberHolder(initial)
    val rnd = new Random

    def immediatelyChangeTheNumber(newNumber: Option[Int]) = {
      newNumber foreach (numberHolder.getAndSet)
      numberHolder
    }

    def promiseToChangeTheNumber(newNumber: Option[Int]) =
      Future {
        Thread.sleep(rnd nextInt 50)
        newNumber foreach (numberHolder.getAndSet)
        Thread.sleep(rnd nextInt 50)
        numberHolder
      }

    def failToChangeTheNumber(newNumber: Option[Int]): NumberHolder =
      throw new IllegalStateException("Cannot change the number")

    def promiseAndFailToChangeTheNumber(newNumber: Option[Int]): Future[NumberHolder] =
      Future {
        Thread.sleep(rnd nextInt 50)
        throw new IllegalStateException("Cannot change the number")
      }
  }

  case class UserContext(num: Int)

  val NumberHolderType = ObjectType("NumberHolder", fields[UserContext, NumberHolder](
    Field("theNumber", OptionType(IntType), resolve = _.value.theNumber.get()),
    Field("userCtx", OptionType(IntType), resolve = _.ctx.num)
  ))

  val NewNumberArg = Argument("newNumber", OptionInputType(IntType))

  val schema = Schema(
    ObjectType("Query", fields[UserContext, Root](
      Field("numberHolder", OptionType(NumberHolderType), resolve = _.value.numberHolder)
    )),
    Some(ObjectType("Mutation", fields[UserContext, Root](
      Field("immediatelyChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(ctx.value.immediatelyChangeTheNumber(ctx.arg(NewNumberArg)))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("deferChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(SuccessfulDefer(ctx.value.immediatelyChangeTheNumber(ctx.arg(NewNumberArg))))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("deferFailChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(FailedDefer(ctx.value.immediatelyChangeTheNumber(ctx.arg(NewNumberArg))))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("deferFutChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(DeferredFutureValue(Future.successful(SuccessfulDefer(ctx.value.immediatelyChangeTheNumber(ctx.arg(NewNumberArg))))))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("deferFutFailChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(DeferredFutureValue(Future.successful(FailedDefer(ctx.value.immediatelyChangeTheNumber(ctx.arg(NewNumberArg))))))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("promiseToChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(ctx.value.promiseToChangeTheNumber(ctx.arg(NewNumberArg)))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("failToChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(ctx.value.failToChangeTheNumber(ctx.arg(NewNumberArg)))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get()))),
      Field("promiseAndFailToChangeTheNumber", OptionType(NumberHolderType),
        arguments = NewNumberArg :: Nil,
        resolve = ctx ⇒ UpdateCtx(ctx.value.promiseAndFailToChangeTheNumber(ctx.arg(NewNumberArg)))(v ⇒ ctx.ctx.copy(num = ctx.ctx.num + v.theNumber.get())))
    )))
  )

  "Execute: Handles mutation execution ordering" should {
    "evaluates mutations serially" in check(
      new Root(6),
      """
        mutation M {
          first: immediatelyChangeTheNumber(newNumber: 1) {
            theNumber
            userCtx
          },
          second: promiseToChangeTheNumber(newNumber: 2) {
            theNumber
            userCtx
          },
          third: immediatelyChangeTheNumber(newNumber: 3) {
            theNumber
            userCtx
          }
          fourth: promiseToChangeTheNumber(newNumber: 4) {
            theNumber
            userCtx
          },
          fifth: immediatelyChangeTheNumber(newNumber: 5) {
            theNumber
            userCtx
          }
          def: deferChangeTheNumber(newNumber: 6) {
            theNumber
            userCtx
          }
          defFut: deferFutChangeTheNumber(newNumber: 7) {
            theNumber
            userCtx
          }
        }
      """,
      Map(
        "data" → Map(
          "first"  → Map(
            "theNumber" → 1,
            "userCtx" → 11),
          "second" → Map(
            "theNumber" → 2,
            "userCtx" → 13),
          "third"  → Map(
            "theNumber" → 3,
            "userCtx" → 16),
          "fourth" → Map(
            "theNumber" → 4,
            "userCtx" → 20),
          "fifth"  → Map(
            "theNumber" → 5,
            "userCtx" → 25),
          "def"  → Map(
            "theNumber" → 6,
            "userCtx" → 31),
          "defFut"  → Map(
            "theNumber" → 7,
            "userCtx" → 38)
        )
      ),
      userContext = UserContext(10),
      resolver = new Resolver
    )

    "evaluates mutations correctly in the presence of a failed mutation" in checkErrors(
      new Root(6),
      """
        mutation M {
          first: immediatelyChangeTheNumber(newNumber: 1) {
            theNumber userCtx
          },
          second: promiseToChangeTheNumber(newNumber: 2) {
            theNumber userCtx
          },
          third: failToChangeTheNumber(newNumber: 3) {
            theNumber userCtx
          }
          fourth: promiseToChangeTheNumber(newNumber: 4) {
            theNumber userCtx
          },
          fifth: immediatelyChangeTheNumber(newNumber: 5) {
            theNumber userCtx
          }
          sixth: promiseAndFailToChangeTheNumber(newNumber: 6) {
            theNumber userCtx
          }
          def: deferChangeTheNumber(newNumber: 7) {
            theNumber userCtx
          }
          defFail: deferFailChangeTheNumber(newNumber: 8) {
            theNumber userCtx
          }
          defFut: deferFutChangeTheNumber(newNumber: 9) {
            theNumber userCtx
          }
          defFutFail: deferFutFailChangeTheNumber(newNumber: 10) {
            theNumber userCtx
          }
          def1: deferChangeTheNumber(newNumber: 11) {
            theNumber userCtx
          }
        }
      """,
      Map(
        "first"  → Map(
          "theNumber" → 1,
          "userCtx" → 11),
        "second" → Map(
          "theNumber" → 2,
          "userCtx" → 13),
        "third"  → null,
        "fourth" → Map(
          "theNumber" → 4,
          "userCtx" → 17),
        "fifth"  → Map(
          "theNumber" → 5,
          "userCtx" → 22),
        "sixth"  → null,
        "def"  → Map(
          "theNumber" → 7,
          "userCtx" → 29),
        "defFail"  → null,
        "defFut"  → Map(
          "theNumber" → 9,
          "userCtx" → 38),
        "defFutFail"  → null,
        "def1"  → Map(
          "theNumber" → 11,
          "userCtx" → 49)
      ),
      List(
        Map(
          "message" → "Cannot change the number",
          "path" → List("third"),
          "locations" → List(Map("line" → 9, "column" → 11))),
        Map("message" → "Cannot change the number",
          "path" → List("sixth"),
          "locations" → List(Map("line" → 18, "column" → 11))),
        Map("message" → "error in resolver",
          "path" → List("defFail"),
          "locations" → List(Map("line" → 24, "column" → 11))),
        Map("message" → "error in resolver",
          "path" → List("defFutFail"),
          "locations" → List(Map("line" → 30, "column" → 11)))),
      userContext = UserContext(10),
      resolver = new Resolver
    )

    "propagates updated context to siblings and children" in {
      val child = ObjectType("Child", fields[String, String](
        Field("ctx", StringType, resolve = _.ctx),
        Field("val", StringType, resolve = _.value)
      ))

      val AddArg = Argument("add", StringType)

      val mutation = ObjectType("Mutation", fields[String, Unit](
        Field("updateSimple", child,
          arguments = AddArg :: Nil,
          resolve = c ⇒ UpdateCtx(c.ctx + " " + c.arg(AddArg))(v ⇒ v + " ctx ").map(v ⇒ v + " map")),
        Field("updateFuture", child,
          arguments = AddArg :: Nil,
          resolve = c ⇒ UpdateCtx(Future.successful(c.ctx + " " + c.arg(AddArg)))(v ⇒ v + " ctx ").map(v ⇒ v + " map")),
        Field("updateTry", child,
          arguments = AddArg :: Nil,
          resolve = c ⇒ UpdateCtx(Success(c.ctx + " " + c.arg(AddArg)))(v ⇒ v + " ctx ").map(v ⇒ v + " map"))
      ))

      val schema = Schema(mutation, Some(mutation))

      SimpleGraphQlSupport.check(
        schema,
        (),
        """
        mutation M {
          a: updateSimple(add: "a") {ctx, val}
          b: updateFuture(add: "b") {ctx, val}
          c: updateTry(add: "c") {ctx, val}
        }
        """,
        Map("data" →
            Map(
              "a" → Map("ctx" → "root a ctx ", "val" → "root a map"),
              "b" → Map("ctx" → "root a ctx  b ctx ", "val" → "root a ctx  b map"),
              "c" → Map("ctx" → "root a ctx  b ctx  c ctx ", "val" → "root a ctx  b ctx  c map"))),
        userContext = "root")

      SimpleGraphQlSupport.check(
        schema,
        (),
        """
        query Q {
          a: updateSimple(add: "a") {ctx, val}
          b: updateFuture(add: "b") {ctx, val}
          c: updateTry(add: "c") {ctx, val}
        }
        """,
        Map("data" →
            Map(
              "a" → Map("ctx" → "root a ctx ", "val" → "root a map"),
              "b" → Map("ctx" → "root b ctx ", "val" → "root b map"),
              "c" → Map("ctx" → "root c ctx ", "val" → "root c map"))),
        userContext = "root")
    }
  }
}
