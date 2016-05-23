package sangria.util

import org.scalatest.Matchers
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.{AstNodeViolation, RuleBasedQueryValidator, ValidationRule}

import scala.util.Success

trait ValidationSupport extends Matchers {
  type TestField = Field[Unit, Unit]

  val Being = InterfaceType("Being", List[TestField](
    Field("name", OptionType(StringType), resolve = _ ⇒ None)
  ))

  val Pet = InterfaceType("Pet", List[TestField](
    Field("name", OptionType(StringType), resolve = _ ⇒ None)
  ))

  val Canine = InterfaceType("Canine", List[TestField](
    Field("name", OptionType(StringType),
      arguments = Argument("surname", OptionInputType(BooleanType)) :: Nil,
      resolve = _ ⇒ None)
  ))

  val DogCommand = EnumType("DogCommand", values = List(
    EnumValue("SIT", value = 0),
    EnumValue("HEEL", value = 1),
    EnumValue("DOWN", value = 2)
  ))

  val FurColor = EnumType("FurColor", values = List(
    EnumValue("BROWN", value = 0),
    EnumValue("BLACK", value = 1),
    EnumValue("TAN", value = 2),
    EnumValue("SPOTTED", value = 3)
  ))

  val Dog = ObjectType("Dog", interfaces[Unit, Unit](Being, Pet, Canine), List[TestField](
    Field("name", OptionType(StringType),
      arguments = Argument("surname", OptionInputType(BooleanType)) :: Nil,
      resolve = _ ⇒ None),
    Field("nickname", OptionType(StringType), resolve = _ ⇒ None),
    Field("barks", OptionType(BooleanType), resolve = _ ⇒ None),
    Field("barkVolume", OptionType(IntType), resolve = _ ⇒ None),
    Field("doesKnowCommand", OptionType(BooleanType),
      arguments = Argument("dogCommand", OptionInputType(DogCommand)) :: Nil,
      resolve = _ ⇒ None),
    Field("isHousetrained", OptionType(BooleanType),
      arguments = Argument("atOtherHomes", OptionInputType(BooleanType), true) :: Nil,
      resolve = _ ⇒ None),
    Field("isAtLocation", OptionType(BooleanType),
      arguments = Argument("x", OptionInputType(IntType)) :: Argument("y", OptionInputType(IntType)) :: Nil,
      resolve = _ ⇒ None)))

  val Cat = ObjectType("Cat", interfaces[Unit, Unit](Being, Pet), List[TestField](
    Field("name", OptionType(StringType), resolve = _ ⇒ None),
    Field("nickname", OptionType(StringType), resolve = _ ⇒ None),
    Field("meows", OptionType(BooleanType), resolve = _ ⇒ None),
    Field("meowVolume", OptionType(IntType), resolve = _ ⇒ None),
    Field("furColor", OptionType(FurColor), resolve = _ ⇒ None)))

  val CatOrDog = UnionType("CatOrDog", types = Dog :: Cat :: Nil)

  val Intelligent = InterfaceType("Intelligent", List[TestField](
    Field("iq", OptionType(IntType), resolve = _ ⇒ None)
  ))

  val Human: ObjectType[Unit, Unit] = ObjectType("Human", interfaces[Unit, Unit](Being, Intelligent), () ⇒ List[TestField](
    Field("name", OptionType(StringType),
      arguments = Argument("surname", OptionInputType(BooleanType)) :: Nil,
      resolve = _ ⇒ None),
    Field("pets", OptionType(ListType(OptionType(Pet))), resolve = _ ⇒ None),
    Field("relatives", OptionType(ListType(OptionType(Human))), resolve = _ ⇒ None)))

  val Alien = ObjectType("Alien", interfaces[Unit, Unit](Being, Intelligent), List[TestField](
    Field("numEyes", OptionType(IntType), resolve = _ ⇒ None)))

  val DogOrHuman = UnionType("DogOrHuman", types = Dog :: Human :: Nil)

  val HumanOrAlien = UnionType("HumanOrAlien", types = Human :: Alien :: Nil)

  val ComplexInput = InputObjectType("ComplexInput", List(
    InputField("requiredField", BooleanType),
    InputField("intField", OptionInputType(IntType)),
    InputField("stringField", OptionInputType(StringType)),
    InputField("booleanField", OptionInputType(BooleanType)),
    InputField("stringListField", OptionInputType(ListInputType(OptionInputType(StringType))))
  ))

  val ComplicatedArgs = ObjectType("ComplicatedArgs", List[TestField](
    Field("intArgField", OptionType(StringType),
      arguments = Argument("intArg", OptionInputType(IntType)) :: Nil,
      resolve = _ ⇒ None),
    Field("bigIntArgField", OptionType(StringType),
      arguments = Argument("bigIntArg", OptionInputType(BigIntType)) :: Nil,
      resolve = _ ⇒ None),
    Field("nonNullIntArgField", OptionType(StringType),
      arguments = Argument("nonNullIntArg", IntType) :: Nil,
      resolve = _ ⇒ None),
    Field("stringArgField", OptionType(StringType),
      arguments = Argument("stringArg", OptionInputType(StringType)) :: Nil,
      resolve = _ ⇒ None),
    Field("booleanArgField", OptionType(StringType),
      arguments = Argument("booleanArg", OptionInputType(BooleanType)) :: Nil,
      resolve = _ ⇒ None),
    Field("enumArgField", OptionType(StringType),
      arguments = Argument("enumArg", OptionInputType(FurColor)) :: Nil,
      resolve = _ ⇒ None),
    Field("floatArgField", OptionType(StringType),
      arguments = Argument("floatArg", OptionInputType(FloatType)) :: Nil,
      resolve = _ ⇒ None),
    Field("bigDecimalArgField", OptionType(StringType),
      arguments = Argument("bigDecimalArg", OptionInputType(BigDecimalType)) :: Nil,
      resolve = _ ⇒ None),
    Field("idArgField", OptionType(StringType),
      arguments = Argument("idArg", OptionInputType(IDType)) :: Nil,
      resolve = _ ⇒ None),
    Field("stringListArgField", OptionType(StringType),
      arguments = Argument("stringListArg", OptionInputType(ListInputType(OptionInputType(StringType)))) :: Nil,
      resolve = _ ⇒ None),
    Field("complexArgField", OptionType(StringType),
      arguments = Argument("complexArg", OptionInputType(ComplexInput)) :: Nil,
      resolve = _ ⇒ None),
    Field("multipleReqs", OptionType(StringType),
      arguments = Argument("req1", IntType) :: Argument("req2", IntType) :: Nil,
      resolve = _ ⇒ None),
    Field("multipleOpts", OptionType(StringType),
      arguments = Argument("opt1", OptionInputType(IntType), 0) :: Argument("opt2", OptionInputType(IntType), 0) :: Nil,
      resolve = _ ⇒ None),
    Field("multipleOptAndReq", OptionType(StringType),
      arguments =
          Argument("req1", IntType) ::
          Argument("req2", IntType) ::
          Argument("opt1", OptionInputType(IntType), 0) ::
          Argument("opt2", OptionInputType(IntType), 0) ::
          Nil,
      resolve = _ ⇒ None)
  ))

  val QueryRoot = ObjectType("QueryRoot", List[TestField](
    Field("human", OptionType(Human),
      arguments = Argument("id", OptionInputType(IDType)) :: Nil,
      resolve = _ ⇒ None),
    Field("alien", OptionType(Alien), resolve = _ ⇒ None),
    Field("dog", OptionType(Dog), resolve = _ ⇒ None),
    Field("cat", OptionType(Cat), resolve = _ ⇒ None),
    Field("pet", OptionType(Pet), resolve = _ ⇒ None),
    Field("catOrDog", OptionType(CatOrDog), resolve = _ ⇒ None),
    Field("dogOrHuman", OptionType(DogOrHuman), resolve = _ ⇒ None),
    Field("humanOrAlien", OptionType(HumanOrAlien), resolve = _ ⇒ None),
    Field("complicatedArgs", OptionType(ComplicatedArgs), resolve = _ ⇒ None)
  ))

  private def alwaysInclude(ctx: DirectiveContext): Boolean = true

  val schema = Schema(QueryRoot, directives = BuiltinDirectives ++ List(
    Directive("onQuery", locations = Set(DirectiveLocation.Query), shouldInclude = alwaysInclude),
    Directive("onMutation", locations = Set(DirectiveLocation.Mutation), shouldInclude = alwaysInclude),
    Directive("onSubscription", locations = Set(DirectiveLocation.Subscription), shouldInclude = alwaysInclude),
    Directive("onField", locations = Set(DirectiveLocation.Field), shouldInclude = alwaysInclude),
    Directive("onFragmentDefinition", locations = Set(DirectiveLocation.FragmentDefinition), shouldInclude = alwaysInclude),
    Directive("onFragmentSpread", locations = Set(DirectiveLocation.FragmentSpread), shouldInclude = alwaysInclude),
    Directive("onInlineFragment", locations = Set(DirectiveLocation.InlineFragment), shouldInclude = alwaysInclude),
    Directive("onSchema", locations = Set(DirectiveLocation.Schema), shouldInclude = alwaysInclude),
    Directive("onScalar", locations = Set(DirectiveLocation.Scalar), shouldInclude = alwaysInclude),
    Directive("onObject", locations = Set(DirectiveLocation.Object), shouldInclude = alwaysInclude),
    Directive("onFieldDefinition", locations = Set(DirectiveLocation.FieldDefinition), shouldInclude = alwaysInclude),
    Directive("onArgumentDefinition", locations = Set(DirectiveLocation.ArgumentDefinition), shouldInclude = alwaysInclude),
    Directive("onInterface", locations = Set(DirectiveLocation.Interface), shouldInclude = alwaysInclude),
    Directive("onUnion", locations = Set(DirectiveLocation.Union), shouldInclude = alwaysInclude),
    Directive("onEnum", locations = Set(DirectiveLocation.Enum), shouldInclude = alwaysInclude),
    Directive("onEnumValue", locations = Set(DirectiveLocation.EnumValue), shouldInclude = alwaysInclude),
    Directive("onInputObject", locations = Set(DirectiveLocation.InputObject), shouldInclude = alwaysInclude),
    Directive("onInputFieldDefinition", locations = Set(DirectiveLocation.InputFieldDefinition), shouldInclude = alwaysInclude)))

  def defaultRule: Option[ValidationRule] = None

  def expectValid(s: Schema[_, _], rules: List[ValidationRule], query: String) = {
    val Success(doc) = QueryParser.parse(query)

    withClue("Should validate") {
      validator(rules).validateQuery(s, doc) should have size 0
    }
  }

  def expectInvalid(s: Schema[_, _], rules: List[ValidationRule], query: String, expectedErrors: List[(String, List[Pos])]) = {
    val Success(doc) = QueryParser.parse(query)

    withClue("Should not validate") {
      val errors = validator(rules).validateQuery(s, doc)

      errors should have size expectedErrors.size

      expectedErrors foreach { case(expected, pos) ⇒
        withClue(s"Expected error not found: $expected${pos map (p ⇒ s" (line ${p.line}, column ${p.col})") mkString "; "}. Actual:\n${errors map (_.errorMessage) mkString "\n"}") {
          errors exists { error ⇒
            error.errorMessage.contains(expected) && {
              val errorPositions = error.asInstanceOf[AstNodeViolation].positions

              errorPositions should have size pos.size

              errorPositions zip pos forall { case (actualPos, expectedPos) ⇒
                expectedPos.line == actualPos.line && expectedPos.col == actualPos.column
              }
            }
          } should be (true)
        }
      }
      
    }
  }

  def expectPassesRule(rule: ValidationRule, query: String) =
    expectValid(schema, rule :: Nil, query)

  def expectPasses(query: String) =
    expectValid(schema, defaultRule.get :: Nil, query)

  def expectFailsRule(rule: ValidationRule, query: String, expectedErrors: List[(String, Option[Pos])]) =
    expectInvalid(schema, rule :: Nil, query, expectedErrors.map{case (msg, pos) ⇒ msg → pos.toList})

  def expectFails(query: String, expectedErrors: List[(String, Option[Pos])]) =
    expectInvalid(schema, defaultRule.get :: Nil, query, expectedErrors.map{case (msg, pos) ⇒ msg → pos.toList})

  def expectFailsPosList(query: String, expectedErrors: List[(String, List[Pos])]) =
    expectInvalid(schema, defaultRule.get :: Nil, query, expectedErrors)

  def validator(rules: List[ValidationRule]) = new RuleBasedQueryValidator(rules)
}
