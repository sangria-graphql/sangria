package sangria.util

import java.util.regex.Pattern

import net.jcazevedo.moultingyaml.{YamlNumber, YamlArray, YamlString, YamlValue}
import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.parser.QueryParser
import sangria.schema.Schema
import sangria.validation._
import spray.json.JsValue
import sangria.parser.DeliveryScheme.Throw

trait CatsSupport { this: WordSpec with Matchers ⇒
  implicit class YamlOps(value: YamlValue) {
    def get(key: String) = value.asYamlObject.fields.get(YamlString(key))
    def apply(key: String) = get(key).get
    def stringValue = value.asInstanceOf[YamlString].value
    def arrayValue = value.asInstanceOf[YamlArray].elements
    def intValue = value.asInstanceOf[YamlNumber[_]].value match {
      case i: Int ⇒ i
      case v ⇒ throw new IllegalArgumentException(s"Unsupported Int '$v' of class '${v.getClass}'.")
    }
  }

  def getSchema(value: Option[YamlValue], path: String): Option[ast.Document] =
    value flatMap (getSchema(_, path))

  def getSchema(value: YamlValue, path: String): Option[ast.Document] =
    value.get("schema")
      .map(v ⇒ QueryParser.parse(v.stringValue))
      .orElse(value.get("schema-file").map(f ⇒ FileUtil.loadSchema(path + "/" + f.stringValue)))

  def getAction(value: YamlValue, testName: String): Action = {
    val when = value("when")

    when.get("validate")
      .map(v ⇒ Validate(v.arrayValue.toList.map(name ⇒ QueryValidator.allRules.find(_.getClass.getSimpleName == name.stringValue) getOrElse (throw new IllegalStateException(s"Can't find the validation rule: $name")))))
      .orElse {
        when.get("execute").map(_ ⇒ Execute)
      }
      .getOrElse(throw new IllegalStateException(s"Can't find action: $testName"))
  }

  def getErrorLocation(value: YamlValue) = value match {
    case YamlArray(elems) ⇒ ErrorLocation(elems(0).intValue, elems(1).intValue)
    case obj ⇒ ErrorLocation(obj("line").intValue, obj("column").intValue)
  }

  def getErrorLocations(value: YamlValue) =
    value.get("loc") match {
      case Some(YamlArray(values)) ⇒ values map getErrorLocation
      case Some(value) ⇒ Vector(getErrorLocation(value))
      case None ⇒ Vector.empty
    }

  def convertToJson(value: YamlValue): JsValue = {
    ???
  }

  def getAssertion(value: YamlValue, testName: String): Assertion = {
    value.get("passes").map(_ ⇒ Passes)
      .orElse(value.get("error-count").map(v ⇒ ErrorsCount(v.intValue)))
      .orElse(value.get("error").map(v ⇒ ErrorsContain(Left(v.stringValue), getErrorLocations(value).toList)))
      .orElse(value.get("error-regex").map(v ⇒ ErrorsContain(Right(v.stringValue.r.pattern), getErrorLocations(value).toList)))
      .orElse(value.get("data").map(v ⇒ Data(convertToJson(v))))
      .getOrElse(throw new IllegalStateException(s"Can't find the assertion: $testName"))
  }

  def getAssertions(value: YamlValue, testName: String): Vector[Assertion] = {
    val thenWord = value("then")

    thenWord match {
      case YamlArray(elems) ⇒
        elems map (getAssertion(_, testName))
      case other ⇒
        Vector(getAssertion(other, testName))
    }
  }

  def getGiven(value: YamlValue, schema: Option[Schema[Unit, Unit]], path: String) = {
    val given = value("given")

    val query = QueryParser.parse(given("query").stringValue)
    val currSchema = getSchema(given, path) map Schema.buildFromAst orElse schema getOrElse (throw new IllegalStateException("No schema provided!"))

    Given(query, currSchema)
  }

  def executeAction(given: Given[Unit, Unit], action: Action) = action match {
    case Validate(rules) ⇒ ValidationResult(new RuleBasedQueryValidator(rules.toList).validateQuery(given.schema, given.query))
    case a ⇒ throw new IllegalStateException(s"Not yet supported action: $a")
  }

  def assetLocations(violation: Violation, locations: List[ErrorLocation]) = {
    if (!violation.isInstanceOf[AstNodeLocation] && locations.nonEmpty)
      fail(s"Locations are empty for violation: ${violation.errorMessage}")

    val withLoc = violation.asInstanceOf[AstNodeLocation]

    withClue(s"Violation does not have all positions: ${violation.errorMessage}") {
      withLoc.positions should have size locations.size
    }

    withLoc.positions.zipWithIndex foreach { case (pos, idx) ⇒
      withClue(s"Violation position mismatch (line: ${locations(idx).line}, column: ${locations(idx).column}): ${violation.errorMessage}") {
        ErrorLocation(pos.line, pos.column) should be(locations(idx))
      }
    }

  }

  def assertResult(result: Result, assertion: Assertion) = (result, assertion) match {
    case (ValidationResult(violations), Passes) ⇒
      violations should have size 0
    case (ValidationResult(violations), ErrorsCount(count)) ⇒
      violations should have size count
    case (ValidationResult(violations), ErrorsContain(message, locations)) ⇒
      message match {
        case Left(text) ⇒
          val v = withClue(s"Can't find error message: $text") {
            val v = violations.find(_.errorMessage.contains(text))

            v should not be ('empty)
            v
          }

          assetLocations(v.get, locations)
        case Right(pattern) ⇒
          val v = withClue(s"Can't find error pattern: $pattern") {
            val v = violations.find(v ⇒ pattern.matcher(v.errorMessage).matches)

            v should not be ('empty)
            v
          }

          assetLocations(v.get, locations)
      }

    case a ⇒ throw new IllegalStateException(s"Not yet supported assertion: $a")
  }

  def generateTests(path: String) = {
    FileUtil.loadScenarios(path) foreach { file ⇒
      val scenario = file.scenario

      scenario("scenario").stringValue should {
        val schema = getSchema(scenario.get("background"), file.folder) map Schema.buildFromAst

        scenario("tests").arrayValue foreach { test ⇒
          val testName = test("name").stringValue

          testName in {
            val given = getGiven(test, schema, file.folder)
            val action = getAction(test, testName)
            val assertions = getAssertions(test, testName)

            val result = executeAction(given, action)

            assertions foreach { a ⇒
              assertResult(result, a)
            }
          }
        }
      }
    }
  }

  case class Given[Ctx, Val](query: ast.Document, schema: Schema[Ctx, Val])

  sealed trait Action

  case class Validate(rules: List[ValidationRule]) extends Action
  case object Execute extends Action

  sealed trait Result

  case class ValidationResult(violations: Vector[Violation]) extends Result

  sealed trait Assertion

  case object Passes extends Assertion
  case class Data(json: JsValue) extends Assertion
  case class ErrorsCount(count: Int) extends Assertion
  case class ErrorsContain(message: Either[String, Pattern], locations: List[ErrorLocation]) extends Assertion

  case class ErrorLocation(line: Int, column: Int)
}
