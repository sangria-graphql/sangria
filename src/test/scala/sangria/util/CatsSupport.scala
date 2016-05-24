package sangria.util

import java.util.regex.Pattern

import net.jcazevedo.moultingyaml._
import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.ast.{ObjectTypeDefinition, FieldDefinition, TypeDefinition}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation._
import spray.json._
import sangria.parser.DeliveryScheme.Throw
import sangria.marshalling.sprayJson._
import scala.concurrent.ExecutionContext.Implicits.global

trait CatsSupport extends FutureResultSupport { this: WordSpec with Matchers ⇒
  implicit class YamlOps(value: YamlValue) {
    def get(key: String) = value match {
      case YamlObject(fields) ⇒ fields.get(YamlString(key))
      case _ ⇒ None
    }
    def apply(key: String) = get(key).get
    def stringValue = value.asInstanceOf[YamlString].value
    def arrayValue = value.asInstanceOf[YamlArray].elements
    def booleanValue = value.asInstanceOf[YamlBoolean].boolean
    def intValue = value.asInstanceOf[YamlNumber[_]].value match {
      case i: Int ⇒ i
      case v ⇒ throw new IllegalArgumentException(s"Unsupported Int '$v' of class '${v.getClass}'.")
    }
  }

  implicit class JsonOps(value: JsValue) {
    def get(key: String) = value match {
      case JsObject(fields) ⇒ fields.get(key)
      case _ ⇒ None
    }
    def apply(key: String) = get(key).get
    def stringValue = value.asInstanceOf[JsString].value
    def arrayValue = value.asInstanceOf[JsArray].elements
    def booleanValue = value.asInstanceOf[JsBoolean].value
    def intValue = value.asInstanceOf[JsNumber].value.intValue
  }

  def extractCorrectValue(tpe: OutputType[_], value: Option[JsValue]): Any = tpe match {
    case OptionType(ofType) ⇒ Option(extractCorrectValue(ofType, value))
    case _ if value.isEmpty || value.get == JsNull ⇒ null
    case ListType(ofType) ⇒ value.get.arrayValue map (v ⇒ extractCorrectValue(ofType, Option(v)))
    case t: ScalarType[_] if t eq BooleanType ⇒ value.get.booleanValue
    case t: ScalarType[_] if t eq StringType ⇒ value.get.stringValue
    case t: ScalarType[_] if t eq IntType ⇒ value.get.intValue
    case t: CompositeType[_] ⇒ value.get.asJsObject
    case t ⇒ throw new IllegalStateException(s"Builder for type '$t' is not supported yet.")
  }

  def schemaBuilder(testData: JsValue) = new DefaultAstSchemaBuilder[Any] {
    override def resolveField(typeDefinition: TypeDefinition, definition: FieldDefinition) =
      c ⇒ extractCorrectValue(c.field.fieldType, c.value.asInstanceOf[JsValue].get(definition.name))

    override def objectTypeInstanceCheck(definition: ObjectTypeDefinition, extensions: List[ast.TypeExtensionDefinition]) =
      Some((value, _) ⇒ value.asInstanceOf[JsValue].get("type").map(_.stringValue == definition.name) getOrElse false)
  }

  def getSchema(value: Option[YamlValue], path: String): Option[ast.Document] =
    value flatMap (getSchema(_, path))

  def getSchema(value: YamlValue, path: String): Option[ast.Document] =
    value.get("schema")
      .map(v ⇒ QueryParser.parse(v.stringValue))
      .orElse(value.get("schema-file").map(f ⇒ FileUtil.loadSchema(path + "/" + f.stringValue)))

  def getTestData(value: Option[YamlValue]) =
    value flatMap (_.get("test-data") map convertToJson) getOrElse JsObject.empty

  def getAction(value: YamlValue, testName: String, testData: JsValue): Action = {
    val when = value("when")

    when.get("validate")
      .map(v ⇒ Validate(v.arrayValue.toList.map(name ⇒ QueryValidator.allRules.find(_.getClass.getSimpleName == name.stringValue) getOrElse (throw new IllegalStateException(s"Can't find the validation rule: $name")))))
      .orElse {
        when.get("execute").map { e ⇒
          val validate = e.get("validate-query").map(_.booleanValue) getOrElse true
          val value = e.get("test-value").map(name ⇒ testData(name.stringValue)) getOrElse JsNull

          Execute(validate, value)
        }
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

  def convertToJson(value: YamlValue): JsValue = value match {
    case YamlArray(elems) ⇒ JsArray(elems map convertToJson)
    case YamlObject(fields) ⇒ JsObject(fields.map {case (k, v) ⇒ k.stringValue → convertToJson(v)})
    case YamlBoolean(v) ⇒ JsBoolean(v)
    case YamlString(v) ⇒ JsString(v)
    case YamlNumber(v: Int) ⇒ JsNumber(v)
    case YamlNumber(v: Long) ⇒ JsNumber(v)
    case YamlNumber(v: BigInt) ⇒ JsNumber(v)
    case YamlNumber(v: BigDecimal) ⇒ JsNumber(v)
    case YamlNumber(v: Double) ⇒ JsNumber(v)
    case YamlNull ⇒ JsNull
    case v ⇒ throw new IllegalStateException(s"Yaml value is not supported in conversion: $v")
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

  def getGiven(value: YamlValue, schema: Option[Schema[Any, Any]], path: String, builder: AstSchemaBuilder[Any]) = {
    val given = value("given")

    val query = QueryParser.parse(given("query").stringValue)
    val currSchema = getSchema(given, path) map (Schema.buildFromAst(_, builder)) orElse schema getOrElse (throw new IllegalStateException("No schema provided!"))

    Given(query, currSchema)
  }

  def executeAction(given: Given[Any, Any], action: Action) = action match {
    case Validate(rules) ⇒ ValidationResult(new RuleBasedQueryValidator(rules.toList).validateQuery(given.schema, given.query))
    case Execute(validate, value) ⇒
      val validator = if (validate) QueryValidator.default else QueryValidator.empty

      ExecutionResult(Executor.execute(given.schema, given.query, root = value, queryValidator = validator).await)
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

    case (ExecutionResult(actual), Data(expected)) ⇒
      actual.get("errors") match {
        case Some(_) ⇒
         fail("Errors detected: " + actual)
        case None ⇒
          actual("data") should be (expected)
      }

    case a ⇒ throw new IllegalStateException(s"Not yet supported assertion: $a")
  }

  def generateTests(path: String) = {
    FileUtil.loadScenarios(path) foreach { file ⇒
      val scenario = file.scenario

      scenario("scenario").stringValue should {
        val testData = getTestData(scenario.get("background"))
        val builder = schemaBuilder(testData)
        val schema = getSchema(scenario.get("background"), file.folder) map (Schema.buildFromAst(_, builder))

        scenario("tests").arrayValue foreach { test ⇒
          val testName = test("name").stringValue

          testName in {
            val given = getGiven(test, schema, file.folder, builder)
            val action = getAction(test, testName, testData)
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
  case class Execute(validate: Boolean, value: JsValue) extends Action

  sealed trait Result

  case class ValidationResult(violations: Vector[Violation]) extends Result
  case class ExecutionResult(value: JsValue) extends Result

  sealed trait Assertion

  case object Passes extends Assertion
  case class Data(json: JsValue) extends Assertion
  case class ErrorsCount(count: Int) extends Assertion
  case class ErrorsContain(message: Either[String, Pattern], locations: List[ErrorLocation]) extends Assertion

  case class ErrorLocation(line: Int, column: Int)
}
