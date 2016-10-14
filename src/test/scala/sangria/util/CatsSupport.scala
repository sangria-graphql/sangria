package sangria.util

import java.util.regex.Pattern

import net.jcazevedo.moultingyaml._
import org.scalatest.{Matchers, WordSpec}
import sangria.{schema, ast}
import sangria.ast.{ObjectTypeDefinition, FieldDefinition, TypeDefinition}
import sangria.execution.{HandledException, Executor}
import sangria.parser.{SyntaxError, QueryParser}
import sangria.schema._
import sangria.validation._
import spray.json._
import sangria.marshalling.sprayJson._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}

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

  def resolveRef(value: JsValue, testData: JsValue) = value match {
    case JsObject(fields) if fields.keySet == Set("$ref") ⇒
      val name = fields("$ref").stringValue

      testData(name)
    case v ⇒ v
  }

  def extractCorrectValue(tpe: OutputType[_], value: Option[JsValue], testData: JsValue): Any = tpe match {
    case OptionType(ofType) ⇒ Option(extractCorrectValue(ofType, value, testData))
    case _ if value.isEmpty || value.get == JsNull ⇒ null
    case ListType(ofType) ⇒ value.get.arrayValue map (v ⇒ extractCorrectValue(ofType, Option(v), testData))
    case t: ScalarType[_] if t eq BooleanType ⇒ resolveRef(value.get, testData).booleanValue
    case t: ScalarType[_] if t eq StringType ⇒ resolveRef(value.get, testData).stringValue
    case t: ScalarType[_] if t eq IntType ⇒ resolveRef(value.get, testData).intValue
    case t: CompositeType[_] ⇒ resolveRef(value.get, testData).asJsObject
    case t ⇒ throw new IllegalStateException(s"Builder for type '$t' is not supported yet.")
  }

  def correctValue(tpe: OutputType[_], value: Any): Any = tpe match {
    case OptionType(_) ⇒ Option(value)
    case _ ⇒ value
  }

  def directiveStringArg(d: ast.Directive, name: String) =
    d.arguments.collectFirst {
      case ast.Argument(`name`, ast.StringValue(str, _, _), _, _) ⇒ str
    }
    .getOrElse(throw new IllegalStateException(s"Can't find a directive argument $name"))

  def directiveStringListArg(d: ast.Directive, name: String) =
    d.arguments.collectFirst {
      case ast.Argument(`name`, ast.ListValue(values, _, _), _, _) ⇒
        values.collect {case ast.StringValue(v, _, _) ⇒ v}
    }
    .getOrElse(throw new IllegalStateException(s"Can't find a directive argument $name"))

  def replacePlaceholders(template: String, args: Args) =
    args.raw.keys.foldLeft(template) {
      case (acc, key) ⇒ acc.replaceAll("\\$" + key, args.arg[Any](key) match {
        case Some(v) ⇒ "" + v
        case None ⇒ ""
        case v ⇒ "" + v
      })
    }

  case class ResolveError(message: String) extends Exception(message)

  def schemaBuilder(testData: JsValue): AstSchemaBuilder[Any] = new DefaultAstSchemaBuilder[Any] {
    val directiveMapping = Map[String, (ast.Directive, FieldDefinition) ⇒ Context[Any, _] ⇒ schema.Action[Any, _]](
      "resolveString" → { (dir, _) ⇒
        val template = directiveStringArg(dir, "value")

        c ⇒ correctValue(c.field.fieldType, replacePlaceholders(template, c.args))
      },

      "argumentsJson" → { (dir, _) ⇒
        c ⇒ {
          def handleValue(v: Any) = v match {
            case v: String ⇒ JsString(v)
            case v: Boolean ⇒ JsBoolean(v)
            case v: Int ⇒ JsNumber(v)
          }

          val argsJson = c.args.raw flatMap {
            case (k, Some(v)) ⇒ Some(k → handleValue(v))
            case (k, None) ⇒ None
            case (k, v) ⇒ Some(k → handleValue(v))
          }

          correctValue(c.field.fieldType, JsObject(argsJson).compactPrint)
        }
      },

      "resolvePromiseString" → { (dir, _) ⇒
        val template = directiveStringArg(dir, "value")

        c ⇒ Future {
          Thread.sleep((math.random * 50).toLong)
          correctValue(c.field.fieldType, replacePlaceholders(template, c.args))
        }
      },

      "resolveEmptyObject" → { (dir, _) ⇒
        c ⇒ correctValue(c.field.fieldType, JsObject.empty)
      },

      "resolveTestData" → { (dir, _) ⇒
        val name = directiveStringArg(dir, "name")

        c ⇒ correctValue(c.field.fieldType, testData(name))
      },

      "resolvePromiseTestData" → { (dir, _) ⇒
        val name = directiveStringArg(dir, "name")

        c ⇒ Future {
          Thread.sleep((math.random * 50).toLong)
          correctValue(c.field.fieldType, testData(name))
        }
      },

      "resolvePromise" → { (dir, definition) ⇒ c ⇒ Future {
        Thread.sleep((math.random * 50).toLong)
        extractCorrectValue(c.field.fieldType, c.value.asInstanceOf[JsValue].get(definition.name), testData)
      }},

      "resolveError" → ((dir, _) ⇒ c ⇒
        throw ResolveError(directiveStringArg(dir, "message"))),

      "resolvePromiseReject" → ((dir, _) ⇒ c ⇒
        Future.failed[Any](ResolveError(directiveStringArg(dir, "message")))),

      "resolveErrorList" → {(dir, _) ⇒
        val values = directiveStringListArg(dir, "values")
        val errors = directiveStringListArg(dir, "messages") map ResolveError.apply

        c ⇒ PartialValue(correctValue(c.field.fieldType, values), errors.toVector)
      },

      "resolvePromiseRejectList" → {(dir, _) ⇒
        val values = directiveStringListArg(dir, "values")
        val errors = directiveStringListArg(dir, "messages") map ResolveError.apply

        c ⇒ PartialFutureValue(Future.successful(PartialValue[Any, Any](correctValue(c.field.fieldType, values), errors.toVector)))
      })

    override def resolveField(typeDefinition: ast.TypeDefinition, definition: FieldDefinition) =
      definition.directives.find(d ⇒ directiveMapping contains d.name) match {
        case Some(dir) ⇒
          directiveMapping(dir.name)(dir, definition)
        case None ⇒
          c ⇒ extractCorrectValue(c.field.fieldType, c.value.asInstanceOf[JsValue].get(definition.name), testData)
      }

    override def objectTypeInstanceCheck(definition: ObjectTypeDefinition, extensions: List[ast.TypeExtensionDefinition]) =
      Some((value, _) ⇒ value.asInstanceOf[JsValue].get("type").exists(_.stringValue == definition.name))
  }

  def getSchema(value: Option[YamlValue], path: String): Option[ast.Document] =
    value flatMap (getSchema(_, path))

  def getSchema(value: YamlValue, path: String): Option[ast.Document] =
    value.get("schema")
      .map { v ⇒
        import sangria.parser.DeliveryScheme.Throw

        QueryParser.parse(v.stringValue)
      }
      .orElse(value.get("schema-file").map(f ⇒ FileUtil.loadSchema(path + "/" + f.stringValue)))

  def getTestData(value: Option[YamlValue], path: String) =
    value
      .flatMap(_.get("test-data") map convertToJson)
      .orElse(
        value.flatMap(_.get("test-data-file")).map(f ⇒ FileUtil.loadTestData(path + "/" + f.stringValue) match {
          case Right(json) ⇒ json
          case Left(yaml) ⇒ convertToJson(yaml)
        }))

  def getAction(value: YamlValue, testName: String, testData: JsValue): Action = {
    val when = value("when")

    when.get("validate")
      .map(v ⇒ Validate(v.arrayValue.toList.map(name ⇒ QueryValidator.allRules.find(_.getClass.getSimpleName == name.stringValue) getOrElse (throw new IllegalStateException(s"Can't find the validation rule: $name")))))
      .orElse {
        when.get("execute").map { e ⇒
          val validate = e.get("validate-query").map(_.booleanValue) getOrElse true
          val value = e.get("test-value").map(name ⇒ testData(name.stringValue)) getOrElse JsNull
          val variables = e.get("variables") map convertToJson getOrElse JsObject.empty
          val operationName = e.get("operation-name") map (_.stringValue)

          Execute(validate, value, variables, operationName)
        }
      }
      .orElse {
        when.get("parse").map(_ ⇒ Parse)
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
      .orElse(value.get("exception").map(v ⇒ ExceptionContain(Left(v.stringValue))))
      .orElse(value.get("exception-regex").map(v ⇒ ExceptionContain(Right(v.stringValue.r.pattern))))
      .orElse(value.get("data").map(v ⇒ Data(convertToJson(v))))
      .orElse(value.get("syntax-error").map(_ ⇒ SyntaxError))
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

  def getGiven(value: YamlValue, schema: Option[Schema[Any, Any]]) = {
    val given = value("given")
    val query = given("query").stringValue

    Given(query, schema)
  }

  val ExceptionHandler: Executor.ExceptionHandler = {
    case (m, e: ResolveError) ⇒ HandledException(e.getMessage)
  }

  def executeAction(given: Given[Any, Any], action: Action) = action match {
    case Parse ⇒
      import sangria.parser.DeliveryScheme.Either

      ParsingResult(QueryParser.parse(given.query).left.map(_.asInstanceOf[SyntaxError]))
    case Validate(rules) ⇒
      import sangria.parser.DeliveryScheme.Throw

      ValidationResult(new RuleBasedQueryValidator(rules.toList).validateQuery(given.schema, QueryParser.parse(given.query)))
    case Execute(validate, value, vars, op) ⇒
      import sangria.parser.DeliveryScheme.Throw

      val validator = if (validate) QueryValidator.default else QueryValidator.empty

      ExecutionResult(Try(Executor.execute(given.schema, QueryParser.parse(given.query),
        root = value,
        queryValidator = validator,
        variables = vars,
        operationName = op,
        exceptionHandler = ExceptionHandler).await))
    case a ⇒
      throw new IllegalStateException(s"Not yet supported action: $a")
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

  def assetLocations(error: JsValue, locations: List[ErrorLocation]) = {
    val actualLocs = error.get("locations") map (_.arrayValue) getOrElse Vector.empty

    withClue(s"Violation does not have all positions: ${error("message").stringValue}") {
      actualLocs should have size locations.size
    }

    actualLocs.zipWithIndex foreach { case (pos, idx) ⇒
      withClue(s"Violation position mismatch (line: ${locations(idx).line}, column: ${locations(idx).column}): ${error("message").stringValue}") {
        ErrorLocation(pos("line").intValue, pos("column").intValue) should be(locations(idx))
      }
    }
  }

  def assertResult(result: Result, assertion: Assertion) = (result, assertion) match {
    case (ValidationResult(violations), Passes) ⇒
      violations should have size 0
    case (ParsingResult(res), Passes) ⇒
      withClue("Parsing result was not successful - query contains some syntax errors.") {
        res.isRight should be (true)
      }
    case (ParsingResult(res), SyntaxError) ⇒
      withClue("Parsing result was successful and does not contain syntax errors.") {
        res.isLeft should be (true)
      }
    case (ValidationResult(violations), ErrorsCount(count)) ⇒
      violations should have size count
    case (ExecutionResult(value), ErrorsCount(count)) ⇒
      value.get.get("errors").map(_.arrayValue).getOrElse(Vector.empty) should have size count
    case (ValidationResult(violations), ErrorsContain(message, locations)) ⇒
      message match {
        case Left(text) ⇒
          val v = withClue(s"Can't find error message: $text") {
            val v = violations.find(_.errorMessage.contains(text))

            withClue(s"Actual violations:${violations map (v ⇒ "  * " + v.errorMessage) mkString  ("\n", "\n", "\n")}") {
              v should not be 'empty
            }

            v
          }

          assetLocations(v.get, locations)
        case Right(pattern) ⇒
          val v = withClue(s"Can't find error pattern: $pattern") {
            val v = violations.find(v ⇒ pattern.matcher(v.errorMessage).matches)

            withClue(s"Actual violations:${violations map (v ⇒ "  * " + v.errorMessage) mkString  ("\n", "\n", "\n")}") {
              v should not be 'empty
            }
            v
          }

          assetLocations(v.get, locations)
      }

    case (ExecutionResult(res), ExceptionContain(message)) ⇒
      res match {
        case Failure(error) ⇒
          message match {
            case Left(text) ⇒ error.getMessage should include (text)
            case Right(pattern) ⇒
              withClue(s"Message '${error.getMessage}' does not match the pattern: $pattern") {
                pattern.matcher(error.getMessage).matches should be ("true")
              }
          }
        case Success(res) ⇒
          fail("Execution was successful: " + res)
      }

    case (ExecutionResult(value), ErrorsContain(message, locations)) ⇒
      val errors = value.get.get("errors") map (_.arrayValue) getOrElse Vector.empty

      message match {
        case Left(text) ⇒
          val v = withClue(s"Can't find error message: $text") {
            val v = errors.find(_("message").stringValue.contains(text))

            v should not be ('empty)
            v
          }

          assetLocations(v.get, locations)
        case Right(pattern) ⇒
          val v = withClue(s"Can't find error pattern: $pattern") {
            val v = errors.find(v ⇒ pattern.matcher(v("message").stringValue).matches)

            v should not be ('empty)
            v
          }

          assetLocations(v.get, locations)
      }

    case (ExecutionResult(actual), Data(expected)) ⇒
      withClue("Result: " + actual) {
        actual.get("data") should be (expected)
      }

    case a ⇒ throw new IllegalStateException(s"Not yet supported assertion: $a")
  }

  def generateTests(path: String) = {
    FileUtil.loadScenarios(path) foreach { file ⇒
      val scenario = file.scenario

      scenario("scenario").stringValue should {
        val bgTestData = getTestData(scenario.get("background"), file.folder)
        val bgBuilder = schemaBuilder(bgTestData getOrElse JsObject.empty)
        val bgSchema = getSchema(scenario.get("background"), file.folder) map (Schema.buildFromAst(_, bgBuilder))

        scenario("tests").arrayValue foreach { test ⇒
          val testName = test("name").stringValue

          testName in {
            val testTestData = getTestData(test.get("given"), file.folder)
            val testBuilder = testTestData map schemaBuilder getOrElse bgBuilder
            val testSchema =
              getSchema(test.get("given"), file.folder) map (Schema.buildFromAst(_, testBuilder)) orElse {
                testTestData match {
                  case Some(newTestData) ⇒ getSchema(scenario.get("given"), file.folder) map (Schema.buildFromAst(_, testBuilder))
                  case None ⇒ bgSchema
                }
              }

            val testData = testTestData orElse bgTestData getOrElse JsObject.empty

            val given = getGiven(test, testSchema)
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

  case class Given[Ctx, Val](query: String, schemaOpt: Option[Schema[Ctx, Val]]) {
    def schema = schemaOpt getOrElse (throw new IllegalStateException("No schema provided!"))
  }

  sealed trait Action

  case object Parse extends Action
  case class Validate(rules: List[ValidationRule]) extends Action
  case class Execute(validate: Boolean, value: JsValue, variables: JsValue, operationName: Option[String]) extends Action

  sealed trait Result

  case class ValidationResult(violations: Vector[Violation]) extends Result
  case class ExecutionResult(value: Try[JsValue]) extends Result
  case class ParsingResult(document: Either[SyntaxError, ast.Document]) extends Result

  sealed trait Assertion

  case object Passes extends Assertion
  case object SyntaxError extends Assertion
  case class Data(json: JsValue) extends Assertion
  case class ErrorsCount(count: Int) extends Assertion
  case class ErrorsContain(message: Either[String, Pattern], locations: List[ErrorLocation]) extends Assertion
  case class ExceptionContain(message: Either[String, Pattern]) extends Assertion

  case class ErrorLocation(line: Int, column: Int)
}