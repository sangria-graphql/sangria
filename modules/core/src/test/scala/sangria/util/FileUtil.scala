package sangria.util

import java.io.File

import io.github.classgraph.ClassGraph
import sangria.parser.QueryParser
import sangria.parser.DeliveryScheme.Throw
import spray.json._

import scala.io.Source
import net.jcazevedo.moultingyaml._

import scala.collection.JavaConverters._


object FileUtil extends StringMatchers {
  def loadQuery(name: String) =
    loadResource("queries/" + name)

  def loadYaml(name: String, root: String = "scenarios") =
    loadResource(root + "/" + name).parseYaml

  def loadScenarios(path: String, root: String = "scenarios") = this.synchronized {
    val yamlResources = new ClassGraph()
      .whitelistPackages(root + "." + path)
      .scan()
      .getResourcesWithExtension("yaml")
      .asScala
      .groupBy(_.getPath).mapValues(_.head) // deduplicate (`ClassGraph` gives duplicates for some reason)
      .values
      .toVector

    yamlResources.map { resource =>
      val name = resource.getPath.substring(resource.getPath.lastIndexOf("/") + 1)
      val relativePath = resource.getPathRelativeToClasspathElement
      val stream = this.getClass.getResourceAsStream("/" + relativePath)
      val contents = Source.fromInputStream(stream, "UTF-8").mkString.parseYaml

      ScenarioFile(name, relativePath, contents)
    }
  }

  def loadSchema(path: String) =
    QueryParser.parse(loadResource(path))

  def loadTestData(path: String): Either[YamlValue, JsValue] = {
    val text = loadResource(path)

    if (path endsWith ".yaml") Left(text.parseYaml)
    else if (path endsWith ".json") Right(text.parseJson)
    else throw new IllegalArgumentException(s"Unsupported file format for test data '$path'. Only `*.json` and `*.yaml` files are supported.")
  }

  def loadResource(path: String) =
    Option(this.getClass.getResourceAsStream("/" + path)) match {
      case Some(res) => stripCarriageReturns(Source.fromInputStream(res, "UTF-8").mkString)
      case None => throw new IllegalArgumentException("Resource not found: /" + path)
    }

  case class ScenarioFile(fileName: String, path: String, scenario: YamlValue) {
    def folder = path.substring(0, path.lastIndexOf("/"))
  }
}
