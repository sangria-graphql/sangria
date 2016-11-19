package sangria.util

import java.io.File

import io.github.lukehutch.fastclasspathscanner.FastClasspathScanner
import io.github.lukehutch.fastclasspathscanner.matchprocessor.FileMatchContentsProcessorWithContext
import sangria.parser.QueryParser
import sangria.parser.DeliveryScheme.Throw
import spray.json._

import scala.collection.immutable.VectorBuilder
import scala.io.Source

import net.jcazevedo.moultingyaml._


object FileUtil {
  def loadQuery(name: String) =
    loadResource("queries/" + name)

  def loadYaml(name: String, root: String = "scenarios") =
    loadResource(root + "/" + name).parseYaml

  def loadScenarios(path: String, root: String = "scenarios") = this.synchronized {
    val builder = new VectorBuilder[ScenarioFile]

    new FastClasspathScanner(root + "." + path).matchFilenameExtension("yaml", new FileMatchContentsProcessorWithContext {
      override def processMatch(file: File, relativePath: String, fileContents: Array[Byte]) = {
        builder += ScenarioFile(file.getName, relativePath, new String(fileContents, "UTF-8").parseYaml)
      }
    }).scan()

    builder.result()
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
      case Some(res) ⇒ Source.fromInputStream(res, "UTF-8").mkString
      case None ⇒ throw new IllegalArgumentException("Resource not found: /" + path)
    }

  case class ScenarioFile(fileName: String, path: String, scenario: YamlValue) {
    def folder = path.substring(0, path.lastIndexOf("/"))
  }
}
